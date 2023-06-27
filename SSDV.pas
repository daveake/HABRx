unit SSDV;

interface

uses Classes, SysUtils, SyncObjs, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, Miscellaneous;

type
    TSSDVThread = class(TThread)
private
    SSDVPackets: TStringList;
    CritSSDV: TCriticalSection;
    OurCallsign: String;
    StatusCallback: TStatusCallback;
    procedure UploadSSDV(Packets: TStringList);
    procedure SyncCallback(SourceID: Integer; Active, OK: Boolean);
  public
    procedure SaveSSDVToHabitat(Line, Callsign: String);
    // procedure SaveSSDVToHabitat(Packet: String);
    procedure Execute; override;
  published
    constructor Create(Callback: TStatusCallback);
end;

implementation

procedure TSSDVThread.SaveSSDVToHabitat(Line, Callsign: String);
begin
    CritSSDV.Enter;
    try
        OurCallsign := Callsign;
        SSDVPackets.Add(Line);
    finally
        CritSSDV.Leave;
    end;
end;


procedure TSSDVThread.UploadSSDV(Packets: TStringList);
var
    URL, FormAction, Callsign, Temp, json: String;
    JsonToSend: TStringStream;
    i: Integer;
    IdHTTP1: TIdHTTP;
    SentOK: Boolean;
begin
    URL := 'http://ssdv.habhub.org/api/v0';
    FormAction := 'packets';
    Callsign := OurCallsign;

    // Create json with the base64 data in hex, the tracker callsign and the current timestamp
    json :=
            '{' +
                '"type": "packets",' +
                '"packets":[';

    for i := 0 to Packets.Count-1 do begin
        if i > 0 then json := json + ',';

        json := json +
                     '{' +
                        '"type": "packet",' +
                        '"packet":' + '"' + Packets[i] + '",' +
                        '"encoding": "hex",' +
                        '"received": "' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', Now) + '",' +
                        '"receiver": "' + Callsign + '"' +
                     '}';
    end;

    json := json + ']}';

    // Need the JSON as a stream
    JsonToSend := TStringStream.Create(Json, TEncoding.UTF8);

    // Post it
    try
        SentOK := False;
        IdHTTP1 := TIdHTTP.Create(nil);
        IdHTTP1.Request.ContentType := 'application/json';
        IdHTTP1.Request.ContentEncoding := 'UTF-8';
        IdHTTP1.Response.KeepAlive := False;
        Temp := IdHTTP1.Post(URL + '/' + FormAction, JsonToSend);
        SentOK := True;
    finally
        IdHTTP1.Free;
        JsonToSend.Free;
        SyncCallback(0, True, SentOK);
    end;
end;


procedure TSSDVThread.Execute;
var
    Packets: TStringList;
    Sentence: String;
    UploadedSomething: Boolean;
    i, SourceID: Integer;
begin
    Packets := TStringList.Create;

    while not Terminated do begin
        UploadedSomething := False;

        // SSDV
        Packets.Clear;
        CritSSDV.Enter;
        try
            if SSDVPackets.Count > 0 then begin
                Packets.Assign(SSDVPackets);
                SSDVPackets.Clear;
            end;
        finally
            CritSSDV.Leave;
        end;

        if Packets.Count > 0 then begin
            UploadSSDV(Packets);
            UploadedSomething := True;
        end;

        // So we don't gobble up the CPU
        if not UploadedSomething then begin
            sleep(100);
        end;
    end;

    Packets.Free;
end;

constructor TSSDVThread.Create(Callback: TStatusCallback);
begin
    CritSSDV := TCriticalSection.Create;
    SSDVPackets := TStringList.Create;

    StatusCallback := Callback;

    inherited Create(False);
end;

procedure TSSDVThread.SyncCallback(SourceID: Integer; Active, OK: Boolean);
begin
    if Addr(StatusCallback) <> nil then begin
        Synchronize(
            procedure begin
                StatusCallback(SourceID, Active, OK, '');
            end
        );
    end;
end;

end.
