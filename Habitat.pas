unit Habitat;

interface

uses Classes, SysUtils, SyncObjs, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, Miscellaneous;

type
    THabitatThread = class(TThread)
private
    CritHabitat: TCriticalSection;
    OurCallsign: String;
    HabitatSentence: Array[0..32] of String;
    StatusCallback: TStatusCallback;
    procedure SyncCallback(SourceID: Integer; Active, OK: Boolean);
    procedure UploadSentence(SourceID: Integer; Telemetry: String);
  public
    procedure SaveTelemetryToHabitat(SourceID: Integer; Sentence, Callsign: String);
    procedure Execute; override;
  published
    constructor Create(Callback: TStatusCallback);
end;

implementation

procedure THabitatThread.SaveTelemetryToHabitat(SourceID: Integer; Sentence, Callsign: String);
begin
    CritHabitat.Enter;
    try
        HabitatSentence[SourceID] := Sentence;
        OurCallsign := Callsign;
    finally
        CritHabitat.Leave;
    end;
end;

procedure THabitatThread.UploadSentence(SourceID: Integer; Telemetry: String);
var
    URL, FormAction, Callsign, Temp: String;
    Params: TStringList;
    IdHTTP1: TIdHTTP;
begin
    URL := 'http://habitat.habhub.org/transition';
    FormAction := 'payload_telemetry';
    Callsign := OurCallsign;

    // Parameters
    Params := TStringList.Create;
    // Params.Add('Submit=' + FormAction);
    Params.Add('callsign=' + Callsign);
    Params.Add('string=' + Telemetry + #10);
    Params.Add('string_type=ascii');
    Params.Add('metadata={}');
    Params.Add('time_created=');

    // Post it
    IdHTTP1 := TIdHTTP.Create(nil);
    try
        try
            IdHTTP1.Request.ContentType := 'application/x-www-form-urlencoded';
            IdHTTP1.Response.KeepAlive := False;
            Temp := IdHTTP1.Post(URL + '/' + FormAction, Params);
            SyncCallback(SourceID, True, True);
        except
            SyncCallback(SourceID, True, False);
        end;
    finally
        IdHTTP1.Free;
    end;

    Params.Free;
end;

procedure THabitatThread.Execute;
var
    Packets: TStringList;
    Sentence: String;
    i, SourceID: Integer;
begin
    Packets := TStringList.Create;

    while not Terminated do begin
        // Telemetry
        Sentence := '';
        CritHabitat.Enter;
        try
            for i := Low(HabitatSentence) to High(HabitatSentence) do begin
                if Sentence = '' then begin
                    if HabitatSentence[i] <> '' then begin
                        Sentence := HabitatSentence[i];
                        HabitatSentence[i] := '';
                        SourceID := i;
                    end;
                end;
            end;
        finally
            CritHabitat.Leave;
        end;

        if Sentence <> '' then begin
            try
                UploadSentence(SourceID, Sentence);
            except
            end;
        end;

        Sleep(1000);
    end;
end;

constructor THabitatThread.Create(Callback: TStatusCallback);
begin
    CritHabitat := TCriticalSection.Create;

    StatusCallback := Callback;

    inherited Create(False);
end;

procedure THabitatThread.SyncCallback(SourceID: Integer; Active, OK: Boolean);
begin
    Synchronize(
        procedure begin
            StatusCallback(SourceID, Active, OK, '');
        end
    );
end;

end.
