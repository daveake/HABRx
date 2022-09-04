unit Sondehub;

interface

uses Classes, SysUtils, SyncObjs, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, Miscellaneous,
     IdSSL, IdSSLOpenSSL, System.DateUtils, Math;

type
    TSondehubThread = class(TThread)
private
    CritSondehub: TCriticalSection;
    OurCallsign: String;
    SondehubPositions: Array[0..32] of THABPosition;
    StatusCallback: TStatusCallback;
    ListenerLatitude, ListenerLongitude, ListenerAltitude: Double;
    NextListenerUploadAt: TDateTime;
    SoftwareName, SoftwareVersion: String;
    IsMobile: Boolean;
    ListenerUploadPeriod: Double;
    EnableListenerUpload: Boolean;
    procedure SyncCallback(SourceID: Integer; Active, OK: Boolean);
    function UploadJson(URL, Json: String): Boolean;
    function UploadPosition(SourceID: Integer): Boolean;
    function UploadListener: Boolean;
  public
    procedure SaveTelemetryToSondehub(SourceID: Integer; Position: THABPosition);
    procedure Execute; override;
    procedure SetListenerPosition(Latitude, Longitude, Altitude: Double);
    procedure SetListener(Software, Version, Callsign: String; Mobile: Boolean = False; UploadPeriod: Double = 6 * 3600; EnableUpload: Boolean = True);
  published
    constructor Create(Callback: TStatusCallback);
end;

implementation

procedure TSondehubThread.SaveTelemetryToSondehub(SourceID: Integer; Position: THABPosition);
begin
    CritSondehub.Enter;
    try
        SondehubPositions[SourceID] := Position;
    finally
        CritSondehub.Leave;
    end;
end;

function ValueToString(FieldName, Value: String): String;
begin
    if Value <> '' then begin
        Result := '"' + FieldName + '": "' + Value + '",';
    end else begin
        Result := '';
    end;
end;

function DoubleToString(FieldName: String; Value: Double; HaveValue: Boolean): String;
begin
    if HaveValue then begin
        Result := '"' + FieldName + '": ' + FormatFloat('0.0000', Value) + ',';
    end else begin
        Result := '';
    end;
end;

function BooleanToString(FieldName: String; Value: Boolean): String;
begin
    if Value then begin
        Result := '"' + FieldName + '": true,';
    end else begin
        Result := '';
    end;
end;

function TSondehubThread.UploadJson(URL, Json: String): Boolean;
var
    Response: String;
    JsonToSend: TStringStream;
    IdHTTP1: TIdHTTP;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
begin
    Result := False;

    JsonToSend := TStringStream.Create(Json, TEncoding.UTF8);

    IdSSLIOHandlerSocketOpenSSL1 := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    with IdSSLIOHandlerSocketOpenSSL1 do begin
        SSLOptions.Method := sslvTLSv1_2;
        SSLOptions.SSLVersions := [sslvTLSv1_2];
    end;

    // Post it
    IdHTTP1 := TIdHTTP.Create(nil);
    try
        IdHTTP1.Request.UserAgent := 'M0RPI_Test';
        IdHTTP1.Request.ContentType := 'application/json';
        IdHTTP1.Request.CharSet := 'utf-8';
        IdHTTP1.IOHandler := IdSSLIOHandlerSocketOpenSSL1;

        try
            Response := IdHTTP1.Put(URL, JsonToSend);
            Result := True;
        except
        end;
    finally
        JsonToSend.Free;
        IdSSLIOHandlerSocketOpenSSL1.Free;
        IdHTTP1.Free;
    end;
end;

function TSondehubThread.UploadPosition(SourceID: Integer): Boolean;
var
    UTC: TDateTime;
    URL, Json: String;
begin
    UTC := TTimeZone.Local.ToUniversalTime(Now);

    URL := 'https://api.v2.sondehub.org/amateur/telemetry';

    with SondehubPositions[SourceID] do begin
        Json := '[{' +
                // '"dev": "",' +
                '"software_name": "' + SoftwareName + '",' +
                '"software_version": "' + SoftwareVersion + '",' +
                '"uploader_callsign": "' + OurCallsign + '",' +
                '"time_received": "' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', UTC) + '",' +
                '"payload_callsign": "' + PayloadID + '",' +
                '"datetime":"' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', UTC) + '",' +
                '"lat": ' + FormatFloat('0.00000', Latitude) + ',' +
                '"lon": ' + FormatFloat('0.00000', Longitude) + ',' +
                '"alt": ' + FormatFloat('0', Altitude) + ',' +
                DoubleToString('frequency', CurrentFrequency + FrequencyError, CurrentFrequency > 0.0) +
    //            '"temp": 0,' +
    //            '"humidity": 0,' +
    //            '"vel_h": 0,' +
    //            '"vel_v": 0,' +
    //            '"pressure": 0,' +
    //            '"heading": 0,' +
    //            '"batt": 0,' +
    //            '"sats": 0,' +
                ValueToString('modulation', Modulation) +
                DoubleToString('snr', SNR, HasSNR) +
                DoubleToString('rssi', PacketRSSI, HasPacketRSSI) +
                '"uploader_position": [' +
                ' ' + FormatFloat('0.00000', ListenerLatitude) + ',' +
                ' ' + FormatFloat('0.00000', ListenerLongitude) + ',' +
                ' ' + FormatFloat('0', ListenerAltitude) +
                '],' +
                '"raw": "' + Line + '"' +
                // '"uploader_antenna": "W-30"' +
                '}]';
    end;

    Result := UploadJson(URL, Json);

    SyncCallback(SourceID, True, Result);
end;

function TSondehubThread.UploadListener: Boolean;
var
    UTC: TDateTime;
    URL, Json: String;
begin
    // GetSystemTime(UTC);
    // NowUTC := SystemTimeToDateTime(UTC);
    UTC := TTimeZone.Local.ToUniversalTime(Now);

    URL := 'https://api.v2.sondehub.org/amateur/listeners';

    Json := '{' +
            '"software_name": "' + SoftwareName + '",' +
            '"software_version": "' + SoftwareVersion + '",' +
            '"uploader_callsign": "' + OurCallsign + '",' +
            BooleanToString('mobile', IsMobile) +
            '"uploader_position": [' +
            ' ' + FormatFloat('0.00000', ListenerLatitude) + ',' +
            ' ' + FormatFloat('0.00000', ListenerLongitude) + ',' +
            ' ' + FormatFloat('0', ListenerAltitude) +
            ']' +
//            '"uploader_radio": "' + 'LoRaGo' + '",' +
//            '"uploader_antenna": "' + 'W-30' + '",' +
//            '"uploader_contact_email":  "' + 'dave@sccs.co.uk' + '"' +
            '}';

    Result := UploadJson(URL, Json);

    // SyncCallback(SourceID, True, Result);
end;

procedure TSondehubThread.Execute;
var
    Packets: TStringList;
    i, SourceID: Integer;
begin
    Packets := TStringList.Create;

    while not Terminated do begin
        // Telemetry
        SourceID := -1;
        CritSondehub.Enter;
        try
            for i := Low(SondehubPositions) to High(SondehubPositions) do begin
                if SourceID < 0 then begin
                    if SondehubPositions[i].InUse then begin
                        SondehubPositions[i].InUse := False;
                        SourceID := i;
                    end;
                end;
            end;
        finally
            CritSondehub.Leave;
        end;

        if SourceID >= Low(SondeHubPositions) then begin
            try
                UploadPosition(SourceID);
            except
            end;
        end;

        if EnableListenerUpload and (Now >= NextListenerUploadAt) and ((ListenerLatitude <> 0) or (ListenerLongitude <> 0)) then begin
            try
                UploadListener;
                NextListenerUploadAt := Now + ListenerUploadPeriod / 86400;
            except
                NextListenerUploadAt := Now + Min(ListenerUploadPeriod, 60) / 86400;
            end;
        end;

        Sleep(1000);
    end;
end;

constructor TSondehubThread.Create(Callback: TStatusCallback);
begin
    CritSondehub := TCriticalSection.Create;

    StatusCallback := Callback;

    ListenerUploadPeriod := 6 * 3600;   // default 6 hours

    inherited Create(False);
end;

procedure TSondehubThread.SyncCallback(SourceID: Integer; Active, OK: Boolean);
begin
    Synchronize(
        procedure begin
            StatusCallback(SourceID, Active, OK);
        end
    );
end;

procedure TSondehubThread.SetListenerPosition(Latitude, Longitude, Altitude: Double);
begin
    ListenerLatitude := Latitude;
    ListenerLongitude := Longitude;
    ListenerAltitude := Altitude;
end;

procedure TSondehubThread.SetListener(Software, Version, Callsign: String; Mobile: Boolean = False; UploadPeriod: Double = 6 * 3600; EnableUpload: Boolean = True);
begin
    SoftwareName := Software;
    SoftwareVersion := Version;
    OurCallsign := Callsign;
    IsMobile := Mobile;
    ListenerUploadPeriod := UploadPeriod;
    EnableListenerUpload := EnableUpload;
end;

end.
