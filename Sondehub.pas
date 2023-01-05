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
    procedure SyncCallback(SourceID: Integer; Active, OK: Boolean; Status: String);
    function UploadJson(URL, Json: String; var Response: String): Boolean;
    function UploadPosition(SourceID: Integer): Boolean;
    function UploadListener: Boolean;
  public
    PayloadList: String;
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
    if (Position.Latitude <> 0) and (Position.Longitude <> 0) then begin
        CritSondehub.Enter;
        try
            SondehubPositions[SourceID] := Position;
        finally
            CritSondehub.Leave;
        end;
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
        Result := '"' + FieldName + '": ' + MyFormatFloat('0.0000', Value) + ',';
    end else begin
        Result := '';
    end;
end;

function IntegerToString(FieldName: String; Value: Integer; HaveValue: Boolean): String;
begin
    if HaveValue then begin
        Result := '"' + FieldName + '": ' + IntToStr(Value) + ',';
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

function TSondehubThread.UploadJson(URL, Json: String; var Response: String): Boolean;
var
    JsonToSend: TStringStream;
    IdHTTP1: TIdHTTP;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
begin
    Result := False;
    Response := '';

    JsonToSend := TStringStream.Create(Json, TEncoding.UTF8);

    IdSSLIOHandlerSocketOpenSSL1 := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    with IdSSLIOHandlerSocketOpenSSL1 do begin
        SSLOptions.Method := sslvTLSv1_2;
        SSLOptions.SSLVersions := [sslvTLSv1_2];
    end;

    // Post it
    IdHTTP1 := TIdHTTP.Create(nil);
    try
        IdHTTP1.Request.UserAgent := 'HAB_BASE';
        IdHTTP1.Request.ContentType := 'application/json';
        IdHTTP1.Request.CharSet := 'utf-8';
        IdHTTP1.IOHandler := IdSSLIOHandlerSocketOpenSSL1;

        try
            IdHTTP1.Put(URL, JsonToSend);
            Response := 'OK';
            Result := True;
        except
            on E : Exception do begin
               Response := E.Message + ' - ' + Json;
            end;
        end;
    finally
        JsonToSend.Free;
        IdHTTP1.Free;
        IdSSLIOHandlerSocketOpenSSL1.Free;
    end;
end;

function TSondehubThread.UploadPosition(SourceID: Integer): Boolean;
var
    UTC: TDateTime;
    URL, Json, Response: String;
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
                '"datetime":"' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', TimeStamp) + '",' +
                '"lat": ' + MyFormatFloat('0.00000', Latitude) + ',' +
                '"lon": ' + MyFormatFloat('0.00000', Longitude) + ',' +
                '"alt": ' + MyFormatFloat('0', Altitude) + ',' +
                DoubleToString('frequency', CurrentFrequency + FrequencyError / 1000.0, CurrentFrequency > 0.0) +
                DoubleToString('temp', InternalTemperature, HaveInternalTemperature) +                  // WAS internal_temp
                DoubleToString('ext_temperature', ExternalTemperature, HaveExternalTemperature) +       // WAS temp
                DoubleToString('humidity', Humidity, HaveHumidity) +
                DoubleToString('vel_h', Speed, HaveSpeed) +
                DoubleToString('vel_v', AscentRate, HaveAscentRate) +
                DoubleToString('pressure', Pressure, HavePressure) +
                DoubleToString('heading', Direction, HasDirection) +
                DoubleToString('batt', BatteryVoltage, HasBatteryVoltage) +
                DoubleToString('pred_lat', PredictedLatitude, PredictionType <> ptNone) +
                DoubleToString('pred_lon', PredictedLongitude, PredictionType <> ptNone) +
                IntegerToString('sats', Satellites, HasSatelliteCount) +
                ValueToString('modulation', Modulation) +
                DoubleToString('snr', SNR, HasSNR) +
                DoubleToString('rssi', PacketRSSI, HasPacketRSSI) +
                '"uploader_position": [' +
                ' ' + MyFormatFloat('0.00000', ListenerLatitude) + ',' +
                ' ' + MyFormatFloat('0.00000', ListenerLongitude) + ',' +
                ' ' + MyFormatFloat('0', ListenerAltitude) +
                '],' +
                '"raw": "' + Line + '"' +
                // '"uploader_antenna": "W-30"' +
                '}]';
    end;

    Result := UploadJson(URL, Json, Response);

    SyncCallback(SourceID, True, Result, 'Telemetry Upload: ' + Response);
end;

function TSondehubThread.UploadListener: Boolean;
var
    URL, Json, Response: String;
begin
    URL := 'https://api.v2.sondehub.org/amateur/listeners';

    Json := '{' +
            '"software_name": "' + SoftwareName + '",' +
            '"software_version": "' + SoftwareVersion + '",' +
            '"uploader_callsign": "' + OurCallsign + '",' +
            BooleanToString('mobile', IsMobile) +
            '"uploader_position": [' +
            ' ' + MyFormatFloat('0.00000', ListenerLatitude) + ',' +
            ' ' + MyFormatFloat('0.00000', ListenerLongitude) + ',' +
            ' ' + MyFormatFloat('0', ListenerAltitude) +
            ']' +
//            '"uploader_radio": "' + 'LoRaGo' + '",' +
//            '"uploader_antenna": "' + 'W-30' + '",' +
//            '"uploader_contact_email":  "' + 'dave@sccs.co.uk' + '"' +
            '}';

    Result := UploadJson(URL, Json, Response);

    SyncCallback(0, True, Result, 'Listener Upload: ' + Response);
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

procedure TSondehubThread.SyncCallback(SourceID: Integer; Active, OK: Boolean; Status: String);
begin
    Synchronize(
        procedure begin
            StatusCallback(SourceID, Active, OK, Status);
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

