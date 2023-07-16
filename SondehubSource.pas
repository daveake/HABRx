unit SondehubSource;

interface

uses Source, DateUtils, SysUtils, Classes, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
     IdSSL, IdSSLOpenSSL, System.JSON;

type
  TSondehubSource = class(TSource)
  private
    { Private declarations }
    function ProcessResponse(Response: String): Boolean;
    function GetURL(URL: String; var Response: String): Boolean;
    procedure Delay(ms: Integer);
  protected
    { Protected declarations }
    procedure Execute; override;
  public
    { Public declarations }
    Latitude, Longitude, Radius: Double;
  public
    constructor Create(ID: Integer; Group: String; Callback: TSourcePositionCallback);
  end;

implementation

uses Miscellaneous;

constructor TSondehubSource.Create(ID: Integer; Group: String; Callback: TSourcePositionCallback);
begin
    inherited Create(ID, Group, Callback);
end;

function TSondehubSource.GetURL(URL: String; var Response: String): Boolean;
var
    ResponseStream: TMemoryStream;
    html: string;
    HTTP: TIdHTTP;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
begin
    Result := False;

    try
        try
            ResponseStream := TMemoryStream.Create;

            IdSSLIOHandlerSocketOpenSSL1 := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
            with IdSSLIOHandlerSocketOpenSSL1 do begin
                SSLOptions.Method := sslvTLSv1_2;
                SSLOptions.SSLVersions := [sslvTLSv1_2];
            end;

            HTTP := TIdHTTP.Create(nil);
            HTTP.IOHandler := IdSSLIOHandlerSocketOpenSSL1;

            HTTP.Get(url, responseStream);

            with TStringStream.Create do begin
                try
                    try
                        LoadFromStream(ResponseStream);
                        Response := DataString;
                        Result := True;
                    except
                        on E : Exception do begin
                            SendMessage('Error: ' + E.Message);
                        end;
                    end;
                finally
                    Free;
                end;
            end;
        except
            on E : Exception do begin
                SendMessage('Error: ' + E.Message);
            end;
        end;
    finally
        try
            // HTTP.Disconnect;
        except
        end;

        HTTP.Free;
        IdSSLIOHandlerSocketOpenSSL1.Free;
        ResponseStream.Free;
    end;
end;

function TSondehubSource.ProcessResponse(Response: String): Boolean;
var
    Position: THABPosition;
    TimeStamp: String;
    JSONObject: TJSONObject;
    JSONPair: TJSONPair;
    PayloadCount: Integer;
begin
    Result := False;

    PayloadCount := 0;

    JSONObject := TJSONObject(TJSONObject.ParseJSONValue(Response));

    try
        for JSONPair in JSONObject do begin
            Position := default(THABPosition);

            try
                Position.PayloadID := JSONPair.JSONValue.FindValue('payload_callsign').Value;

                TimeStamp := JSONPair.JSONValue.FindValue('datetime').Value;
                if Length(TimeStamp) >= 19 then begin
                    Position.TimeStamp := EncodeDateTime(StrToIntDef(Copy(TimeStamp, 1, 4), 0),
                                                         StrToIntDef(Copy(TimeStamp, 6, 2), 0),
                                                         StrToIntDef(Copy(TimeStamp, 9, 2), 0),
                                                         StrToIntDef(Copy(TimeStamp, 12, 2), 0),
                                                         StrToIntDef(Copy(TimeStamp, 15, 2), 0),
                                                         StrToIntDef(Copy(TimeStamp, 18, 2), 0),
                                                         0);
                    Position.Latitude := MyStrToFloat(JSONPair.JSONValue.FindValue('lat').Value);
                    Position.Longitude := MyStrToFloat(JSONPair.JSONValue.FindValue('lon').Value);
                    if JSONPair.JSONValue.FindValue('alt') <> nil then begin
                        Position.Altitude := MyStrToFloat(JSONPair.JSONValue.FindValue('alt').Value);
                    end;

                    if JSONPair.JSONValue.FindValue('raw') <> nil then begin
                        Position.Line := JSONPair.JSONValue.FindValue('raw').Value;
                    end;

                    Position.ReceivedAt := Now;
                    Position.Line := Position.PayloadID + ',' + TimeStamp + ',' + MyFormatFloat('0.00000', Position.Latitude) + ',' + MyFormatFloat('0.00000', Position.Longitude) + ',' + MyFormatFloat('0', Position.Altitude);
                    Position.InUse := True;
                    SyncCallback(SourceID, True,  '', Position);
                    Inc(PayloadCount);
                    Result := True;
                end;
            except
                Position.InUse := False;
                Position.Line := 'Parsing Error';
                SyncCallback(SourceID, True, '', Position);
            end;
        end;
    finally
        JSONObject.Free;
    end;
end;

procedure TSondehubSource.Delay(ms: Integer);
var
    DelaySoFar: Integer;
begin
    DelaySoFar := 0;

    while (DelaySoFar < ms) and (not Terminated) do begin
        Sleep(100);
        Inc(DelaySoFar, 100);
    end;
end;

procedure TSondehubSource.Execute;
var
    PayloadID, Response, URL: String;
    Last: Integer;
    Position: THABPosition;
begin
    Position := default(THABPosition);
    Last := 3 * 60 * 60;

    while not Terminated do begin
        if GetSettingBoolean(GroupName, 'Enabled', False) then begin

            if ((Latitude = 0) and (Longitude = 0)) or (Radius = 0) then begin
                SendMessage('No listener position/radius configured');
            end else begin
                URL := 'https://api.v2.sondehub.org/amateur?lat='  + MyFormatFloat('0.00000', Latitude) +
                                                          '&lon=' + MyFormatFloat('0.00000', Longitude) +
                                                          '&distance=' + MyFormatFloat('0', Radius * 1000) +
                                                          '&last=' + IntToStr(Last);

                if GetURL(URL, Response) then begin
                    try
                        if not ProcessResponse(Response) then begin
                            SendMessage('No payloads within range');
                        end;
                        Last := 90;
                    except
                        SendMessage('Error in ProcessResponse');
                    end;
                end else begin
                    SyncCallback(SourceID, True, '', Position);
                    SyncCallback(SourceID, False, '', Position);
                end;

                Delay(9000);
            end;
        end else begin
            SendMessage('Sondehub Download Disabled');
            Last := 3 * 60 * 60;
        end;

        Delay(1000);
    end;
end;


end.

