unit Tawhiri;

interface

uses DateUtils, SysUtils, Classes, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, Math;


type TPredictionParameters = record
    Valid:          Boolean;
    Updated:        Boolean;
    Latitude:       Double;
    Longitude:      Double;
    Altitude:       Double;
    BurstAltitude:  Double;
    AscentRate:     Double;
    DescentRate:    Double;
end;

type TPredictedPosition = record
    Latitude:       Double;
    Longitude:      Double;
    Altitude:       Double;
end;

type TPredictedPath = record
    Path:           Array[1..500] of TPredictedPosition;
    Count:          Integer;
end;

type TLandingPath = record
    Valid:          Boolean;
    LastPredAt:     TDateTime;
    Updated:        Boolean;
    Latitude:       Double;
    Longitude:      Double;
    Altitude:       Double;
    Path:           TPredictedPath;
end;

type TPayloadPrediction = record
    PayloadID:          String;
    Parameters:         TPredictionParameters;
    LandingPath:        TLandingPath;
end;

type TPayloadPredictions = record
    PayloadPredictions:     Array[1..100] of TPayloadPrediction;
    Count:                  Integer;
end;

type
  TTawhiri = class(TThread)
  private
    { Private declarations }
    PayloadPredictions: TPayloadPredictions;
    function ProcessResponse(PayloadIndex: Integer; Response: String): Boolean;
    function GetURL(URL: String): String;
    procedure Delay(ms: Integer);
  protected
    { Protected declarations }
    procedure Execute; override;
  public
    { Public declarations }
    function AddPayload(PayloadID: String): Integer;
    procedure UpdatePayload(PayloadIndex: Integer; Latitude, Longitude, Altitude, BurstAltitude, AscentRate, DescentRate: Double);
    procedure GetPrediction(PayloadIndex: Integer; var Latitude: Double; var Longitude: Double; var Altitude: Double);
    function PredictionUpdated(PayloadIndex: Integer): Boolean;
    function FindNextNewPrediction: Integer;
    procedure RemovePayload(PayloadIndex: Integer);
    function GetPayloadPath(PayloadIndex, Index: Integer; var Latitude: Double; var Longitude: Double; var Altitude: Double): Boolean;
  end;

implementation

uses Miscellaneous;

function TTawhiri.PredictionUpdated(PayloadIndex: Integer): Boolean;
begin
    Result := PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Valid and
              PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Updated;
end;

function TTawhiri.FindNextNewPrediction: Integer;
var
    i: Integer;
begin
    for i := 1 to PayloadPredictions.Count do begin
        if PayloadPredictions.PayloadPredictions[i].LandingPath.Valid and
           PayloadPredictions.PayloadPredictions[i].LandingPath.Updated then begin
            Result := i;
            Exit;
        end;
    end;

    Result := -1;
end;

function TTawhiri.AddPayload(PayloadID: String): Integer;
var
    i: Integer;
begin
    // Already known?
    for i := 1 to PayloadPredictions.Count do begin
        if PayloadPredictions.PayloadPredictions[i].PayloadID = PayloadID then begin
            Result := i;
            Exit;
        end;
    end;

    // Free entry ?
    for i := 1 to PayloadPredictions.Count do begin
        if PayloadPredictions.PayloadPredictions[i].PayloadID = '' then begin
            PayloadPredictions.PayloadPredictions[i].PayloadID := PayloadID;
            Result := i;
            Exit;
        end;
    end;

    // Append
    if PayloadPredictions.Count < High(PayloadPredictions.PayloadPredictions) then begin
        Inc(PayloadPredictions.Count);
        PayloadPredictions.PayloadPredictions[PayloadPredictions.Count].PayloadID := PayloadID;
        Result := PayloadPredictions.Count;
        Exit;
    end;

    Result := -1;
end;

procedure TTawhiri.UpdatePayload(PayloadIndex: Integer; Latitude, Longitude, Altitude, BurstAltitude, AscentRate, DescentRate: Double);
begin
    PayloadPredictions.PayloadPredictions[PayloadIndex].Parameters.Valid := True;
    PayloadPredictions.PayloadPredictions[PayloadIndex].Parameters.Updated := True;
    PayloadPredictions.PayloadPredictions[PayloadIndex].Parameters.Latitude := Latitude;
    PayloadPredictions.PayloadPredictions[PayloadIndex].Parameters.Longitude := Longitude;
    PayloadPredictions.PayloadPredictions[PayloadIndex].Parameters.Altitude := Altitude;
    PayloadPredictions.PayloadPredictions[PayloadIndex].Parameters.BurstAltitude := BurstAltitude;
    PayloadPredictions.PayloadPredictions[PayloadIndex].Parameters.AscentRate := AscentRate;
    PayloadPredictions.PayloadPredictions[PayloadIndex].Parameters.DescentRate := DescentRate;
end;

procedure TTawhiri.GetPrediction(PayloadIndex: Integer; var Latitude: Double; var Longitude: Double; var Altitude: Double);
begin
    if PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Valid then begin
        Latitude := PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Latitude;
        Longitude := PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Longitude;
        Altitude := PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Altitude;

        PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Updated := False;
    end;
end;

procedure TTawhiri.RemovePayload(PayloadIndex: Integer);
begin
    PayloadPredictions.PayloadPredictions[PayloadIndex].PayloadID := '';
    PayloadPredictions.PayloadPredictions[PayloadIndex].Parameters.Valid := False;
    PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Valid := False;
end;

function TTawhiri.GetURL(URL: String): String;
var
    ResponseStream: TMemoryStream;
    html: string;
    HTTP: TIdHTTP;
begin
    try
        try
            ResponseStream := TMemoryStream.Create;
            HTTP := TIdHTTP.Create(nil);
            HTTP.Request.ContentType := 'text/json; charset=utf-8';
            HTTP.Request.ContentEncoding := 'utf-8';
            HTTP.HTTPOptions := [hoForceEncodeParams];
            HTTP.Get(url, responseStream);
            with TStringStream.Create do begin
                try
                    try
                        LoadFromStream(ResponseStream);
                        Result := DataString;
                    except
                        on E : Exception do begin
                            // SendMessage('Error: '+E.Message);
                        end;
                    end;
                finally
                    Free;
                end;
            end;
        except
            //
        end;
    finally
        try
            HTTP.Disconnect;
        except
        end;

        HTTP.Free;
        ResponseStream.Free;
    end;
end;

function GetCommand(var Line: String): String;
var
    Position: Integer;
begin
    Result := '';
    Position := Pos('"', Line);

    if Position > 0 then begin
        Line := Copy(Line, Position+1, 99);
        Position := Pos('": ', Line);
        if Position > 0 then begin
            Result := Copy(Line, 1, Position-1);
            Line := Copy(Line, Position+3, 99);
        end;
    end;
end;

function GetValue(Line: String): Double;
var
    Position: Integer;
begin
    Result := 0.0;
    Position := Pos(',', Line);

    if Position > 0 then begin
        Result := StrToFloat(Copy(Line, 1, Position-1));
    end else begin
        Result := StrToFloat(Line);
    end;
end;

function TTawhiri.ProcessResponse(PayloadIndex: Integer; Response: String): Boolean;
var
    Strings: TStringList;
    i: Integer;
    Line, Command: String;
    Latitude, Longitude, Altitude, Value: Double;
    Ascending, Descending: Boolean;
begin
    Result := False;

    Strings := TStringList.Create;
    Strings.Text := Response;
    Ascending := False;
    Descending := False;
    PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Path.Count := 0;

    for i := 0 to Strings.Count-1 do begin
        Line := Strings[i];

        if Ascending or Descending then begin
            Command := GetCommand(Line);
            if Command = 'altitude' then begin
                Altitude := GetValue(Line);
            end else if Command = 'latitude' then begin
                Latitude := GetValue(Line);
            end else if Command = 'longitude' then begin
                Longitude := GetValue(Line);
                if Longitude > 180 then Longitude := Longitude - 360;

                if PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Path.Count < High(PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Path.Path) then begin
                    Inc(PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Path.Count);

                    PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Path.Path[PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Path.Count].Latitude := Latitude;
                    PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Path.Path[PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Path.Count].Longitude := Longitude;
                    PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Path.Path[PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Path.Count].Altitude := Altitude;
                end;

                Result := True;
            end;
        end;

        if not Descending then begin
            if (Pos('"stage"', Strings[i]) > 0) and (Pos('"descent"', Strings[i]) > 0) then begin
                Descending := True;
            end;
        end;
        if not Ascending then begin
            if (Pos('"stage"', Strings[i]) > 0) and (Pos('"ascent"', Strings[i]) > 0) then begin
                Ascending := True;
            end;
        end;
    end;

    if Result then begin
        with PayloadPredictions.PayloadPredictions[PayloadIndex] do begin
            LandingPath.Valid := True;
            LandingPath.Updated := True;
            LandingPath.Latitude := Latitude;
            LandingPath.Longitude := Longitude;
            LandingPath.Altitude := Altitude;
            LandingPath.LastPredAt := Now;
        end;
    end;

    Strings.Free;
end;

procedure TTawhiri.Delay(ms: Integer);
var
    DelaySoFar: Integer;
begin
    DelaySoFar := 0;

    while (DelaySoFar < ms) and (not Terminated) do begin
        Sleep(100);
        Inc(DelaySoFar, 100);
    end;
end;

procedure TTawhiri.Execute;
var
    URL, Response: String;
    i: Integer;
    AscentRate, BurstAltitude, DescentRate: Double;
begin
    while not Terminated do begin
        // Get next payload to predict
        for i := 1 to PayloadPredictions.Count do begin
            with PayloadPredictions.PayloadPredictions[i] do begin
                if Parameters.Valid and Parameters.Updated and (Parameters.Altitude > 300) and (LandingPath.LastPredAt < (Now - 10/86400)) then begin
                    if Parameters.Longitude < 0 then Parameters.Longitude := Parameters.Longitude + 360;

                    // Descending?
                    if Parameters.AscentRate < -1 then begin
                        BurstAltitude := Parameters.Altitude + 1;
                        AscentRate := 10;
                        DescentRate := 5;           // Calculate
                    end else begin
                        BurstAltitude := max(Parameters.BurstAltitude, Parameters.Altitude+1);
                        if Parameters.AscentRate > 1 then begin
                            AscentRate := Parameters.AscentRate;        // Use average
                        end else begin
                            AscentRate := 5;
                        end;

                        DescentRate := 5;
                    end;

                    URL := 'http://predict.cusf.co.uk/api/v1/?launch_latitude=' + MyFormatFloat('0.00000', Parameters.Latitude) +
                           '&launch_longitude=' + MyFormatFloat('0.00000', Parameters.Longitude) +
                           '&launch_altitude=' + MyFormatFloat('0', Parameters.Altitude) +
                           '&launch_datetime=' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss%2B00:00', TTimeZone.Local.ToUniversalTime(Now)) +
                           '&ascent_rate=' + MyFormatFloat('0.0', AscentRate) +
                           '&burst_altitude=' + MyFormatFloat('0', BurstAltitude) +
                           '&descent_rate=' + MyFormatFloat('0.0', DescentRate);

                    Response := GetURL(URL);
                    try
                        if ProcessResponse(i, Response) then begin
                            Parameters.Updated := False;
                        end;
                    except
                        // SendMessage('Error in ProcessHabitatResponse');
                    end;
                end;
            end;
        end;

        Delay(1000);
    end;
end;

function TTawhiri.GetPayloadPath(PayloadIndex, Index: Integer; var Latitude: Double; var Longitude: Double; var Altitude: Double): Boolean;
begin
    if Index <= PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Path.Count then begin
        Result := True;

        Latitude := PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Path.Path[Index].Latitude;
        Longitude := PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Path.Path[Index].Longitude;
        Altitude := PayloadPredictions.PayloadPredictions[PayloadIndex].LandingPath.Path.Path[Index].Altitude;
    end else begin
        Result := False;
    end;
end;

end.

