unit Miscellaneous;

interface

uses DateUtils, Math, INIFiles,
{$IFDEF ANDROID}
     Androidapi.JNI.Interfaces.JGeomagneticField,
{$ENDIF}
     Generics.Collections, System.IOUtils;

type TFlightMode = (fmIdle, fmLaunched, fmDescending, fmLanded);

type
  TPredictionType = (ptNone, ptOnboard, ptTawhiri);

type
  THABPosition = record
    InUse:          Boolean;
    IsChase:        Boolean;
    IsSSDV:         Boolean;
    Repeated:       Boolean;
    IsSonde:        Boolean;
    FailedCRC:      Boolean;
    // ShowMessage:    Boolean;
    Channel:        Integer;
    PayloadID:      String;
    Counter:        Integer;
    TimeStamp:      TDateTime;
    Latitude:       Double;
    Longitude:      Double;
    Altitude:       Double;

    Satellites:         Integer;
    HasSatelliteCount:  Boolean;

    AscentRate:     Double;
    HaveAscentRate: Boolean;
    FlightMode:     TFlightMode;
    ReceivedAt:     TDateTime;
    Line:           String;

    InternalTemperature:        Double;
    HaveInternalTemperature:    Boolean;
    ExternalTemperature:        Double;
    HaveExternalTemperature:    Boolean;

    Pressure:                   Double;
    HavePressure:               Boolean;
    Humidity:                   Double;
    HaveHumidity:               Boolean;

    Speed:                      Double;
    HaveSpeed:                  Boolean;

    Heading:                    Double;
    HaveHeading:                Boolean;

    BatteryVoltage:             Double;
    HasBatteryVoltage:          Boolean;

    BatteryCurrent:             Double;
    HasBatteryCurrent:          Boolean;

    PredictionType:             TPredictionType;
    PredictedLatitude:          Double;
    PredictedLongitude:         Double;

    CutdownStatus:              Integer;
    HaveCutdownStatus:          Boolean;

    UplinkSNR:                  Integer;
    HaveUplinkSNR:              Boolean;

    UplinkRSSI:                 Integer;
    HaveUplinkRSSI:             Boolean;

    UplinkCount:                Integer;
    HaveUplinkCount:            Boolean;

    LastCommand:                String;

    // Packet signal information from the receiver
    SNR:                        Integer;
    HasSNR:                     Boolean;

    PacketRSSI:                 Integer;
    HasPacketRSSI:              Boolean;

    // Current Signal information from the receiver
    CurrentRSSI:                Integer;
    HasCurrentRSSI:             Boolean;

    CDA:                        Double;
    LandingSpeed:               Double;
    HaveLandingSpeed:           Boolean;
    TTL:                        Integer;
    HaveTTL:                    Boolean;

    MaxAltitude:                Double;
    HaveMaxAltitude:            Boolean;

    Altitude2:                  Double;
    HaveAltitude2:              Boolean;


    // Frequency error
    FrequencyError:     Double;
    HasFrequency:       Boolean;

    CurrentFrequency:   Double;

    // Meta
    ReceivedRemotely:   Boolean;
    Modulation:         String;

    // Calculated Values
    PayloadDocID:           String;
    Distance:               Double;

    Direction:              Double;
    HasDirection:           Boolean;

    Elevation:              Double;
    PredictionDistance:     Double;
    PredictionDirection:    Double;
    DirectionValid:         Boolean;
    UsingCompass:           Boolean;

    // APRS Symbol
    Symbol:                 Char;

    FieldList:              String;
    // Field Indices
    // SatelliteFieldIndex:    Integer;

    // Transmission (uplink)
    Transmitting:           Boolean;

    // About device
    Device:                 String;
    Version:                String;

    // SSDV Info
    SSDVFileNumber:         Integer;
    SSDVPacketNumber:       Integer;

    // Calling Mode
    CallingModeMessage:     Boolean;
    CallingModeFrequency:   Double;
    CallingModeImplicit:    Integer;
    CallingModeError:       Integer;
    CallingModeBandwidth:   Integer;
    CallingModeSpreading:   Integer;
    CallingModeOptimize:    Integer;

    // Externally maintained
    TelemetryCount:         Integer;
    SSDVCount:              Integer;
    DescentTime:            TDateTime;
  end;

type
  TStatusCallback = procedure(SourceID: Integer; Active, OK: Boolean; Status: String) of object;

type
  TSettings = TDictionary<String, Variant>;

type
  TIPLookup = record
    HostName:   String;
    IPAddress:  String;
  end;

type
  TIPLookups = record
    IPLookups:  Array[1..8] of TIPLookup;
    Count:      Integer;
  end;

function CalculateDistance(HABLatitude, HabLongitude, CarLatitude, CarLongitude: Double): Double;
function CalculateDirection(HABLatitude, HabLongitude, CarLatitude, CarLongitude: Double): Double;
function CalculateElevation(lat1, lon1, alt1, lat2, lon2, alt2: Double): Double;
function GetJSONString(Line: String; FieldName: String; StringChar: String = '"'): String;
function GetJSONInteger(Line: String; FieldName: String; StringChar: String = '"'): LongInt;
function GetJSONFloat(Line: String; FieldName: String; StringChar: String = '"'): Double;
function GetUDPString(Line: String; FieldName: String): String;
procedure InsertDate(var TimeStamp: TDateTime);
function CalculateDescentTime(Altitude, DescentRate, Land: Double): Double;
function DataFolder: String;
function ImageFolder: String;
function GetString(var Line: String; Delimiter: String=','): String;
function GetString2(var Line: String; Delimiters: String=','): String;
function GetTimeFromString(Temp: String): TDateTime;
function GetTime(var Line: String; Delimiter: String = ','): TDateTime;
function GetInteger(var Line: String; Delimiter: String = ','): Integer;
function GetFloat(var Line: String; Delimiter: String = ','): Double;
function SourceName(SourceID: Integer): String;
procedure AddHostNameToIPAddress(HostName, IPAddress: String);
function GetIPAddressFromHostName(HostName: String): String;
function MyStrToFloat(Value: String): Double;
function MyFormatFloat(Format: String; Value: Double): String;
function CalculateHorizonRadius(Altitude, Elevation: Double): Double;
procedure DoPositionCalculations(PreviousPosition: THABPosition; var NewPosition: THABPosition);
function EncryptMessage(Password, Msg: String): String;

{$IFDEF ANDROID}
    function MagneticDeclination: Single;
{$ENDIF}

procedure InitialiseSettings;

function GetSettingString(Section, Item, Default: String): String;
function GetSettingInteger(Section, Item: String; Default: Integer): Integer;
function GetSettingBoolean(Section, Item: String; Default: Boolean): Boolean;
function GetGroupChangedFlag(Section: String): Boolean;

procedure SetSettingString(Section, Item, Value: String);
procedure SetSettingInteger(Section, Item: String; Value: Integer);
procedure SetSettingBoolean(Section, Item: String; Value: Boolean);
procedure SetGroupChangedFlag(Section: String; Changed: Boolean);

const
    GPS_SOURCE = 0;
    SERIAL_SOURCE = 1;
    BLUETOOTH_SOURCE = 2;
    UDP_SOURCE = 3;
    SONDEHUB_SOURCE = 4;
    GATEWAY_SOURCE_1 = 5;
    GATEWAY_SOURCE_2 = 6;
    HABLINK_SOURCE = 7;


var
    INIFileName: String;

implementation

uses SysUtils;

var
    Settings: TSettings;
    Ini: TIniFile;
    IPLookups: TIPLookups;

function SourceName(SourceID: Integer): String;
const
    SourceNames: Array[0..6] of String = ('GPS', 'Serial', 'Bluetooth', 'UDP', 'Habitat', 'Gateway', 'Gateway 2');
begin
    if (SourceID >= Low(SourceNames)) and (SourceID <= High(SourceNames)) then begin
        Result := SourceNames[SourceID];
    end else begin
        Result := '';
    end;
end;

function DataFolder: String;
begin
    if ParamCount > 0 then begin
        Result := ParamStr(1);
    end else begin
        Result := ExtractFilePath(ParamStr(0));
    end;

    {$IF Defined(IOS) or Defined(ANDROID)}
        Result := TPath.GetDocumentsPath;
    {$ENDIF}

    Result := IncludeTrailingPathDelimiter(Result);
end;

function ImageFolder: String;
begin
    Result := DataFolder;

    {$IFDEF MSWINDOWS}
        Result := TPath.Combine(DataFolder, 'Images');
    {$ENDIF}
end;


function GetUDPString(Line: String; FieldName: String): String;
var
    Position: Integer;
begin
    // Gateway:HOST=xxxx,IP=yyyyy

    Position := Pos(FieldName + '=', Line);

    if Position > 0 then begin
        Line := Copy(Line, Position + Length(FieldName) + 1, 999);

        Position := Pos(',', Line);

        Result := Copy(Line, 1, Position-1);
    end;
end;

function GetJSONString(Line: String; FieldName: String; StringChar: String = '"'): String;
var
    Position: Integer;
begin
    // {"class":"POSN","payload":"NOTAFLIGHT","time":"13:01:56","lat":52.01363,"lon":-2.50647,"alt":5507,"rate":7.0}
    // "callsign": "R3330018"

    Position := Pos(StringChar + FieldName + StringChar + ':', Line);

    if Position > 0 then begin
        // Remove fieldname in quotes and ':'
        Line := Copy(Line, Position + Length(FieldName) + 3, 999);

        // Remove up to first quote

        Position := Pos(StringChar, Line);
        if (Position > 0) and (Position <= 2) then begin
            // Remove first quote
            Line := Copy(Line, Position+1, 999);

            // Find and trim to next quote
            Position := Pos(StringChar, Line);
            Result := Copy(Line, 1, Position-1);
        end else begin
            Position := Pos(',', Line);
            if Position = 0 then begin
                Position := Pos('}', Line);
            end;

            Result := Copy(Line, 1, Position-1);
        end;
    end else begin
        Result := '';
    end;
end;

function GetJSONFloat(Line: String; FieldName: String; StringChar: String = '"'): Double;
var
    Position: Integer;
begin
    // {"class":"POSN","payload":"NOTAFLIGHT","time":"13:01:56","lat":52.01363,"lon":-2.50647,"alt":5507,"rate":7.0}

    Position := Pos(StringChar + FieldName + StringChar + ':', Line);

    if Position > 0 then begin
        Line := Copy(Line, Position + Length(FieldName) + 3, 999);

        Position := Pos(',', Line);
        if Position = 0 then begin
            Position := Pos('}', Line);
        end else if Pos('}', Line) < Position then begin
            Position := Pos('}', Line);
        end;


        Line := Copy(Line, 1, Position-1);

        if Copy(Line, 1, 1) = StringChar then begin
            Line := Copy(Line,2, Length(Line)-2);
        end;

        try
            Result := MyStrToFloat(Line);
        except
            Result := 0;
        end;
    end else begin
        Result := 0;
    end;
end;

function GetJSONInteger(Line: String; FieldName: String; StringChar: String = '"'): LongInt;
var
    Position: Integer;
begin
    // {"class":"POSN","payload":"NOTAFLIGHT","time":"13:01:56","lat":52.01363,"lon":-2.50647,"alt":5507,"rate":7.0}

    Position := Pos(StringChar + FieldName + StringChar + ':', Line);

    if Position > 0 then begin
        Line := Copy(Line, Position + Length(FieldName) + 3, 999);

        Position := Pos(',', Line);
        if Position = 0 then begin
            Position := Pos('}', Line);
        end;

        Line := Copy(Line, 1, Position-1);

        if Copy(Line, 1, 1) = StringChar then begin
            Line := Copy(Line,2, Length(Line)-2);
        end;

        Result := StrToIntDef(Line, 0);
    end else begin
        Result := 0;
    end;
end;

procedure InsertDate(var TimeStamp: TDateTime);
begin
    if TimeStamp < 1 then begin
        // Add today's date
        TimeStamp := TimeStamp + Trunc(TTimeZone.Local.ToUniversalTime(Now));

        if (TimeStamp > 0.99) and (Frac(TTimeZone.Local.ToUniversalTime(Now)) < 0.01) then begin
            // Just past midnight, but payload transmitted just before midnight, so use yesterday's date
            TimeStamp := TimeStamp - 1;
        end;
    end;
end;


function CalculateAirDensity(alt: Double): Double;
var
    Temperature, Pressure: Double;
begin
    if alt < 11000.0 then begin
        // below 11Km - Troposphere
        Temperature := 15.04 - (0.00649 * alt);
        Pressure := 101.29 * power((Temperature + 273.1) / 288.08, 5.256);
    end else if alt < 25000.0 then begin
        // between 11Km and 25Km - lower Stratosphere
        Temperature := -56.46;
        Pressure := 22.65 * exp(1.73 - ( 0.000157 * alt));
    end else begin
        // above 25Km - upper Stratosphere
        Temperature := -131.21 + (0.00299 * alt);
        Pressure := 2.488 * power((Temperature + 273.1) / 216.6, -11.388);
    end;

    Result := Pressure / (0.2869 * (Temperature + 273.1));
end;

function CalculateDescentRate(Weight, Density, CDTimesArea: Double): Double;
begin
    Result := sqrt((Weight * 9.81)/(0.5 * Density * CDTimesArea));
end;

function CalculateCDA(Weight, Altitude, DescentRate: Double): Double;
var
	Density: Double;
begin
	Density := CalculateAirDensity(Altitude);

    Result := (Weight * 9.81)/(0.5 * Density * DescentRate * DescentRate);
end;

function CalculateDescentTime(Altitude, DescentRate, Land: Double): Double;
var
    Density, CDTimesArea, TimeAtAltitude, TotalTime, Step: Double;
begin
    Step := 100;

    CDTimesArea := CalculateCDA(1.0, Altitude, DescentRate);

    TotalTime := 0;

    while Altitude > Land do begin
        Density := CalculateAirDensity(Altitude);

        DescentRate := CalculateDescentRate(1.0, Density, CDTimesArea);

        TimeAtAltitude := Step / DescentRate;
        TotalTime := TotalTime + TimeAtAltitude;

        Altitude := Altitude - Step;
    end;

    Result := TotalTime;
end;

procedure InitialiseSettings;
begin
    Settings := TSettings.Create;

    if INIFileName <> '' then begin
        Ini := TIniFile.Create(INIFileName);
    end;
end;

function GetSettingString(Section, Item, Default: String): String;
var
    Key: String;
begin
    Key := Section + '/' + Item;

    if Settings.ContainsKey(Key) then begin
        Result := Settings[Key];
    end else if Ini = nil then begin
        Result := Default;
    end else begin
        Result := Ini.ReadString(Section, Item, Default);
        Settings.Add(Key, Result);
    end;
end;

function GetSettingInteger(Section, Item: String; Default: Integer): Integer;
var
    Key, Temp: String;
begin
    Key := Section + '/' + Item;

    if Settings.ContainsKey(Key) then begin
        try
            Result := Settings[Key];
        except
            Result := Default;
        end;
    end else if Ini = nil then begin
        Result := Default;
    end else begin
        Temp := Ini.ReadString(Section, Item, '');
        Result := StrToIntDef(Temp, Default);
        Settings.Add(Key, Result);
    end;
end;

function GetSettingBoolean(Section, Item: String; Default: Boolean): Boolean;
var
    Key: String;
begin
    Key := Section + '/' + Item;

    if Settings.ContainsKey(Key) then begin
        Result := Settings[Key];
    end else if Ini = nil then begin
        Result := Default;
    end else begin
        try
            Result := Ini.ReadBool(Section, Item, Default);
        except
            Result := Default;
        end;
        Settings.Add(Key, Result);
    end;
end;

procedure SetGroupChangedFlag(Section: String; Changed: Boolean);
begin
    Settings.AddOrSetValue(Section, Changed);
end;

function GetGroupChangedFlag(Section: String): Boolean;
begin
    if Settings.ContainsKey(Section) then begin
        Result := Settings[Section];
    end else begin
        Result := False;
        Settings.Add(Section, False);
    end;
end;

procedure SetSettingString(Section, Item, Value: String);
var
    Key: String;
begin
    Key := Section + '/' + Item;
    Settings.AddOrSetValue(Key, Value);
    if Ini <> nil then begin
        Ini.WriteString(Section, Item, Value);
        Ini.UpdateFile;
    end;
end;

procedure SetSettingInteger(Section, Item: String; Value: Integer);
var
    Key: String;
begin
    Key := Section + '/' + Item;
    Settings.AddOrSetValue(Key, Value);
    if Ini <> nil then begin
        Ini.WriteInteger(Section, Item, Value);
        Ini.UpdateFile;
    end;
end;

procedure SetSettingBoolean(Section, Item: String; Value: Boolean);
var
    Key: String;
begin
    Key := Section + '/' + Item;
    Settings.AddOrSetValue(Key, Value);
    if Ini <> nil then begin
        Ini.WriteBool(Section, Item, Value);
        Ini.UpdateFile;
    end;
end;

function GetString(var Line: String; Delimiter: String=','): String;
var
    Position: Integer;
begin
    Position := Pos(Delimiter, string(Line));
    if Position > 0 then begin
        Result := Copy(Line, 1, Position-1);
        Line := Copy(Line, Position+Length(Delimiter), Length(Line));
    end else begin
        Result := Line;
        Line := '';
    end;
end;

function FindFirstCharacterInString(Line: String; Delimiters: String=','): Integer;
var
    i, j: Integer;
begin
    for i := 1 to Length(Line) do begin
        for j := 1 to Length(Delimiters) do begin
            if Copy(Line,i,1) = Copy(Delimiters,j,1) then begin
                Result := i;
                Exit;
            end;
        end;
    end;

    Result := 0;
end;

function GetString2(var Line: String; Delimiters: String=','): String;
var
    Position: Integer;
begin
    Position := FindFirstCharacterInString(Line, Delimiters);

    if Position > 0 then begin
        Result := Copy(Line, 1, Position);
        Line := Copy(Line, Position+1, Length(Line));
    end else begin
        Result := Line;
        Line := '';
    end;
end;

function GetInteger(var Line: String; Delimiter: String = ','): Integer;
var
    Temp: String;
begin
    Temp := GetString(Line, Delimiter);

    Result := StrToIntDef(Temp, 0);
end;

function GetFloat(var Line: String; Delimiter: String = ','): Double;
var
    Temp: String;
begin
    Temp := GetString(Line, Delimiter);

    try
        Result := MyStrToFloat(Temp);
    except
        Result := 0.0;
    end;
end;

function GetTimeFromString(Temp: String): TDateTime;
begin
    try
        if Pos(':', Temp) > 0 then begin
            Result := EncodeTime(StrToIntDef(Copy(Temp, 1, 2), 0),
                      StrToIntDef(Copy(Temp, 4, 2), 0),
                      StrToIntDef(Copy(Temp, 7, 2), 0),
                      0);
        end else begin
            Result := EncodeTime(StrToIntDef(Copy(Temp, 1, 2), 0),
                      StrToIntDef(Copy(Temp, 3, 2), 0),
                      StrToIntDef(Copy(Temp, 5, 2), 0),
                      0);
        end;
    except
        Result := 0;
    end;
end;


function GetTime(var Line: String; Delimiter: String = ','): TDateTime;
var
    Temp: String;
begin
    Temp := GetString(Line, Delimiter);

    Result := GetTimeFromString(Temp);
end;

procedure AddHostNameToIPAddress(HostName, IPAddress: String);
var
    i: Integer;
begin
    for i := 1 to IPLookups.Count do begin
        if IPLookups.IPLookups[i].HostName = HostName then begin
            if IPLookups.IPLookups[i].IPAddress <> IPAddress then begin
                IPLookups.IPLookups[i].IPAddress := IPAddress;
            end;
            Exit;
        end;
    end;

    if IPLookups.Count < High(IPLookups.IPLookups) then begin
        Inc(IPLookups.Count);
        IPLookups.IPLookups[IPLookups.Count].HostName := HostName;
        IPLookups.IPLookups[IPLookups.Count].IPAddress := IPAddress;
    end;
end;

function GetIPAddressFromHostName(HostName: String): String;
var
    i: Integer;
begin
    for i := 1 to IPLookups.Count do begin
        if UpperCase(IPLookups.IPLookups[i].HostName) = UpperCase(HostName) then begin
            Result := IPLookups.IPLookups[i].IPAddress;
            Exit;
        end;
    end;

    Result := '';
end;

function MyStrToFloat(Value: String): Double;
//var
//    LFormat: TFormatSettings;
begin
//    if FormatSettings.DecimalSeparator <> '.' then begin
//        Value := StringReplace(Value, '.', FormatSettings.DecimalSeparator, []);
//    end;

//    LFormat := TFormatSettings.Create;          // Note: no need to free this
//    LFormat.DecimalSeparator := '.';
//    LFormat.ThousandSeparator := ',';

    try
        Result := StrToFloat(Value,  TFormatSettings.Invariant);
    except
        try
            Result := StrToFloat(Value);
        except
            Result := 0;
        end;
    end;
end;

{$IFDEF ANDROID}
function SwitchDWords(n:int64):int64;    // switch hi <--> lo dwords of an int64
var i: Integer;
    nn :int64; nnA:array[0..7]  of byte absolute nn;
    nn1:int64; nn1A:array[0..7] of byte absolute nn1;
begin
  nn1 := n;           // copy n
  for i := 0 to 3 do  // switch bytes  hidword <--> lodword
    begin
      nnA[i]   := nn1A[i+4];
      nnA[i+4] := nn1A[i];
    end;
  Result := nn;
end;

function MagneticDeclination: Single;
var
    GeoField: JGeomagneticField; tm:int64;  aD:Single;
begin
    tm := System.DateUtils.DateTimeToUnix( Now, {InputAsUTC:} false )*1000;

    tm := switchDWords(tm);    // <--  hack tm

    GeoField := TJGeomagneticField.JavaClass.init({Lat:}51.95023, {Lon:}-2.54445, {Alt:}145, tm );
    aD := GeoField.getDeclination();   // <-- this shows -21.8416 which is correct !

    Result := aD;
end;
{$ENDIF}

function CalculateDistance(HABLatitude, HABLongitude, CarLatitude, CarLongitude: Double): Double;
begin
    // Return distance in metres

    HABLatitude := HABLatitude * Pi / 180;
    HABLongitude := HABLongitude * Pi / 180;
    CarLatitude := CarLatitude * Pi / 180;
    CarLongitude := CarLongitude * Pi / 180;

    try
        Result := 6371000 * arccos(sin(CarLatitude) * sin(HABLatitude) +
                                   cos(CarLatitude) * cos(HABLatitude) * cos(HABLongitude-CarLongitude));
    except
        Result := 0.0;
    end;
end;

function CalculateDirection(HABLatitude, HabLongitude, CarLatitude, CarLongitude: Double): Double;
var
    x, y: Double;
begin
    HABLatitude := HABLatitude * Pi / 180;
    HabLongitude := HABLongitude * Pi / 180;
    CarLatitude := CarLatitude * Pi / 180;
    CarLongitude := CarLongitude * Pi / 180;

    y := sin(HABLongitude - CarLongitude) * cos(HABLatitude);
    x := cos(CarLatitude) * sin(HABLatitude) - sin(CarLatitude) * cos(HABLatitude) * cos(HABLongitude - CarLongitude);

    try
        Result := ArcTan2(y, x);
    except
        Result := 0.0;
    end;
end;

function CalculateElevation(lat1, lon1, alt1, lat2, lon2, alt2: Double): Double;
const
    Radius: Double = 6371000.0;
var
    d_lon, aa, ab, ea, eb, sa, sb, ta, tb, angle_at_centre: Double;
begin
    lat1 := DegToRad(lat1);
    lat2 := DegToRad(lat2);
    lon1 := DegToRad(lon1);
    lon2 := DegToRad(lon2);

    d_lon := lon2 - lon1;

    sa := cos(lat2) * sin(d_lon);
    sb := (cos(lat1) * sin(lat2)) - (sin(lat1) * cos(lat2) * cos(d_lon));

    aa := sqrt((sqr(sa)) + (sqr(sb)));
    ab := (sin(lat1) * sin(lat2)) + (cos(lat1) * cos(lat2) * cos(d_lon));

    angle_at_centre := arctan2(aa, ab);

    ta := radius + alt1;
    tb := radius + alt2;

    ea := (cos(angle_at_centre) * tb) - ta;
    eb := sin(angle_at_centre) * tb;

    Result := RadToDeg(arctan2(ea, eb));
end;

function MyFormatFloat(Format: String; Value: Double): String;
//var
//    LFormat: TFormatSettings;
begin
//    LFormat := TFormatSettings.Create;          // Note: no need to free this
//    LFormat.DecimalSeparator := '.';
//    LFormat.ThousandSeparator := ',';
//
//    Result := FormatFloat(Format, Value, LFormat);
    Result := FormatFloat(Format, Value,  TFormatSettings.Invariant);

//    if FormatSettings.DecimalSeparator <> '.' then begin
//        Result := StringReplace(Result, FormatSettings.DecimalSeparator, '.', []);
//    end;
end;

function CalculateHorizonRadius(Altitude, Elevation: Double): Double;
const
    Radius: Double = 6378.10;        // radius of earth
var
    Angle, Slant: Double;
begin
    // Altitude in metres
    // Elevation in degrees
    // Result in km

    if Elevation > 0 then begin
        Altitude := Altitude / 1000.0;      // km
        Angle := DegToRad(Elevation);

        Slant := Radius * (cos(Pi/2 + Angle) + sqrt(power(cos(Pi/2 + Angle),2) + Altitude * (2 * Radius + Altitude) / power(Radius,2)));

        Result := arccos((power(Radius,2) + power(Radius+Altitude,2) - power(Slant,2)) / (2 * Radius * (Radius + Altitude))) * Radius;
    end else begin
        Result := sqrt(12.756 * Altitude);
    end;
end;

procedure DoPositionCalculations(PreviousPosition: THABPosition; var NewPosition: THABPosition);
begin
    if NewPosition.InUse and PreviousPosition.InUse and (NewPosition.TimeStamp > PreviousPosition.TimeStamp) then begin
        NewPosition.AscentRate := (NewPosition.Altitude - PreviousPosition.Altitude) /
                                  ((NewPosition.TimeStamp - PreviousPosition.TimeStamp) * 86400);
        NewPosition.HaveAscentRate := True;
        if NewPosition.AscentRate < -2 then begin
            NewPosition.FlightMode := fmDescending;
        end else if NewPosition.AscentRate > 2 then begin
            NewPosition.FlightMode := fmLaunched;
        end else if abs(NewPosition.AscentRate) < 1 then begin
            NewPosition.FlightMode := fmLanded;
//        end else if NewPosition.FlightMode = fmIdle then begin
//            NewPosition.FlightMode := fmLaunched;
        end else begin
            NewPosition.FlightMode := PreviousPosition.FlightMode;
        end;
    end else begin
        NewPosition.HaveAscentRate := False;
    end;
end;

function EncryptMessage(Password, Msg: String): String;
var
    i, j: Integer;
begin
    Result := Msg;

    if Password <> '' then begin
        j := Low(Password);

        for i := Low(Msg) to High(Msg) do begin
            Result[i] := Char((Ord(Result[i]) xor Ord(Password[j])) or $80);
            j := (j mod Length(Password)) + Low(Password);
        end;
    end;
end;

end.


