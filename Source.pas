unit Source;

interface

uses Classes, SysUtils, System.DateUtils, Miscellaneous, System.JSON;

type
  TUplinkWhen = (uwNone, uwNow, uwSecondsAfterMinute, uwAfterRx);

type
  TUplinkDetails = record
    When:       TUplinkWhen;
    Seconds:    Integer;
    Msg:        String;
end;

type
  TSourcePositionCallback = procedure(ID: Integer; Connected: Boolean; Line: String; Position: THABPosition) of object;

type
  TSource = class(TThread)
  private
    { Private declarations }
  protected
    { Protected declarations }
    SourceID: Integer;
    GroupName: String;
    SentenceCount: Integer;
    Commands: TStringList;
    UplinkDetails: TUplinkDetails;
    PositionCallback: TSourcePositionCallback;
    SourceFilter: String;
    NeedToReconnect: Boolean;
    procedure LookForPredictionInFields(var Position: THABPosition; Fields: TStringList);
    procedure SendMessage(Line: String);
    procedure LookForPredictionInSentence(var Position: THABPosition);
    procedure Execute; override;
    procedure SyncCallback(ID: Integer; Connected: Boolean; Line: String; Position: THABPosition);
    function ExtractPositionFromSentence(Line: String; PayloadID: String = ''; CheckCRC: Boolean = False): THABPosition;
    function ExtractPositionFrom(Line: String; PayloadID: String = ''; CheckCRC: Boolean = False): THABPosition; virtual;
    function ExtractChaseCarPosition(Line: String): THABPosition;
    function GotFilterIfNeeded: Boolean; virtual;
    procedure AddCommand(Command: String);
    procedure ProcessMQTTMessage(Topic, Value: String);
  public
    { Public declarations }
    procedure SendUplink(When: TUplinkWhen; WhenValue, Channel: Integer; Prefix, Msg, Password: String);
    procedure SendSetting(SettingName, SettingValue: String); virtual;
    constructor Create(ID: Integer; Group: String; Callback: TSourcePositionCallback);
    function WaitingToSend: Boolean;
    procedure SetFilter(Filter: String);
  end;

implementation

procedure TSource.Execute;
begin
    Commands := TStringList.Create;
end;

constructor TSource.Create(ID: Integer; Group: String; Callback: TSourcePositionCallback);
begin
    SentenceCount := 0;
    SourceID := ID;
    GroupName := Group;
    PositionCallback := Callback;
//    Enabled := True;
    inherited Create(False);
end;

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings) ;
var
    Temp: String;
begin
   ListOfStrings.Clear;
   ListOfStrings.Delimiter       := Delimiter;
   ListOfStrings.StrictDelimiter := True;
   ListOfStrings.DelimitedText   := Str;

    if ListOfStrings.Count > 0 then begin
        // Remove "*CRC" from final field
        Temp := ListOfStrings[ListOfStrings.Count-1];
        ListOfStrings[ListOfStrings.Count-1] := GetString(Temp, '*');
    end;
end;

procedure TSource.LookForPredictionInFields(var Position: THABPosition; Fields: TStringList);
var
    i: Integer;
begin
    if Position.PredictionType = ptNone then begin
        for i := 6 to Fields.Count-2 do begin
            try
                if (Pos('.', Fields[i]) > 0) and (Pos('.', Fields[i+1]) > 0) then begin
                    if (abs(MyStrToFloat(Fields[i]) - Position.Latitude) < 1) and
                       (abs(MyStrToFloat(Fields[i+1]) - Position.Longitude) < 1) and
                       (Position.Latitude <> 0) and (Position.Longitude <> 0) then begin
                        Position.PredictionType := ptOnboard;
                        Position.PredictedLatitude := MyStrToFloat(Fields[i]);
                        Position.PredictedLongitude := MyStrToFloat(Fields[i+1]);
                        Exit;
                    end;
                end;
            except
            end;
        end;
    end;
end;

procedure TSource.LookForPredictionInSentence(var Position: THABPosition);
var
    Fields: TStringList;
begin
    if Position.PredictionType = ptNone then begin
        Fields := TStringList.Create;

        Split(',', Position.Line, Fields);

        LookForPredictionInFields(Position, Fields);

        Fields.Free;
    end;
end;

function TSource.ExtractChaseCarPosition(Line: String): THABPosition;
begin
    Result := default(THABPosition);

    Result.PayloadID := GetString(Line);
    Result.TimeStamp := GetTime(Line);
    Result.Latitude := GetFloat(Line);
    Result.Longitude := GetFloat(Line);
    Result.Altitude := GetFloat(Line);
    Result.IsChase := True;
    Result.InUse := True;
end;

function TSource.ExtractPositionFrom(Line: String; PayloadID: String = ''; CheckCRC: Boolean = False): THABPosition;
begin
    if Pos('SENTENCE:', Line) = 1 then begin
        Result := ExtractPositionFromSentence(Copy(Line, 10, Length(Line)));
    end else if Pos('CHASE:', Line) = 1 then begin
        Result := ExtractChaseCarPosition(Copy(Line, 7, Length(Line)));
    end else begin
        Result := ExtractPositionFromSentence(Line, PayloadID, CheckCRC);
    end;
end;

function ConvertLatLonToDecimal(Value: String): Double;
var
    Position, Minutes, Seconds: Double;
begin
    // 5157.01N
    // 00232.66W

  	Position := MyStrToFloat(Copy(Value, 1, Length(Value)-1));

    Position := Position / 100;

	Minutes := Trunc(Position);
	Seconds := Frac(Position);

	Result := Minutes + Seconds * 5 / 3;

    if Pos(Copy(Value, Length(Value), 1), 'SW') > 0 then begin
        Result := -Result;
    end;
end;

function ExtractFieldList(Fields: TStringList): String;
var
    i: Integer;
begin
    Result := '';

    for i := 0 to Fields.Count-1 do begin
        if Copy(Fields[i], 1, 6) = '012345' then begin
            Result := Fields[i];
            Exit;
        end;
    end;
end;

procedure PopulateValuesFromFieldList(Fields: TStringList; var Position: THABPosition);
var
    i: Integer;
begin
    for i := 1 to Length(Position.FieldList) do begin
        try
            case Position.FieldList[i] of
                '6': begin
                    Position.Satellites := StrToIntDef(Fields[i-1], 0);
                    Position.HasSatelliteCount := True;
                end;

                '7': begin
                    Position.Speed := MyStrToFloat(Fields[i-1]);
                    Position.HaveSpeed := True;
                end;

                '8': begin
                    Position.Heading := MyStrToFloat(Fields[i-1]);
                    Position.HaveHeading := True;
                end;

                '9': begin
                    Position.BatteryVoltage := MyStrToFloat(Fields[i-1]) / 1000.0;
                    Position.HasBatteryVoltage := True;
                end;

                'A': begin
                    Position.InternalTemperature := MyStrToFloat(Fields[i-1]);
                    Position.HaveInternalTemperature := True;
                end;

                'B': begin
                    Position.ExternalTemperature := MyStrToFloat(Fields[i-1]);
                    Position.HaveExternalTemperature := True;
                end;

                'C': begin
                    Position.PredictedLatitude := MyStrToFloat(Fields[i-1]);
                    Position.PredictionType := ptOnboard;
                end;

                'D': begin
                    Position.PredictedLongitude := MyStrToFloat(Fields[i-1]);
                    Position.PredictionType := ptOnboard;
                end;

                'E': begin
                    Position.CutdownStatus := StrToIntDef(Fields[i-1], 0);
                    Position.HaveCutdownStatus := True;
                end;

                'F': begin
                    Position.UplinkSNR := StrToIntDef(Fields[i-1], 0);
                    Position.HaveUplinkSNR := True;
                end;

                'G': begin
                    Position.UplinkRSSI := StrToIntDef(Fields[i-1], 0);
                    Position.HaveUplinkRSSI := True;
                end;

                'H': begin
                    Position.UplinkCount := StrToIntDef(Fields[i-1], 0);
                    Position.HaveUplinkCount := True;
                end;

                'I'..'N': begin
                end;

                'O': begin
                end;

                'P': begin
                    Position.BatteryCurrent := MyStrToFloat(Fields[i-1]);
                    Position.HasBatteryCurrent := True;
                end;

                'Q': begin
                end;

                'R': begin
                    Position.Pressure := MyStrToFloat(Fields[i-1]);
                    Position.HavePressure := True;
                end;

                'S': begin
                    Position.Humidity := MyStrToFloat(Fields[i-1]);
                    Position.HaveHumidity := True;
                end;

                'T': begin
                    Position.CDA := StrToIntDef(Fields[i-1], 0);
                end;

                'U': begin
                    Position.LandingSpeed := MyStrToFloat(Fields[i-1]);
                    Position.HaveLandingSpeed := True;
                end;

                'V': begin
                    Position.TTL := StrToIntDef(Fields[i-1], 0);
                    Position.HaveTTL := True;
                end;

                'W': begin
                    Position.LastCommand := Fields[i-1];
                    Position.HaveLandingSpeed := True;
                end;
            end;
        except

        end;
    end;

end;

function ValidCRC(Line: String): Boolean;
var
    i, j, Finish: Integer;
    Started: Boolean;
    CRC, xPolynomial: WORD;
begin
    Result := False;
    Started := False;
    Finish := 0;
    CRC := $ffff;           // Seed
    xPolynomial := $1021;

    for i := 1 to Length(Line) do begin
        if (not Started) and (Line[i] <> '$') then begin
            Started := True;
        end;

        if Started then begin
            if Finish = 0 then begin
                if Line[i] = '*' then begin
                    Finish := i;
                end else begin
                    CRC := CRC xor (WORD(Line[i]) shl 8);
                    for j := 0 to 7 do begin
                        if (CRC and $8000) <> 0 then begin
                            CRC := CRC and $7FFF;
                            CRC := (CRC shl 1) xor xPolynomial;
                        end else begin
                            CRC := CRC shl 1;
                        end;
                    end;
                end;
            end;
        end;
    end;

    if Started and (Finish > 0) then begin
        Result := Copy(Line, Finish+1, 4) = IntToHex(CRC, 4);
    end;
end;

function TSource.ExtractPositionFromSentence(Line: String; PayloadID: String = ''; CheckCRC: Boolean = False): THABPosition;
var
    Position: THABPosition;
    Fields: TStringList;
    Temp, HostName, IPAddress, MessageType: String;
    Start, Offset: Integer;
begin
    Position := Default(THABPosition);

    try
        Position.Line := Line;

        if Pos(#10, Line) > 0 then begin
            Line := Copy(Line, 1, Pos(#10, Line)-1);
        end;

        if Line <> '' then begin
            if Copy(Line,1,2) = '^^' then begin
                // Calling Packet
                Fields := TStringList.Create;
                try
                    Split(',', Line, Fields);
                    if Fields.Count = 7 then begin
                        // Position.InUse := True;
                        Position.PayloadID := stringreplace(Fields[0], '^', '', [rfReplaceAll]);

                        Position.CallingModeMessage := True;
                        Position.CallingModeFrequency := MyStrToFloat(Fields[1]);
                        Position.CallingModeImplicit := StrToIntDef(Fields[2], 0);
                        Position.CallingModeError := StrToIntDef(Fields[3], 0);
                        Position.CallingModeBandwidth := StrToIntDef(Fields[4], 0);
                        Position.CallingModeSpreading := StrToIntDef(Fields[5], 0);
                        Position.CallingModeOptimize := StrToIntDef(Fields[6], 0);
                    end;
                finally
                    Fields.Free;
                end;
            end else if Pos('TELEMETRY,', Line) = 1 then begin
                // OziMux sentence
                Fields := TStringList.Create;
                try
                    Split(',', Line, Fields);
                    if Fields.Count >= 5 then begin
                        Position.InUse := True;
                        Position.ReceivedAt := Now;
                        Position.PayloadID := PayloadID;   // Not included in the telemetry string
                        Position.Counter := 1;
                        if Pos(':', Fields[1]) > 0 then begin
                            Position.TimeStamp :=  StrToTime(Fields[1]);
                        end else begin
                            Position.TimeStamp :=  EncodeTime(StrToIntDef(Copy(Fields[1], 1, 2), 0),
                                                              StrToIntDef(Copy(Fields[1], 3, 2), 0),
                                                              StrToIntDef(Copy(Fields[1], 5, 2), 0),
                                                              0);
                        end;
                        InsertDate(Position.TimeStamp);
                        Position.Latitude :=  MyStrToFloat(Fields[2]);
                        Position.Longitude :=  MyStrToFloat(Fields[3]);
                        Position.Altitude :=  MyStrToFloat(Fields[4]);
                    end;
                finally
                    Fields.Free;
                end;
            end else if Copy(Line,1,2) = '55' then begin
                // SSDV
                Position.Line := Line;
                Position.IsSSDV := True;
                Position.SSDVFileNumber := StrToIntDef('$' + Copy(Line,13,2), 0);
                Position.SSDVPacketNumber := StrToIntDef('$' + Copy(Line,15,4), 0);
            end else if Pos('"type": "PAYLOAD_SUMMARY"', Line) > 0 then begin
                // ChaseMapper sentence / Horus UDP "Payload Summary" messages
{
    'type' : 'PAYLOAD_SUMMARY',  # Packet Type
    'callsign' : callsign,       # Arbitrary text field, usually the payload callsign or serial number.
    'latitude' : latitude,       # float, Decimal Degrees
    'longitude' : longitude,     # float, Decimal Degrees
    'altitude' : altitude,       # int, Metres
    'speed' : speed,             # int, kph, if provided by the payload, otherwise -1
    'heading': heading,          # int, degrees 0-359, if provided by the payload, otherwise -1
    'time' : packet_time,        # Packet timestamp, ideally from the payload itself, as 'HH:MM:SS', in UTC time.
    'comment' : comment          # An optional comment field. This may not always be provided.
}
// {"bt": 65535, "f_centre": 405700000.0, "model": "RS41", "comment": "Radiosonde", "ppm": 215.33333333333334, "temp": -58.9, "fest": [9750.0, 14250.0], "station": "OZ1SKY_AUTO_RX", "time": "18:00:44", "callsign": "R3330018", "longitude": 10.38665, "type": "PAYLOAD_SUMMARY", "sats": 9, "frame": 5171, "humidity": -1.0, "freq": "405.700 MHz", "speed": 44.444483999999996, "batt": 2.7, "latitude": 52.43773, "altitude": 20806.8029, "snr": 10.6, "heading": 101.17816}

                Position.InUse := True;
                Position.ReceivedAt := Now;
                Position.PayloadID := GetJSONString(Line, 'callsign');
                Temp := GetJSONString(Line, 'time');
                Position.TimeStamp := EncodeTime(StrToIntDef(Copy(Temp, 1, 2), 0),
                                      StrToIntDef(Copy(Temp, 4, 2), 0),
                                      StrToIntDef(Copy(Temp, 7, 2), 0),
                                      0);

                InsertDate(Position.TimeStamp);
                Position.Latitude :=  GetJSONFloat(Line, 'latitude');
                Position.Longitude := GetJSONFloat(Line, 'longitude');
                Position.Altitude := Round(GetJSONFloat(Line, 'altitude'));

                Position.Satellites := GetJSONInteger(Line, 'sats');
                Position.BatteryVoltage := GetJSONFloat(Line, 'batt');
                Position.CurrentFrequency := GetJSONFloat(Line, 'freq');
                Position.Speed := GetJSONFloat(Line, 'speed');
                Position.HaveSpeed := True;
    //            Position.Heading := GetJSONFloat(Line, 'heading');
    //
    //            Position.TempExt := GetJSONFloat(Line, 'temp');
                Position.Device := GetJSONString(Line, 'model');


            end else if Pos('{''software_name'':', Line) > 0 then begin
                // Sondehub API V2 format
(*
{'software_name': 'radiosonde_auto_rx',
'software_version': '1.5.2',
'uploader_callsign': 'HomeBase',
'uploader_position': '51.43403,6.90144',
'uploader_antenna': 'wet shoelace under roof',
'time_received': '2021-08-04T11:28:11.182397Z',
'datetime': '2021-08-04T11:28:27.001000Z',
'manufacturer': 'Vaisala',
'type': 'RS41',
'serial': 'T1120367',
'subtype': 'RS41-SGP',
'frame': 4119,
'lat': 51.49745,
'lon': 7.25379,
'alt': 14153.89027,
'temp': -47.5,
'humidity': 1.2,
'pressure': 144.17,
'vel_v': 4.30559,
'vel_h': 8.58827,
'heading': 61.01915,
'sats': 8,
'batt': 2.7,
'frequency': 405.3, 'burst_timer': 65535, 'snr': 15.2, 'user-agent': 'Amazon CloudFront', 'position': '51.49745,7.25379', 'upload_time_delta': -0.403673, 'uploader_alt': 80.0}
*)
                try
                    Position.ReceivedAt := Now;
                    Position.PayloadID := GetJSONString(Line, 'serial', '''');
                    Temp := GetJSONString(Line, 'time_received', '''');
                    Position.TimeStamp := EncodeDateTime(StrToIntDef(Copy(Temp, 1, 4), 0),
                                                         StrToIntDef(Copy(Temp, 6, 2), 0),
                                                         StrToIntDef(Copy(Temp, 9, 2), 0),
                                                         StrToIntDef(Copy(Temp, 12, 2), 0),
                                                         StrToIntDef(Copy(Temp, 15, 2), 0),
                                                         StrToIntDef(Copy(Temp, 18, 2), 0),
                                                         0);

                    Position.Latitude :=  GetJSONFloat(Line, 'lat', '''');
                    Position.Longitude := GetJSONFloat(Line, 'lon', '''');
                    Position.Altitude := Round(GetJSONFloat(Line, 'alt', ''''));

                    Position.Satellites := GetJSONInteger(Line, 'sats', '''');
                    Position.BatteryVoltage := GetJSONFloat(Line, 'batt', '''');
                    Position.CurrentFrequency := GetJSONFloat(Line, 'frequency', '''');
                    Position.ExternalTemperature := GetJSONFloat(Line, 'temp', '''');
                    Position.Speed := GetJSONFloat(Line, 'speed', '''');
                    Position.HaveSpeed := True;
        //            Position.Heading := GetJSONFloat(Line, 'heading');
        //
        //            Position.TempExt := GetJSONFloat(Line, 'temp');
                    Position.Device := GetJSONString(Line, 'subtype', '''');
                    Position.InUse := True;
                except
                end;
            end else if Pos('GATEWAY:', Line) = 1 then begin
                // Gateway meta data
                HostName := GetUDPString(Line, 'HOST');
                IPAddress := GetUDPString(Line, 'IP');

                AddHostNameToIPAddress(HostName, IPAddress);
            end else if (Pos(Copy(Line,1,1), '0123') > 0) and (Copy(Line,2,1) = '/') then begin
                // MySonde format
                Position.IsSonde := True;
                MessageType := GetString(Line, '/');
                Position.Device := GetString(Line, '/');
                Position.CurrentFrequency := GetFloat(Line, '/');

                if MessageType = '0' then begin
                    // Status but no position
                    // 0/RS41/404.200/120.5/100/4260/-1/2.30/o
                    Position.CurrentRSSI := Round(GetFloat(Line, '/'));
                    Position.HasCurrentRSSI := True;
                end else if MessageType = '1' then begin
                    // Position
                    Position.PayloadID := GetString(Line, '/');
                    Position.Latitude := GetFloat(Line, '/');
                    Position.Longitude := GetFloat(Line, '/');
                    Position.Altitude := GetFloat(Line, '/');
                    Position.Speed := GetFloat(Line, '/');
                    Position.PacketRSSI := Round(GetFloat(Line, '/'));
                    Position.HasPacketRSSI := True;

                    Position.TimeStamp := TTimeZone.Local.ToUniversalTime(Now);
                    Position.InUse := True;
                    // 1/RS41/404.200/S2340007/51.95029/-2.54439/189.5/0.4/45.0/100/3967/0/30600/4391/-1/0/0/0/2.30/o
                    // 1/TYPE/FREQ/NAME/LAT/LON/ALT/VEL/SIGN/BAT%/AFC/BK/BKTIME/BATV/BUZMUTE/RESER/RESER/RESE R/VER/o
                end else if MessageType = '2' then begin
                    // 2/TYPE/FREQ/NAME/SIGN/BAT%/AFC/BATV/BUZMUTE/VER/o
                end else if MessageType = '3' then begin
                    // (Bluetooth) 3/TYPE/FREQ/OLED-SDA/OLED-SCL/OLED-RST/LED-PIN/RS41-BAND/M20-BAND/M10-BAND/PILOT-BAND/DFM-BAND/MYCALL/FREQ-OFS/BAT-PIN/BAT-MIN/BAT-MAX/BAT-TYPE/LCDTYPE/NAME-TYPE/BUZ-PIN/VER/o
                    // (Serial) 3/TYPE/FREQ/OLED-SDA/OLED-SCL/OLED-RST/LED-PIN/RS41-BAND/M20-BAND/M10-BAND/PILOTBAND/DFM-BAND/MYCALL/FREQ-OFS/BAT-PIN/BAT-MIN/BAT-MAX/BAT-TYPE/LCD-TYPE/NAME-TYPE/BUZPIN/VER/LCDON/BLUON/COM/BAUD/o
                end;
            end else if (Ord(Line[Low(Line)]) = 65533) and (Line[Low(Line)+16] = ';') then begin
                // AXUDP format from rtzTTGOSonde
                // ..........;S2340007 *155146z5157.01N/00232.66WO/A=000661!wN)!&Clb=0.5m/s 404.200MHz Type=RS41..'

                Line := Copy(Line, 18, 999);            // Remove up to the ';'
                // S2340007 *155146z5157.01N/00232.66WO/A=000661!wN)!&Clb=0.5m/s 404.200MHz Type=RS41'
                Position.Line := Line;

                Position.PayloadID := GetString(Line, ' ');
                //  *155146z5157.01N/00232.66WO/A=000661!wN)!&Clb=0.5m/s 404.200MHz Type=RS41'

                GetString(Line, '*');
                // 155146z5157.01N/00232.66WO/A=000661!wN)!&Clb=0.5m/s 404.200MHz Type=RS41'

                GetString2(Line, 'zh');
                // 5157.01N/00232.66WO/A=000661!wN)!&Clb=0.5m/s 404.200MHz Type=RS41'

                Position.Latitude := ConvertLatLonToDecimal(GetString2(Line, 'NS'));
                GetString(Line, '/');
                // 00232.66WO/A=000661!wN)!&Clb=0.5m/s 404.200MHz Type=RS41'

                Position.Longitude := ConvertLatLonToDecimal(GetString2(Line, 'EW'));
                // O/A=000661!wN)!&Clb=0.5m/s 404.200MHz Type=RS41'

                GetString(Line, '/A=');
                // O/A=000661!wN)!&Clb=0.5m/s 404.200MHz Type=RS41'
                Position.Altitude := GetFloat(Line, '!') / 3.28084;          // feet to metres

                // wN)!&Clb=0.5m/s 404.200MHz Type=RS41'

                Position.TimeStamp := TTimeZone.Local.ToUniversalTime(Now);
                Position.InUse := True;
            end else begin
                Start := Pos('$$', Line);
                if Start = 0 then begin
                    Start := Pos('%$', Line);
                    Position.Repeated := Start > 0;
                end;

                if Start > 0 then begin
                    Line := Copy(Line, Start, Length(Line));
                    Line[Low(Line)] := '$';
                    if CheckCRC and not ValidCRC(Line) then begin
                        Position.FailedCRC := True;
                    end else begin
                        Fields := TStringList.Create;
                        try
                            Split(',', Line, Fields);
                            if Fields.Count >= 5 then begin
                                // Do we have a sentence counter?
                                if Pos(':', Fields[1]) > 0 then begin
                                    // No sentence counter - straight into time
                                    Position.Counter := 0;
                                    Offset := 0;
                                end else begin
                                    Position.Counter := StrToIntDef(Fields[1], 0);
                                    Offset := 1;
                                end;

                                if Fields.Count >= Offset+5 then begin
                                    Position.InUse := True;
                                    Position.ReceivedAt := Now;
                                    Position.PayloadID := stringreplace(Fields[0], '$', '', [rfReplaceAll]);

                                    Position.TimeStamp :=  GetTimeFromString(Fields[Offset+1]);
        //                            if Pos(':', Fields[Offset+1]) > 0 then begin
        //                                // Position.TimeStamp :=  StrToTime(Fields[2]);
        //                                Position.TimeStamp :=  EncodeTime(StrToIntDef(Copy(Fields[Offset+1], 1, 2), 0),
        //                                                                  StrToIntDef(Copy(Fields[Offset+1], 4, 2), 0),
        //                                                                  StrToIntDef(Copy(Fields[Offset+1], 7, 2), 0),
        //                                                                  0);
        //                            end else begin
        //                                Position.TimeStamp :=  EncodeTime(StrToIntDef(Copy(Fields[Offset+1], 1, 2), 0),
        //                                                                  StrToIntDef(Copy(Fields[Offset+1], 3, 2), 0),
        //                                                                  StrToIntDef(Copy(Fields[Offset+1], 5, 2), 0),
        //                                                                  0);
        //                            end;
                                    InsertDate(Position.TimeStamp);
                                    Position.Latitude := MyStrToFloat(Fields[Offset+2]);
                                    Position.Longitude := MyStrToFloat(Fields[Offset+3]);
                                    Temp := Fields[Offset+4];
                                    Position.Altitude := MyStrToFloat(GetString(Temp, '*'));

                                    Position.FieldList := ExtractFieldList(Fields);

                                    if Position.FieldList = '' then begin
                                        LookForPredictionInFields(Position, Fields);
                                    end else begin
                                        PopulateValuesFromFieldList(Fields, Position);
                                    end;
                                end;
                            end;
                        finally
                            Fields.Free;
                        end;
                    end;
                end;
            end;
        end;
    finally
        Result := Position;
    end;

end;

procedure TSource.SyncCallback(ID: Integer; Connected: Boolean; Line: String; Position: THABPosition);
begin
    Synchronize(
        procedure begin
            PositionCallback(SourceID, Connected, Line, Position);
        end
    );
end;

//procedure TSource.Enable;
//begin
//    Enabled := True;
//end;
//
//procedure TSource.Disable;
//begin
//    Enabled := False;
//end;

procedure TSource.SendSetting(SettingName, SettingValue: String);
begin
    // virtual
end;

procedure TSource.AddCommand(Command: String);
begin
    Commands.Add(Command);
end;


procedure TSource.SendUplink(When: TUplinkWhen; WhenValue, Channel: Integer; Prefix, Msg, Password: String);
var
    ChannelText, EncodedMessage: String;
begin
    if Channel >= 0 then begin
        ChannelText := IntToStr(Channel);
    end else begin
        ChannelText := '';
    end;

    if Password <> '' then begin
        EncodedMessage := EncryptMessage(Password, Msg);
    end else begin
        EncodedMessage := Msg;
    end;

    UplinkDetails.When := When;
    UplinkDetails.Seconds := WhenValue;
    UplinkDetails.Msg := Prefix + ChannelText + EncodedMessage;

    if UplinkDetails.When = uwNow then begin
        AddCommand(UplinkDetails.Msg);
        UplinkDetails.When := uwNone;
    end;
end;

procedure TSource.SendMessage(Line: String);
var
    Position: THABPosition;
begin
    // FillChar(Position, SizeOf(Position), 0);
    Position := Default(THABPosition);
    SyncCallback(SourceID, True, Line, Position);
end;

function TSource.WaitingToSend: Boolean;
begin
    Result := UplinkDetails.When <> uwNone;
end;

function TSource.GotFilterIfNeeded: Boolean;
begin
    Result := True; // Most sources don't need a filter (exception so far is APRS)
end;

procedure TSource.SetFilter(Filter: String);
begin
    SourceFilter := Filter;
    NeedToReconnect := True;
end;

procedure TSource.ProcessMQTTMessage(Topic, Value: String);
var
    Position: THABPosition;
    TimeStamp, Sentence: String;
    JSONValue: TJSONValue;
begin
    Position := Default(THABPosition);

    if Copy(Value,1,2) = '$$' then begin
        Position := ExtractPositionFrom(Value);
    end else begin
        JSONValue := TJSONValue(TJSONObject.ParseJSONValue(Value));

        try
            if JSONValue.FindValue('raw') <> nil then begin
                Sentence := JSONValue.FindValue('raw').Value;
            end else begin
                Sentence := '';
            end;

            if Copy(Sentence, 1, 2) = '$$' then begin
                Position := ExtractPositionFrom(Sentence);
            end else begin
                Position.PayloadID := JSONValue.FindValue('payload_callsign').Value;

                TimeStamp := JSONValue.FindValue('datetime').Value;

                if Length(TimeStamp) >= 19 then begin
                    Position.TimeStamp := EncodeDateTime(StrToIntDef(Copy(TimeStamp, 1, 4), 0),
                                                         StrToIntDef(Copy(TimeStamp, 6, 2), 0),
                                                         StrToIntDef(Copy(TimeStamp, 9, 2), 0),
                                                         StrToIntDef(Copy(TimeStamp, 12, 2), 0),
                                                         StrToIntDef(Copy(TimeStamp, 15, 2), 0),
                                                         StrToIntDef(Copy(TimeStamp, 18, 2), 0),
                                                         0);
                    Position.Latitude := MyStrToFloat(JSONValue.FindValue('lat').Value);
                    Position.Longitude := MyStrToFloat(JSONValue.FindValue('lon').Value);
                    if JSONValue.FindValue('alt') <> nil then begin
                        Position.Altitude := MyStrToFloat(JSONValue.FindValue('alt').Value);
                    end;

                    Position.Line := Position.PayloadID + ',' + FormatDateTime('hh:nn:ss', Position.TimeStamp) + ',' + MyFormatFloat('0.00000', Position.Latitude) + ',' + MyFormatFloat('0.00000', Position.Longitude) + ',' + MyFormatFloat('0', Position.Altitude);

                    Position.ReceivedAt := Now;
                    Position.InUse := True;
                end;
            end;
        except
            Position.Line := 'Parsing Error';
            SyncCallback(SourceID, True, '', Position);
        end;
    end;

    if Position.InUse then begin
        SyncCallback(SourceID, True, '', Position);
    end;
end;


end.
