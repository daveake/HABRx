unit Source;

interface

uses Classes, SysUtils, Miscellaneous;

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
    function ExtractPositionFromSentence(Line: String; PayloadID: String = ''): THABPosition;
    function ExtractPositionFrom(Line: String; PayloadID: String = ''): THABPosition; virtual;
    function GotFilterIfNeeded: Boolean; virtual;
    procedure AddCommand(Command: String);
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
    Position.ContainsPrediction := False;

    for i := 6 to Fields.Count-2 do begin
        try
            if (Pos('.', Fields[i]) > 0) and (Pos('.', Fields[i+1]) > 0) then begin
                if (abs(MyStrToFloat(Fields[i]) - Position.Latitude) < 1) and
                   (abs(MyStrToFloat(Fields[i+1]) - Position.Longitude) < 1) and
                   (Position.Latitude <> 0) and (Position.Longitude <> 0) then begin
                    Position.ContainsPrediction := True;
                    Position.PredictedLatitude := MyStrToFloat(Fields[i]);
                    Position.PredictedLongitude := MyStrToFloat(Fields[i+1]);
                    Exit;
                end;
            end;
        except
        end;
    end;
end;

procedure TSource.LookForPredictionInSentence(var Position: THABPosition);
var
    Fields: TStringList;
begin
    Position.ContainsPrediction := False;

    Fields := TStringList.Create;

    Split(',', Position.Line, Fields);

    LookForPredictionInFields(Position, Fields);

    Fields.Free;
end;

function TSource.ExtractPositionFrom(Line: String; PayloadID: String = ''): THABPosition;
begin
    if Pos('SENTENCE:', Line) = 1 then begin
        Result := ExtractPositionFromSentence(Copy(Line, 10, Length(Line)));
    end else begin
        Result := ExtractPositionFromSentence(Line);
    end;
end;

function TSource.ExtractPositionFromSentence(Line: String; PayloadID: String = ''): THABPosition;
var
    Position: THABPosition;
    Fields: TStringList;
    Temp, HostName, IPAddress: String;
    Offset: Integer;
begin
    Position := Default(THABPosition);

    try
        Position.Line := Line;

        if Pos(#10, Line) > 0 then begin
            Line := Copy(Line, 1, Pos(#10, Line)-1);
        end;

        if Line <> '' then begin
            // Check for repeated packets

            if Line[Low(Line)] in ['$', '%'] then begin
                // UKHAS sentence

                if Line[Low(Line)] = '%' then begin
                    Position.Repeated := True;
                    Line[Low(Line)] := '$';
                end;

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

                            if Pos(':', Fields[Offset+1]) > 0 then begin
                                // Position.TimeStamp :=  StrToTime(Fields[2]);
                                Position.TimeStamp :=  EncodeTime(StrToIntDef(Copy(Fields[Offset+1], 1, 2), 0),
                                                                  StrToIntDef(Copy(Fields[Offset+1], 4, 2), 0),
                                                                  StrToIntDef(Copy(Fields[Offset+1], 7, 2), 0),
                                                                  0);
                            end else begin
                                Position.TimeStamp :=  EncodeTime(StrToIntDef(Copy(Fields[Offset+1], 1, 2), 0),
                                                                  StrToIntDef(Copy(Fields[Offset+1], 3, 2), 0),
                                                                  StrToIntDef(Copy(Fields[Offset+1], 5, 2), 0),
                                                                  0);
                            end;
                            InsertDate(Position.TimeStamp);
                            Position.Latitude := MyStrToFloat(Fields[Offset+2]);
                            Position.Longitude := MyStrToFloat(Fields[Offset+3]);
                            Temp := Fields[Offset+4];
                            Position.Altitude := MyStrToFloat(GetString(Temp, '*'));

                            LookForPredictionInFields(Position, Fields);
                        end;
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
                Position.PayloadID := GetJSONString(Line, 'callsign');
                Position.CurrentFrequency := GetJSONFloat(Line, 'freq');
                Position.Speed := GetJSONFloat(Line, 'speed');
                Position.HaveSpeed := True;
    //            Position.Heading := GetJSONFloat(Line, 'heading');
    //
    //            Position.TempExt := GetJSONFloat(Line, 'temp');
                Position.Device := GetJSONString(Line, 'model');
            end else if Pos('GATEWAY:', Line) = 1 then begin
                // Gateway meta data
                HostName := GetUDPString(Line, 'HOST');
                IPAddress := GetUDPString(Line, 'IP');

                AddHostNameToIPAddress(HostName, IPAddress);
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


end.
