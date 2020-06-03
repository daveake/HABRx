unit Source;

interface

uses Classes, SysUtils, Miscellaneous;



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
//    Enabled: Boolean;
    PositionCallback: TSourcePositionCallback;
    procedure LookForPredictionInFields(var Position: THABPosition; Fields: TStringList);
    procedure SendMessage(Line: String);
    procedure LookForPredictionInSentence(var Position: THABPosition);
    procedure Execute; override;
    procedure SyncCallback(ID: Integer; Connected: Boolean; Line: String; Position: THABPosition);
    function ExtractPositionFromSentence(Line: String; PayloadID: String = ''): THABPosition;
    function ExtractPositionFrom(Line: String; PayloadID: String = ''): THABPosition; virtual;
  public
    { Public declarations }
//    procedure Enable; virtual;
//    procedure Disable; virtual;
    procedure SendSetting(SettingName, SettingValue: String); virtual;
  public
    constructor Create(ID: Integer; Group: String; Callback: TSourcePositionCallback);
  end;

implementation

procedure TSource.Execute;
begin
    // Nothing to do here
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

    Position.Line := Line;

    try
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
            end else if Pos('{"comment":', Line) = 1 then begin
                // ChaseMapper sentence
                Position.InUse := True;
                Position.ReceivedAt := Now;
                Position.PayloadID := GetJSONString(Line, 'callsign');
                Position.Counter := 1;
                Temp := GetJSONString(Line, 'time');
                Position.TimeStamp := EncodeTime(StrToIntDef(Copy(Temp, 1, 2), 0),
                                      StrToIntDef(Copy(Temp, 4, 2), 0),
                                      StrToIntDef(Copy(Temp, 7, 2), 0),
                                      0);

                InsertDate(Position.TimeStamp);
                Position.Latitude :=  GetJSONFloat(Line, 'latitude');
                Position.Longitude := GetJSONFloat(Line, 'longitude');
                Position.Altitude := Round(GetJSONFloat(Line, 'altitude'));

    //            Position.Frequency := GetJSONFloat(Line, 'freq');
    //            Position.Speed := GetJSONFloat(Line, 'speed');
    //            Position.Heading := GetJSONFloat(Line, 'heading');
    //
    //            Position.TempExt := GetJSONFloat(Line, 'temp');
    //            Position.Model := GetJSONString(Line, 'model');
            end else if Pos('GATEWAY:', Line) = 1 then begin
                // Gateway meta data
                HostName := GetUDPString(Line, 'HOST');
                IPAddress := GetUDPString(Line, 'IP');

                AddHostNameToIPAddress(HostName, IPAddress);
            end;
        end;
    except
        // Failed conversion probably
    end;

    Result := Position;
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

procedure TSource.SendMessage(Line: String);
var
    Position: THABPosition;
begin
    // FillChar(Position, SizeOf(Position), 0);
    Position := Default(THABPosition);
    SyncCallback(SourceID, True, Line, Position);
end;

end.
