unit APRSSource;

interface

uses SocketSource, Miscellaneous, Source, SysUtils, Classes,
{$IFDEF VCL}
  ExtCtrls, Windows
{$ELSE}
  FMX.Types
{$ENDIF}
;

type TSendMode = (smIdle, smWaitingForHello, smWaitingForVerified, smRunning);

type
  TAPRSSource = class(TSocketSource)
  private
    { Private declarations }
    SendMode: TSendMode;
  protected
    { Protected declarations }
    procedure InitialiseDevice; override;
    function ExtractPositionFrom(Line: String; PayloadID: String = ''; CheckCRC: Boolean = False): THABPosition; override;
    function GotFilterIfNeeded: Boolean; override;
  public
    { Public declarations }
    procedure SendSetting(SettingName, SettingValue: String); override;
  end;

implementation


procedure TAPRSSource.InitialiseDevice;
begin
    SendMode := smWaitingForHello;
end;

function PossibleBalloonSSID(Callsign: String): Boolean;
var
    Minus, SSID: Integer;
begin
//    -0 Your primary station usually fixed and message capable
//    -1 generic additional station, digi, mobile, wx, etc
//    -2 generic additional station, digi, mobile, wx, etc
//    -3 generic additional station, digi, mobile, wx, etc
//    -4 generic additional station, digi, mobile, wx, etc
//    -5 Other networks (Dstar, Iphones, Androids, Blackberry's etc)
//    -6 Special activity, Satellite ops, camping or 6 meters, etc
//    -7 walkie talkies, HT's or other human portable
//    -8 boats, sailboats, RV's or second main mobile
//    -9 Primary Mobile (usually message capable)
//    -10 internet, Igates, echolink, winlink, AVRS, APRN, etc
//    -11 balloons, aircraft, spacecraft, etc
//    -12 APRStt, DTMF, RFID, devices, one-way trackers*, etc
//    -13 Weather stations
//    -14 Truckers or generally full time drivers
//    -15 generic additional station, digi, mobile, wx, etcend;

    Result := False;
    Minus := Pos('-', Callsign);

    if Minus > 0 then begin
        SSID := StrToIntDef(Copy(Callsign, Minus+1, 2), 11);
        Result := not (SSID in [0,5,6,7,8,9,10,13,14]);
    end else if Copy(Callsign, 1, 3) = 'OGN' then begin
        // Glider network might be a balloon
        Result := True;
    end;
end;

function ConvertLatLon(Value: String; Negative: Boolean): Double;
begin
    Result := StrToFloat(Copy(Value,1,3)) + StrToFloat(Copy(Value,4,5)) / 60;
    if Negative then result := -Result;
end;

function ParseUncompressedAPRSPosition(var Body: String; var Position: THABPosition): Boolean;
var
    Temp: String;
    Posn: Integer;
begin
    // CT2KAZ-11>CT2KAZ,TCPIP*,qAR,CT2KAZ:@112117h3846.72N/00903.21WO091/053/A=005299 SondeID:S4720769 [403.02MHz] GPSAlt:1615.09m P-Alt:1644.57m Spd:27.31m/s Clb:5.78m/s P:830.69hPa T:3.96oC U:99.41% FN:1764
    // @112117h3846.72N/00903.21WO091/053/A=005299 SondeID:S4720769 [403.02MHz] GPSAlt:1615.09m P-Alt:1644.57m Spd:27.31m/s Clb:5.78m/s P:830.69hPa T:3.96oC U:99.41% FN:1764

    if Pos(Copy(Body,7,1), 'zh') > 0 then begin
        // Got timestamp
        Position.TimeStamp := EncodeTime(StrToIntDef(Copy(Body, 1, 2), 0),
                                         StrToIntDef(Copy(Body, 3, 2), 0),
                                         StrToIntDef(Copy(Body, 5, 2), 0), 0);

        Body := Copy(Body, 8, Length(Body));
    end;

    try
        Position.Latitude := ConvertLatLon('0' + Copy(Body, 1, 7), Copy(Body,8,1) = 'S');
        Position.Longitude := ConvertLatLon(Copy(Body, 10, 8), Copy(Body,18,1) = 'W');

        Body := Copy(Body, 19, Length(Body));

        Position.Symbol := Body[1];

        Posn := Pos('/A=', Body);
        if Posn > 0 then begin
            Position.Altitude := StrToIntDef(Copy(Body, Posn+3, 6), 0) * 0.3048;
            Posn := Pos(' ID=', Body);
            if Posn > 0 then begin
                Temp := Copy(Body, Posn+4, Length(Body));
                Position.PayloadID := GetString(Temp, ' ');
            end;
        end;

        Result := Pos('SondeID', Body) = 0;

        if Result and (Copy(Position.PayloadID, 1, 3) = 'OGN') then begin
            Posn := Pos('kHz ', Body);
            if Posn > 0 then begin
                Position.FrequencyError := MyStrToFloat(Copy(Body, Posn-5, 5));
                Position.HasFrequency := True;
            end;

            Posn := Pos('dB ', Body);
            if Posn > 0 then begin
                Position.SNR := Round(MyStrToFloat(Copy(Body, Posn-4, 4)));
                Position.HasSNR := True;
            end;
        end;
    except
        // Memo1.Lines.Add('invalid characters in latitude/longitude encoding');
    end;
end;

function Base91ToInteger(Temp: String): Double;
var
    i: Integer;
    Multiplier: Double;
begin
    Result := 0;
    Multiplier := 1;

    // for i := High(Temp) downto Low(Temp) do begin
    for i := 4 downto 1 do begin
        Result := Result + (Ord(Temp[i]) - 33) * Multiplier;
        Multiplier := Multiplier * 91;
    end;
end;

function ParseCompressedAPRSPosition(var Body: String; var Position: THABPosition): Boolean;
var
    Compressed: String;
begin
    if (Body[1] = '/') and
       (Ord(Body[2]) in [33..124]) and
       (Ord(Body[3]) in [33..124]) and
       (Ord(Body[4]) in [33..124]) and
       (Ord(Body[5]) in [33..124]) then begin
        if Length(Body) >= 13 then begin
            Compressed := Copy(Body, 1, 13);
            Body := Copy(Body, 14, Length(Body));

            Position.Symbol := Compressed[10];

            try
                Position.Latitude := 90 - (Base91ToInteger(Copy(Compressed, 2, 4)) / 380926.0);
                Position.Longitude := -180 + (Base91ToInteger(Copy(Compressed, 6, 4)) / 190463.0);
                Result := True;

                if Copy(Body,1,2) = '/A' then begin
                    Position.Altitude := StrToIntDef(Copy(Body, 4, 6), 0) * 0.3048;
                end;
            except
                // Memo1.Lines.Add('invalid characters in latitude/longitude encoding');
            end;
        end;
     end;
end;

function ParseAPRSPosition(PacketType: Char; Body: String; var Position: THABPosition): Boolean;
var
    Posn: Integer;
begin
    Result := False;

    if Pos(PacketType, '!=/@;') = 0 then begin
        GetString(Body, '!');
        Body := '!';
    end;

    if PacketType = ';' then begin
        Posn := Pos('*', Body);
        if Posn > 0 then begin
            Position.PayloadID := Trim(Copy(Body, 1, Posn-1));
            Body := Copy(Body, Posn+1, Length(Body));
        end;
    end;

    if PossibleBalloonSSID(Position.PayloadID) then begin
        if Length(Body) >= 13 then begin
            Result := ParseCompressedAPRSPosition(Body, Position);

            if not Result then begin
                // Try as uncompressed
                Result := ParseUncompressedAPRSPosition(Body, Position);
            end;
        end;
    end;
end;

function ParseAPRSBody(PacketType: Char; Body: String; var Position: THABPosition): Boolean;
begin
    Result := False;

    if Pos('F0AA50', Position.PayloadID) > 0 then begin
        Position.IsSSDV := False;
    end;


    if Pos(PacketType, '#$%)*<?T[}') > 0 then begin
        // Memo1.Lines.Add('Unsupported format');
    end else if PacketType = ',' then begin
        // Memo1.Lines.Add('Invalid format');
    end else if PacketType = '{' then begin
        // Memo1.Lines.Add('Packet is user-defined');
    end else if PacketType = '>' then begin
        // Memo1.Lines.Add('Packet is just a status message');
    end else if Pos(PacketType, '`''') > 0 then begin
        // Memo1.Lines.Add('Mic-e packet');
    end else if PacketType = ':' then begin
        // Memo1.Lines.Add('Message packet');
    end else if PacketType = '_' then begin
        // Memo1.Lines.Add('Positionless weather report');
    end else if (Pos(PacketType, '!=/@;') > 0) or (Pos('!', Body) in [1..40]) then begin
        // Memo1.Lines.Add('POSITION PACKET');
        Result := ParseAPRSPosition(PacketType, Body, Position);
    end else begin
        // Memo1.Lines.Add('Format not recognised');
    end
end;

function ParseAPRS(Packet: String; var Position: THABPosition): Boolean;
var
    Header, Body: String;
    PacketType: Char;
begin
    Result := False;
    Position := default(THABPosition);
    Position.Counter := -1;
    Body := Packet;
    Header := GetString(Body, ':');
    if Packet <> '' then begin
        Position.PayloadID := GetString(Header, '>');
        Position.TimeStamp := Now;
        PacketType := Body[1];
        Body := Copy(Body,2,Length(Body));
        Result := ParseAPRSBody(PacketType, Body, Position);
    end;
end;


function TAPRSSource.ExtractPositionFrom(Line: String; PayloadID: String = ''; CheckCRC: Boolean = False): THABPosition;
var
    Position: THABPosition;
    Callsign, Passcode, FullLine: String;
begin
    FillChar(Position, SizeOf(Position), 0);

    case SendMode of
        smWaitingForHello: begin
            if Pos('# aprsc', Line) > 0 then begin
                SendMode := smWaitingForVerified;
                Callsign := 'N0CALL';
                Passcode := '-1';

                AddCommand('user ' + Callsign + ' pass ' + PassCode + ' filter ' + SourceFilter);

                SyncCallback(SourceID, True, 'Registering as user ' + Callsign + ' with filter ' + SourceFilter, Position);
            end;
        end;

        smWaitingForVerified: begin
            if Pos('# logresp', Line) > 0 then begin
                SyncCallback(SourceID, True, 'Registered OK', Position);
                SendMode := smRunning;
            end;
        end;

        smRunning: begin
            if (Copy(Line, 1, 1) <> '#') and
               (Pos('auto_rx', Line) = 0) and
               (Pos('Radiosonda', Line) = 0) and
               (Pos('Radiosonde', Line) = 0) then begin
                // M0RPI-11>APRS,WIDE1-1,WIDE2-1,qAR,M0RPI-1:!/46;)MnLUO   /A=000482|!"|  /M0RPI,10'C,http://www.daveakerman.com
                FullLine := Line;
                Callsign := GetString(Line, '>');
                // if Pos('-11', Callsign) > 0 then begin
                if PossibleBalloonSSID(Callsign) then begin
                    // ListBox2.ItemIndex := ListBox2.Items.Add('Rx from ' + RxCallsign + ':');
                    // 'M0RPI-11>APEHAB,qAR,M0RPI-1:!/46;7MnLXO   /A=000488|!:!)"GPM|http://www.pi-in-the-sky.com'
                    // 'APEHAB,qAR,M0RPI-1:!/46;>MnLbO   /A=000478|!8!*"GPM|http://www.pi-in-the-sky.com'
                    if ParseAPRS(FullLine, Position) then begin
                        if PossibleBalloonSSID(Position.PayloadID) then begin
                            if Position.Symbol = 'O' then begin
                                // 'NSM10-11>APPMSP,WIDE1-1,WIDE2-1,qAO,F1ZWR-3:!4351.26N/00424.98EO119/015 Balloon is climbing   NSM is Not Sonde Monitor'
                                Position.InUse := True;
                            end;
                        end;
                    end;
                end;
            end;
        end;
    end;

    Result := Position;
end;

procedure TAPRSSource.SendSetting(SettingName, SettingValue: String);
begin
    AddCommand(SettingName + '=' + SettingValue);
end;

function TAPRSSource.GotFilterIfNeeded: Boolean;
begin
    Result := SourceFilter <> '';
end;


end.
