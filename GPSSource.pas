unit GPSSource;

interface

uses Miscellaneous, Source, SysUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
System.Classes;

type
  TGPSSource = class(TSource)
  private
    { Private declarations }
  protected
    { Protected declarations }
{$IFDEF MSWINDOWS}
    function ExtractPositionFrom(Line: String; PayloadID: String = ''): THABPosition; override;
    procedure Execute; override;
{$ENDIF}
  public
    { Public declarations }
  end;

implementation

function FixPosition(Position: Double): Double;
var
    Minutes, Seconds: Double;
begin
	Position := Position / 100;

	Minutes := Trunc(Position);
	Seconds := Frac(Position);

	Result := Minutes + Seconds * 5 / 3;
end;

{$IFDEF MSWINDOWS}
function TGPSSource.ExtractPositionFrom(Line: String; PayloadID: String = ''): THABPosition;
const
    Direction: Double = 0;
var
    Position: THABPosition;
    Fields: TStringList;
    Satellites: Integer;
    Speed: Double;
begin
    Position := default(THABPosition);

    try
        if Copy(Line, 1, 2) = '$G' then begin
            // Looks like an NME sentence so far
            if Copy(Line, 4, 3) = 'RMC' then begin
                // Just get direction from RMC
                Fields := TStringList.Create;
                ExtractStrings([','], [], pWideChar(Line), Fields);
                if Fields.Count >= 10 then begin
                    Speed := MyStrToFloat(Fields[7]);
                    if Speed > 2 then begin
                        Direction := MyStrToFloat(Fields[8]);
                    end;
                end;
                Fields.Free;
            end else if Copy(Line, 4, 3) = 'GGA' then begin
                Fields := TStringList.Create;
                ExtractStrings([','], [], pWideChar(Line), Fields);
                if Fields.Count >= 10 then begin
                    Satellites := StrToIntDef(Fields[7], 0);
                    if Satellites >= 3 then begin
                        Position.TimeStamp := EncodeTime(StrToIntDef(Copy(Fields[1], 1, 2), 0),
                                                         StrToIntDef(Copy(Fields[1], 3, 2), 0),
                                                         StrToIntDef(Copy(Fields[1], 5, 2), 0),
                                                         0);

                        Position.Latitude := FixPosition(MyStrToFloat(Fields[2]));
                        if Fields[3] = 'S' then Position.Latitude := -Position.Latitude;

                        Position.Longitude := FixPosition(MyStrToFloat(Fields[4]));
                        if Fields[5] = 'W' then Position.Longitude := -Position.Longitude;

                        if Fields[9] = 'M' then begin
                            Position.Altitude := MyStrToFloat(Fields[8]);
                        end else begin
                            Position.Altitude := MyStrToFloat(Fields[9]);
                        end;

                        Position.Direction := Direction;

                        Position.InUse := True;
                    end;
                end;
                Fields.Free;
            end;
        end;
    finally
        //
    end;

    Result := Position;
end;

procedure TGPSSource.Execute;           // For Windows
const
    Line: AnsiString='';
var
    Position: THABPosition;
    CommPort: String;
    hCommFile : THandle;
    TimeoutBuffer: PCOMMTIMEOUTS;
    DCB : TDCB;
    NumberOfBytesRead : dword;
    Buffer : array[0..80] of Ansichar;
    i: Integer;
begin
    while not Terminated do begin
        CommPort := GetSettingString(GroupName, 'Port', '');
        SetGroupChangedFlag(GroupName, False);

        // Open serial port as a file
        hCommFile := CreateFile(PChar(CommPort),
                              GENERIC_READ or GENERIC_WRITE,
                              0,
                              nil,
                              OPEN_EXISTING,
                              FILE_ATTRIBUTE_NORMAL,
                              0);

         if hCommFile = INVALID_HANDLE_VALUE then begin
            FillChar(Position, SizeOf(Position), 0);
            SyncCallback(SourceID, False, 'Cannot open serial port ' + CommPort, Position);
            Sleep(1000);
         end else begin
            // Set baud rate etc
            GetCommState(hCommFile, DCB);
            DCB.BaudRate := CBR_4800;
            DCB.ByteSize := 8;
            DCB.StopBits := ONESTOPBIT;
    //        DCB.Parity := NOPARITY;
            if SetCommState(hCommFile, DCB) then begin
                // Set timeouts
                GetMem(TimeoutBuffer, sizeof(COMMTIMEOUTS));
                GetCommTimeouts (hCommFile, TimeoutBuffer^);
                TimeoutBuffer.ReadIntervalTimeout        := 300;
                TimeoutBuffer.ReadTotalTimeoutMultiplier := 300;
                TimeoutBuffer.ReadTotalTimeoutConstant   := 300;
                SetCommTimeouts (hCommFile, TimeoutBuffer^);
                FreeMem(TimeoutBuffer, sizeof(COMMTIMEOUTS));

                FillChar(Position, SizeOf(Position), 0);
                SyncCallback(SourceID, True, '', Position);

                while (not Terminated) and (not GetGroupChangedFlag(GroupName)) do begin
                    if ReadFile(hCommFile, Buffer, sizeof(Buffer), NumberOfBytesRead, nil) then begin
                        for i := 0 to NumberOfBytesRead - 1 do begin
                            if Buffer[i] = #13 then begin
                                Position := ExtractPositionFrom(String(Line));
                                if Position.InUse then begin
                                    SyncCallback(SourceID, True, string(Line), Position);
                                end;
                                Line := '';
                            end else if Buffer[i] = '$' then begin
                                Line := Buffer[i];
                            end else if Line <> '' then begin
                                Line := Line + Buffer[i];
                            end;
                        end;
                    end;
                end;
            end;
            CloseHandle(hCommFile);
            Sleep(100);
        end;
    end;
end;
{$ENDIF}


end.

