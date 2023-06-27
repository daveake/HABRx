unit SerialSource;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF ANDROID}
  Androidapi.JNIBridge, Androidapi.Jni.App, Androidapi.Jni.JavaTypes, Androidapi.Helpers,
  Androidapi.Jni.Widget, Androidapi.Jni.Os, Androidapi.Jni,  Winsoft.Android.UsbSer, Winsoft.Android.Usb,
{$ENDIF}
Source, Classes, SysUtils, Miscellaneous;

type
  TSerialSource = class(TSource)
  private
    { Private declarations }
    CurrentFrequency: Double;
    LoRaMode: Integer;

    Line:               String;
    SNR:                Integer;
    HasSNR:             Boolean;
    PacketRSSI:         Integer;
    HasPacketRSSI:      Boolean;
    FrequencyError:     Double;
    HasFrequency:       Boolean;

{$IFDEF ANDROID}
    UsbDevices: TArray<JUsbDevice>;
    UsbSerial: TUsbSerial;
    SelectedUSBDevice: JUsbDevice;
    ShowWhenReceiving: Boolean;

    procedure RefreshDevices;
    function OpenUSBDevice(SelectedUSBDevice: JUsbDevice): Boolean;
    procedure OnDeviceAttached(Device: JUsbDevice);
    procedure OnDeviceDetached(Device: JUsbDevice);
    procedure OnReceivedData(Data: TJavaArray<Byte>);
{$ENDIF}
    procedure InitialiseDevice;
  protected
    { Protected declarations }
    procedure Execute; override;
    function ExtractPositionFrom(Line: String; PayloadID: String = ''): THABPosition; override;
  public
    { Public declarations }
    procedure SendSetting(SettingName, SettingValue: String); override;
  end;

implementation


procedure TSerialSource.InitialiseDevice;
begin
    // Request device information
    AddCommand('~D');
    AddCommand('~V');

    // LoRa settings
    SendSetting('F', GetSettingString(GroupName, 'Frequency', '434.250'));
    SendSetting('M', GetSettingString(GroupName, 'Mode', '1'));
end;

{$IFDEF MSWINDOWS}
function FixSerialPortName(ComPort: String): String;
begin
    Result := ComPort;

    if Copy(ComPort, 1, 3) = 'COM' then begin
        if Length(ComPort) > 4 then begin
            Result := '\\.\' + ComPort;
        end;
    end;
end;

procedure TSerialSource.Execute;
var
    Position: THABPosition;
    Temp, CommPort, Sentence: String;
    hCommFile : THandle;
    TimeoutBuffer: PCOMMTIMEOUTS;
    DCB : TDCB;
    NumberOfBytesRead, NumberOfBytesWritten: dword;
    Buffer: array[0..80] of Ansichar;
    TxBuffer: Array[0..16] of Ansichar;
    i, j: Integer;
begin
    inherited;

    while not Terminated do begin
        if GetSettingBoolean(GroupName, 'Enabled', True) then begin
            CommPort := FixSerialPortName(GetSettingString(GroupName, 'Port', ''));
            // SetGroupChangedFlag(GroupName, False);

            // Open serial port as a file
            hCommFile := CreateFile(PChar(CommPort),
                                  GENERIC_READ or GENERIC_WRITE,
                                  0,
                                  nil,
                                  OPEN_EXISTING,
                                  FILE_ATTRIBUTE_NORMAL,
                                  0);

             if hCommFile = INVALID_HANDLE_VALUE then begin
                Position := Default(THABPosition);
                SyncCallback(SourceID, False, 'Cannot open serial port ' + CommPort, Position);
                Sleep(1000);
             end else begin
                // Set baud rate etc
                GetCommState(hCommFile, DCB);
//                if True then begin
//                    // MySondy
//                    DCB.BaudRate := CBR_9600;
//                end else begin
//                    // LoRaGo
//                    DCB.BaudRate := CBR_57600;
//                end;
                DCB.BaudRate := CBR_57600;

                DCB.ByteSize := 8;
                DCB.StopBits := ONESTOPBIT;
                DCB.Parity := NOPARITY;
                if SetCommState(hCommFile, DCB) then begin
                    // Set timeouts
                    GetMem(TimeoutBuffer, sizeof(COMMTIMEOUTS));
                    GetCommTimeouts (hCommFile, TimeoutBuffer^);
                    TimeoutBuffer.ReadIntervalTimeout        := 300;
                    TimeoutBuffer.ReadTotalTimeoutMultiplier := 300;
                    TimeoutBuffer.ReadTotalTimeoutConstant   := 300;
                    SetCommTimeouts (hCommFile, TimeoutBuffer^);
                    FreeMem(TimeoutBuffer, sizeof(COMMTIMEOUTS));

                    // FillChar(Position, SizeOf(Position), 0);
                    Position := Default(THABPosition);
                    // SyncCallback(SourceID, True, '', Position);
                    SyncCallback(SourceID, True, 'Connected to ' + CommPort, Position);

                    InitialiseDevice;

                    // while (not Terminated) and (not GetGroupChangedFlag(GroupName)) do begin
                    while (not Terminated) and
                          (CommPort = FixSerialPortName(GetSettingString(GroupName, 'Port', ''))) do begin

                        if ReadFile(hCommFile, Buffer, sizeof(Buffer), NumberOfBytesRead, nil) then begin
                            for i := 0 to NumberOfBytesRead - 1 do begin
                                if Buffer[i] = #13 then begin
                                    Temp := String(Line);
                                    while Length(Temp) > 2 do begin
                                        try
                                            Sentence := GetString(Temp, #10);
                                            Position := ExtractPositionFrom(Sentence);
                                            SyncCallback(SourceID, True, '', Position);
                                        except
                                            SendMessage('Decoding Failed');
                                        end;
                                    end;
                                    Line := '';
                                end else if (Buffer[i] <> #10) or (Line <> '') then begin
                                    Line := Line + Buffer[i];
                                end;
                            end;
                        end;

                        if GetGroupChangedFlag(GroupName) then begin
                            InitialiseDevice;
                            SetGroupChangedFlag(GroupName, False);
                        end;

                        if UplinkDetails.When = uwSecondsAfterMinute then begin
                            if (Trunc(Now * 86400) mod 60) = UplinkDetails.Seconds then begin
                                AddCommand(UplinkDetails.Msg);
                                UplinkDetails.When := uwNone;
                            end;
                        end;

                        if Commands.Count > 0 then begin
                            Temp := Commands[0] + #13;
                            for j := 1 to Length(Temp) do begin
                                TxBuffer[j-1] := AnsiChar(Temp[j]);
                            end;
                            WriteFile(hCommFile, TxBuffer, Length(Temp), NumberOfBytesWritten, nil);
                            Commands.Delete(0);
                            Sleep(100);
                        end;
                    end;
                end;
                CloseHandle(hCommFile);

                Sleep(100);
            end;
        end else begin
            Position := Default(THABPosition);
            SyncCallback(SourceID, True, 'Source disabled', Position);
            Sleep(1000);
        end;
    end;
end;
{$ENDIF}

{$IFDEF ANDROID}
procedure TSerialSource.Execute;
var
    PermissionRequested: Boolean;
    Position: THABPosition;
    InitialiseAt: TDateTime;
begin
    inherited;

    InitialiseAt := 0;

    UsbSerial := TUsbSerial.Create;
    UsbSerial.OnDeviceAttached := OnDeviceAttached;
    UsbSerial.OnDeviceDetached := OnDeviceDetached;
    UsbSerial.OnReceivedData := OnReceivedData;

    RefreshDevices;

    PermissionRequested := False;

    while not Terminated do begin
        if GetSettingBoolean(GroupName, 'Enabled', True) then begin
            if UsbSerial.Opened then begin
                if UplinkDetails.When = uwSecondsAfterMinute then begin
                    if (Trunc(Now) mod 60) = UplinkDetails.Seconds then begin
                        AddCommand(UplinkDetails.Msg);
                        UplinkDetails.When := uwNone;
                    end;
                end;

                if (InitialiseAt > 0) and (Now >= InitialiseAt) then begin
                    InitialiseAt := 0;
                    InitialiseDevice;
                end;

                if Commands.Count > 0 then begin
                    UsbSerial.Write(TEncoding.UTF8.GetBytes(Commands[0] + #13), 0);
                    Commands.Delete(0);
                    Sleep(100);
                end;
            end else begin
                if SelectedUSBDevice = nil then begin
                    PermissionRequested := False;
                end else begin
                    if UsbSerial.IsSupported(SelectedUSBDevice) then begin
                        if UsbSerial.HasPermission(SelectedUSBDevice) then begin
                            if OpenUSBDevice(SelectedUSBDevice) then begin
                                SendMessage('USB Device Opened');
                                ShowWhenReceiving := True;
                                InitialiseAt := Now + 5/86400;
                                // Sleep(2000);         // Give device a chance to initialise itself before we program it
                                // InitialiseDevice;
                            end;
                        end else if not PermissionRequested then begin
                            PermissionRequested := True;
                            SendMessage('Requested Permission');
                            UsbSerial.RequestPermission(SelectedUSBDevice);
                        end;
                    end;
                end;
            end;

            Sleep(100);
        end else begin
            Position := Default(THABPosition);
            SyncCallback(SourceID, True, 'Source disabled', Position);
            Sleep(1000);
        end;
    end;
end;

//function AndroidApi: Integer;
//begin
//    Result := TJBuild_VERSION.JavaClass.SDK_INT;
//end;

procedure TSerialSource.RefreshDevices;
var
    I: Integer;
    // Device: JUsbDevice;
begin
    if UsbSerial.Opened then begin
        UsbSerial.Close;
    end;

    SelectedUSBDevice := nil;

    UsbDevices := UsbSerial.UsbDevices;
    if UsbDevices <> nil then begin
        for I := 0 to Length(UsbDevices) - 1 do begin
            SelectedUSBDevice := UsbDevices[I];
//            if AndroidApi >= 21 then begin
//                USBDeviceName := JStringToString(Device.getManufacturerName) + ' ' + JStringToString(Device.getProductName);
//            end else begin
//                USBDeviceName := JStringToString(Device.getDeviceName);
//            end;
        end;
    end;

    // Position.ShowMessage := True;
    if SelectedUSBDevice = nil then begin
        SendMessage('USB Disconnected');
    end else begin
        SendMessage('USB Connected');
    end;
end;

procedure TSerialSource.OnDeviceAttached(Device: JUsbDevice);
begin
    RefreshDevices;
end;

procedure TSerialSource.OnDeviceDetached(Device: JUsbDevice);
begin
    RefreshDevices;
end;

function ByteArrayToString(Data: TJavaArray<Byte>): string;
begin
    Result := '';

    try
        if (Data <> nil) and (Data.Length > 0) then begin
            Result := TEncoding.ANSI.GetString(ToByteArray(Data));
        end;
    except
    end;
end;

procedure TSerialSource.OnReceivedData(Data: TJavaArray<Byte>);
var
    NewText, Sentence: String;
    i: Integer;
    Character: Char;
    Position: THABPosition;
begin
    if ShowWhenReceiving then begin
        ShowWhenReceiving := False;
        SendMessage('Listening');
    end;

    NewText := ByteArrayToString(Data);

    for i := 0 to Length(NewText)-1 do begin
        Character := NewText[i];

//        if Character = #13 then begin
//            Position := ExtractPositionFrom(Line);
//            SyncCallback(SourceID, True, '', Position);
//            Line := '';
//        end else if Character <> #10 then begin
//            Line := Line + Character;
//        end;
        if Character = #13 then begin
            while Length(Line) > 2 do begin
                Sentence := GetString(Line, #10);
                Position := ExtractPositionFrom(Sentence);
                SyncCallback(SourceID, True, '', Position);
            end;
            Line := '';
        end else if (Character <> #10) or (Line <> '') then begin
            Line := Line + Character;
        end;
    end;
end;


function TSerialSource.OpenUSBDevice(SelectedUSBDevice: JUsbDevice): Boolean;
begin
    try
        UsbSerial.Connect(SelectedUSBDevice);
        UsbSerial.BaudRate := 57600;
        UsbSerial.Open(False);
        UsbSerial.SetDtr;
        UsbSerial.SetRts;
        Result := True;
    except
        Result := False;
    end;
end;


{$ENDIF}

{$IFDEF IOS}
procedure TSerialSource.Execute;
begin
    // Nothing as we have no serial comms code for IOS
end;
{$ENDIF}

function TSerialSource.ExtractPositionFrom(Line: String; PayloadID: String = ''): THABPosition;
var
    Command: String;
    Position: THABPosition;
begin
    Position := Default(THABPosition);

    try
        // Position.SignalValues := TSignalValues.Create;

        if Copy(Line, 1, 2) = '$$' then begin
            Line := 'MESSAGE=' + Line;
        end else if Copy(Line, 1, 2) = '^^' then begin
            Line := 'MESSAGE=' + Line;
        end else if Copy(Line, 2, 1) = '/' then begin
            if Pos(Copy(Line,1,1), '0123') > 0 then begin
                Line := 'MESSAGE=' + Line;
            end;
        end;

        Command := UpperCase(GetString(Line, '='));

        if Command = 'CURRENTRSSI' then begin
            Position.CurrentRSSI := StrToIntDef(Line, 0);
            Position.HasCurrentRSSI := True;
            // SyncCallback(SourceID, True, '', Position);
        end else if Command = 'HEX' then begin
            // SSDV ?
            if (Copy(Line,1,2) = '66') or (Copy(Line,1,2) = '67') then begin
                // Looks like SSDV
                Line := '55' + Line;
                Position := inherited;
            end;
        end else if Command = 'FREQERR' then begin
            FrequencyError := MyStrToFloat(Line);
            HasFrequency := True;
            // SyncCallback(SourceID, True, '', Position);
        end else if Command = 'PACKETRSSI' then begin
            // Position.SignalValues.Add('PacketRSSI', StrToIntDef(Line, 0));
            PacketRSSI := StrToIntDef(Line, 0);
            HasPacketRSSI := True;
            // SyncCallback(SourceID, True, '', Position);
        end else if Command = 'PACKETSNR' then begin
            SNR := StrToIntDef(Line, 0);
            HasSNR := True;

            Position.SNR := SNR;
            Position.HasSNR := True;
            SyncCallback(SourceID, True, '', Position);

            // Position.SignalValues.Add('PacketSNR', StrToIntDef(Line, 0));
            // SyncCallback(SourceID, True, '', Position);
        end else if Command = 'TX' then begin
            Position.Transmitting := Pos('N', UpperCase(Line)) > 0;
        end else if Command = 'DEVICE' then begin
            Position.Device := Line;
        end else if Command = 'VERSION' then begin
            Position.Version := Line;
        end else if Command = 'MESSAGE' then begin
            Position := inherited;

            Position.Modulation := 'LoRa Mode ' + IntToStr(LoRaMode);

            Position.SNR := SNR;
            Position.HasSNR := HasSNR;

            Position.PacketRSSI := PacketRSSI;
            Position.HasPacketRSSI := HasPacketRSSI;

            Position.FrequencyError := FrequencyError;
            Position.HasFrequency := HasFrequency;

            HasSNR := False;
            HasPacketRSSI := False;
            HasFrequency := False;
        end;

        if Position.InUse then begin
            if UplinkDetails.When = uwAfterRx then begin
                AddCommand(UplinkDetails.Msg);
                UplinkDetails.When := uwNone;
            end;
        end;
    finally
        Position.CurrentFrequency := CurrentFrequency;
        Result := Position;
    end;
end;

procedure TSerialSource.SendSetting(SettingName, SettingValue: String);
begin
    if SettingValue <> '' then begin
        AddCommand('~' + SettingName + SettingValue);
        if SettingName = 'F' then begin
            CurrentFrequency := MyStrToFloat(SettingValue);
        end;

        if SettingName = 'M' then begin
            LoRaMode := StrToIntDef(SettingValue, 1);
        end;
    end;
end;

end.
