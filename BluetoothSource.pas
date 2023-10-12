unit BluetoothSource;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF ANDROID}
  Androidapi.JNIBridge, Androidapi.Jni.App, Androidapi.Jni.JavaTypes, Androidapi.Helpers,
  Androidapi.Jni.Widget, Androidapi.Jni.Os, Androidapi.Jni,
{$ENDIF}
System.Bluetooth, System.Bluetooth.Components, Source, Classes, SysUtils, Miscellaneous;

{$IF Defined(MSWINDOWS) or Defined(ANDROID)}

type
  TBluetoothSource = class(TSource)
  private
    { Private declarations }
    CurrentFrequency: Double;
    procedure InitialiseDevice;
  protected
    { Protected declarations }
    procedure Execute; override;
    function ExtractPositionFrom(Line: String; PayloadID: String = ''): THABPosition; override;
  public
    { Public declarations }
    procedure SendSetting(SettingName, SettingValue: String); override;
  end;

{$ENDIF}

implementation

{$IF Defined(MSWINDOWS) or Defined(ANDROID)}

procedure TBluetoothSource.InitialiseDevice;
begin
    SendSetting('F', GetSettingString(GroupName, 'Frequency', '434.250'));
    SendSetting('M', GetSettingString(GroupName, 'Mode', '1'));
end;

function SelectDevice(Bluetooth1: TBluetooth; DeviceName: String): Integer;
var
    i: Integer;
begin
    Result := -1;

    if Bluetooth1.LastPairedDevices <> nil then begin
        for i := 0 to Bluetooth1.LastPairedDevices.Count-1 do begin
            // if DeviceName = IntToStr(Ord(Bluetooth1.LastPairedDevices[i].State)) + ': ' + Bluetooth1.LastPairedDevices[i].DeviceName + ' - ' + Bluetooth1.LastPairedDevices[i].Address then begin
            if DeviceName = Bluetooth1.LastPairedDevices[i].DeviceName then begin
                Result := i;
                Exit;
            end;
        end;
    end;
end;

function FindService(LServices: TBluetoothServiceList): Integer;
var
    i: Integer;
begin
    Result := -1;

    for i := 0 to LServices.Count-1 do begin
        if Pos('SerialPort', LServices[i].Name) > 0 then begin
            Result := i;
            Exit;
        end;
    end;
end;

procedure TBluetoothSource.Execute;
{$IF Defined(MSWINDOWS) or Defined(ANDROID)}
var
    Position: THABPosition;
    i, DeviceIndex, ServiceIndex: Integer;
    Connected: Boolean;
    Bytes: TBytes;
    Line, Temp, Sentence, DeviceName: String;
    Bluetooth1: TBluetooth;
    LDevice: TBluetoothDevice;
    LServices: TBluetoothServiceList;
    FSocket: TBluetoothSocket;
    Guid: TGUID;
{$ENDIF}
begin
{$IF Defined(MSWINDOWS) or Defined(ANDROID)}
    inherited;

    Bluetooth1 := TBluetooth.Create(nil);
    Bluetooth1.Enabled := True;

    while not Terminated do begin
        DeviceName := GetSettingString(GroupName, 'Device', '');
        SetGroupChangedFlag(GroupName, False);

        if DeviceName = '' then begin
            SendMessage('No Device Selected');
        end else begin
            SendMessage('Connecting to ' + DeviceName + ' ...');

            // Get device
            DeviceIndex := SelectDevice(Bluetooth1, DeviceName);

            if DeviceIndex < 0 then begin
                SendMessage('Cannot Find Device');
            end else begin
                SendMessage('Getting device services');

                LDevice := nil;
                LServices := nil;

                LDevice := Bluetooth1.LastPairedDevices[DeviceIndex];

                LServices := LDevice.GetServices;

                ServiceIndex := FindService(LServices);

                if ServiceIndex < 0 then begin
                    SendMessage('Device has no serial service');
                end else begin
                    SendMessage('Device has serial service');

                    Guid := LServices[ServiceIndex].UUID;

                    // Now open socket
                    FSocket := LDevice.CreateClientSocket(Guid, True);

                    try
                        SendMessage('Attempting Connection');
                        FSocket.Connect;
                        Connected := True;
                        SendMessage('Connected To Device');
                    except
                        SendMessage('Cannot Connect To Device');
                    end;

                    if FSocket.Connected then begin
                        InitialiseDevice;
                        Line := '';

                        // while (not Terminated) and (not GetGroupChangedFlag(GroupName)) and Connected and (DeviceName = GetSettingString(GroupName, 'Device', '')) do begin
                        while (not Terminated) and Connected and (DeviceName = GetSettingString(GroupName, 'Device', '')) do begin
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
                                try
                                    SendMessage('Sending ' + Commands[0]);
                                    // Sleep(1000);
                                    FSocket.SendData(TEncoding.UTF8.GetBytes(Commands[0] + #13));
                                    Commands.Delete(0);
                                    // Sleep(1000);
                                    // SendMessage(' ');
                                except
                                    Connected := False;
                                    SendMessage('Disconnected From Device');
                                end;
                            end;

                            Bytes := FSocket.ReceiveData;

                            for i := 0 to Length(Bytes)-1 do begin
//                                if Bytes[i] = 13 then begin
//                                    Position := ExtractPositionFrom(Line);
//                                    SyncCallback(SourceID, True, '', Position);
//                                    Line := '';
//                                end else if Bytes[i] <> 10 then begin
//                                    Line := Line + Chr(Bytes[i]);
//                                end;
                                if Bytes[i] = 13 then begin
                                    Temp := String(Line);
                                    while Length(Temp) > 2 do begin
                                        Sentence := GetString(Temp, #10);
                                        try
                                            Position := ExtractPositionFrom(Sentence);
                                            SyncCallback(SourceID, True, '', Position);
                                        except
                                            SendMessage('Decoding Failed');
                                        end;
                                    end;
                                    Line := '';
                                end else if (Bytes[i] <> 10) or (Line <> '') then begin
                                    Line := Line + Chr(Bytes[i]);
                                end;
                            end;

                            Sleep(200);
                        end;

                        FSocket.Free;
                    end;
                end;

//                if LServices <> nil then LServices.Free;
//                if LDevice <> nil then LDevice.Free;
            end;
        end;

        Sleep(5000);
    end;

    Bluetooth1.Free;


{$ENDIF}
end;

function TBluetoothSource.ExtractPositionFrom(Line: String; PayloadID: String = ''): THABPosition;
var
    Command: String;
    Position: THABPosition;
begin
    // FillChar(Position, SizeOf(Position), 0);
    Position := default(THABPosition);

    try
        if Copy(Line, 1, 2) = '$$' then begin
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
            SyncCallback(SourceID, True, '', Position);
        end else if Command = 'HEX' then begin
            // SSDV
            Line := '55' + Line;
            inherited;
        end else if Command = 'FREQERR' then begin
            Position.FrequencyError := MyStrToFloat(Line);
            Position.HasFrequency := True;
            SyncCallback(SourceID, True, '', Position);
        end else if Command = 'PACKETRSSI' then begin
            Position.PacketRSSI := StrToIntDef(Line, 0);
            Position.HasPacketRSSI := True;
            SyncCallback(SourceID, True, '', Position);
        end else if Command = 'PACKETSNR' then begin
            SyncCallback(SourceID, True, '', Position);
        end else if Command = 'MESSAGE' then begin
            Position := inherited;
        end;

        Position.CurrentFrequency := CurrentFrequency;

        if Position.InUse then begin
            if UplinkDetails.When = uwAfterRx then begin
                AddCommand(UplinkDetails.Msg);
                UplinkDetails.When := uwNone;
            end;
        end;
    finally
        Result := Position;
    end;
end;

procedure TBluetoothSource.SendSetting(SettingName, SettingValue: String);
begin
    if SettingValue <> '' then begin
        AddCommand('~' + SettingName + SettingValue);
        if SettingName = 'F' then begin
            CurrentFrequency := MyStrToFloat(SettingValue);
        end;
    end;
end;

{$ENDIF}

end.
