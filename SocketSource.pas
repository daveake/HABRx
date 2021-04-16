unit SocketSource;

interface

uses Source, Classes, SysUtils, Miscellaneous,
     IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient;

type
  TSocketSource = class(TSource)
  private
    { Private declarations }
    AClient: TIdTCPClient;
  protected
    { Protected declarations }
    procedure InitialiseDevice; virtual;
    procedure Execute; override;
  public
    { Public declarations }
  public
    constructor Create(ID: Integer; Group: String; Callback: TSourcePositionCallback);
  end;

implementation

procedure TSocketSource.Execute;
var
    Position: THABPosition;
    Host, HostOrIP, Line: String;
    Port: Integer;
begin
    inherited;

    // Create client
    AClient := TIdTCPClient.Create;

    while not Terminated do begin
        if GetSettingBoolean(GroupName, 'Enabled', True) and GotFilterIfNeeded then begin
            // Connect to socket server
            Host := GetSettingString(GroupName, 'Host', '');
            Port := GetSettingInteger(GroupName, 'Port', 0);
            // SetGroupChangedFlag(GroupName, False);

            if (Host = '') or (Port <= 0) then begin
                Position := Default(THABPosition);
                SyncCallback(SourceID, True, 'Source not configured', Position);
                Sleep(1000);
            end else begin
                NeedToReconnect := False;

                // while (not Terminated) and (not GetGroupChangedFlag(GroupName)) do begin
                while (not Terminated) and
                      (not NeedToReconnect) and
                      (Host = GetSettingString(GroupName, 'Host', '')) and
                      (Port = GetSettingInteger(GroupName, 'Port', 0)) and
                      GetSettingBoolean(GroupName, 'Enabled', True) do begin

                    HostOrIP := GetIPAddressFromHostName(Host);
                    if HostOrIP = '' then begin
                        HostOrIP := Host;
                    end;

                    try
                        // FillChar(Position, SizeOf(Position), 0);
                        Position := Default(THABPosition);
                        SyncCallback(SourceID, False, 'Connecting to ' + HostOrIP + '...', Position);
                        AClient.Host := HostOrIP;
                        AClient.Port := Port;
                        AClient.Connect;
                        SyncCallback(SourceID, True, 'Connected to ' + HostOrIP, Position);

                        SetGroupChangedFlag(GroupName, True);

                        // while not GetGroupChangedFlag(GroupName) do begin
                        while (not Terminated) and
                              (not NeedToReconnect) and
                              (Host = GetSettingString(GroupName, 'Host', '')) and
                              (Port = GetSettingInteger(GroupName, 'Port', 0)) and
                              GetSettingBoolean(GroupName, 'Enabled', True) do begin

                            if GetGroupChangedFlag(GroupName) then begin
                                InitialiseDevice;
                                SetGroupChangedFlag(GroupName, False);
                            end;

                            while Commands.Count > 0 do begin
                                AClient.IOHandler.WriteLn(Commands[0]);
                                Commands.Delete(0);
                            end;

                            Line := AClient.IOHandler.ReadLn;
                            if Line <> '' then begin
                                try
                                    Position := ExtractPositionFrom(Line);
                                    if Position.InUse or Position.HasPacketRSSI or Position.HasCurrentRSSI then begin
                                        SyncCallback(SourceID, True, '', Position);
                                    end;
                                except

                                end;
                            end;
                        end;

                        AClient.IOHandler.InputBuffer.clear;
                        AClient.IOHandler.CloseGracefully;
                        AClient.Disconnect;
                    except
                        // Wait before retrying
                        SyncCallback(SourceID, False, 'No Connection to ' + HostOrIP, Position);
                        Sleep(5000);
                    end;
                end;
            end;
        end else begin
            Position := Default(THABPosition);
            if GetSettingBoolean(GroupName, 'Enabled', True) then begin
                SyncCallback(SourceID, False, 'Need Source Filter', Position);
            end else begin
                SyncCallback(SourceID, False, 'Source disabled', Position);
            end;
            Sleep(1000);
        end;
    end;
end;

constructor TSocketSource.Create(ID: Integer; Group: String; Callback: TSourcePositionCallback);
begin
    inherited Create(ID, Group, Callback);
end;

procedure TSocketSource.InitialiseDevice;
begin
    // virtual;
end;

end.
