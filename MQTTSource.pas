 unit MQTTSource;

interface

uses Source, Classes, DateUtils, SysUtils, Miscellaneous, TMS.MQTT.Global, TMS.MQTT.Client;

type
  TMQTTSource = class(TSource)
  private
    { Private declarations }
    MQTTClient: TTMSMQTTClient;
    Filtered: Boolean;
    Topic, WhiteList, ExtraPayloads: String;
    procedure ConnectedStatusChanged(ASender: TObject; const AConnected: Boolean; AStatus: TTMSMQTTConnectionStatus);
    procedure PublishReceived(ASender: TObject; APacketID: Word; ATopic: string; APayload: TArray<System.Byte>);
    procedure SubscriptionAcknowledged(ASender: TObject; APacketID: Word; ASubscriptions: TTMSMQTTSubscriptions);
  protected
    { Protected declarations }
    procedure Execute; override;
  public
    { Public declarations }
  public
    constructor Create(ID: Integer; Group: String; Callback: TSourcePositionCallback);
    destructor Destroy; override;
  end;

implementation

constructor TMQTTSource.Create(ID: Integer; Group: String; Callback: TSourcePositionCallback);
begin
    // Create client
    MQTTClient := TTMSMQTTClient.Create(nil);

    inherited Create(ID, Group, Callback);
end;

destructor TMQTTSource.Destroy;
begin
    MQTTClient.TimeOutSettings.Free;
    MQTTClient.Free;

    inherited;
end;

procedure TMQTTSource.Execute;
var
    Host, Port, UserName, Password: String;
    Position: THABPosition;
begin
    inherited;

    MQTTClient.UseSSL := False;
    MQTTClient.OnConnectedStatusChanged := ConnectedStatusChanged;
    MQTTClient.OnSubscriptionAcknowledged := SubscriptionAcknowledged;
    MQTTClient.OnPublishReceived := PublishReceived;

    Position := Default(THABPosition);

    while not Terminated do begin
        if GetSettingBoolean(GroupName, 'Enabled', True) and GotFilterIfNeeded then begin
            // Connect to socket server
            Host := GetSettingString(GroupName, 'Host', '');
            Port := GetSettingString(GroupName, 'Port', '');
            Topic := GetSettingString(GroupName, 'Topic', '');
            UserName := GetSettingString(GroupName, 'UserName', '');
            Password := GetSettingString(GroupName, 'Password', '');
            WhiteList := GetSettingString(GroupName, 'WhiteList', '');
            ExtraPayloads := GetSettingString(GroupName, 'ExtraPayloads', '');
            Filtered := GetSettingBoolean(GroupName, 'Filtered', False);
            // SetGroupChangedFlag(GroupName, False);

            if (Host = '') or (Topic = '') then begin
                Position := Default(THABPosition);
                SyncCallback(SourceID, True, 'Source not configured', Position);
                Sleep(1000);
            end else begin
                if MQTTClient.IsConnected then begin
                    MQTTClient.Disconnect;
                end;

                while (not Terminated) and
                      (not NeedToReconnect) and
                      (Host = GetSettingString(GroupName, 'Host', '')) and
                      (Port = GetSettingString(GroupName, 'Port', '')) and
                      (Topic = GetSettingString(GroupName, 'Topic', '')) and
                      (WhiteList = GetSettingString(GroupName, 'WhiteList', '')) and
                      (ExtraPayloads = GetSettingString(GroupName, 'ExtraPayloads', '')) and
                      (Filtered = GetSettingBoolean(GroupName, 'Filtered', False)) and
                      (UserName = GetSettingString(GroupName, 'UserName', '')) and
                      (Password = GetSettingString(GroupName, 'Password', '')) and
                      GetSettingBoolean(GroupName, 'Enabled', True) do begin

                    if MQTTClient.IsConnected then begin
                        Sleep(1000);
                    end else begin
                        try
                            MQTTClient.BrokerHostName := Host;
                            MQTTClient.BrokerPort := StrToIntDef(Port, 1883);
                            MQTTClient.Credentials.Username := UserName;
                            MQTTClient.Credentials.Password := Password;

                            SyncCallback(SourceID, False, 'Connecting to ' + Host + ':' + IntToStr(MQTTClient.BrokerPort) + '...', Position);

                            MQTTClient.Connect;
                        except
                            SyncCallback(SourceID, False, 'No Connection to ' + Host + ':' + IntToStr(MQTTClient.BrokerPort) + '...', Position)
                        end;

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

procedure TMQTTSource.ConnectedStatusChanged(ASender: TObject;
  const AConnected: Boolean; AStatus: TTMSMQTTConnectionStatus);
var
    Position: THABPosition;
    Payloads, PayloadTopic, PayloadID: String;
begin
    Position := Default(THABPosition);

    if AConnected then begin
        if Filtered then begin
            if (WhiteList + ExtraPayloads) = '' then begin
                SyncCallback(SourceID, False, 'No payloads to subscribe to ', Position);
            end else begin
                Payloads := WhiteList + ',' + ExtraPayloads;
                SyncCallback(SourceID, False, 'Subscribing to ' + Topic + Payloads, Position);

                repeat
                    PayloadID := GetString(Payloads, ',');
                    if PayloadID <> '' then begin
                        PayloadTopic := Topic + PayloadID + '/#';
                        MQTTClient.Subscribe(PayloadTopic);
                    end;
                until Payloads = '';
             end;
        end else begin
            SyncCallback(SourceID, False, 'Subscribing to ' + Topic, Position);
            MQTTClient.Subscribe(Topic);
        end;
    end else begin
        case AStatus of
            csConnecting:       SyncCallback(SourceID, False, 'Connecting to broker', Position);
            csConnectionLost:   SyncCallback(SourceID, False, 'Lost connection to broker', Position);
            csReconnecting:     SyncCallback(SourceID, False, 'Reconnecting to broker', Position);
            else                SyncCallback(SourceID, False, 'Not Connected to broker', Position);
        end;
    end;
end;

procedure TMQTTSource.PublishReceived(ASender: TObject;
  APacketID: Word; ATopic: string; APayload: TArray<System.Byte>);
begin
    ProcessMQTTMessage(ATopic, TEncoding.UTF8.GetString(APayload));
end;

procedure TMQTTSource.SubscriptionAcknowledged(ASender: TObject; APacketID: Word; ASubscriptions: TTMSMQTTSubscriptions);
var
    Position: THABPosition;
begin
    Position := Default(THABPosition);

    // SyncCallback(SourceID, True, 'Subscribed to ' + Topic + #10 + WhiteList + #10 + ExtraPayloads, Position);
    SyncCallback(SourceID, True, 'Subscribed to ' + Topic + WhiteList + ' ' + ExtraPayloads, Position);
end;


end.
