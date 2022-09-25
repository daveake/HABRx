unit MQTTSource;

interface

uses Source, Classes, DateUtils, SysUtils, Miscellaneous, TMS.MQTT.Global, TMS.MQTT.Client, System.JSON;

type
  TMQTTSource = class(TSource)
  private
    { Private declarations }
    MQTTClient: TTMSMQTTClient;
    Topic: String;
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
  end;

implementation

procedure TMQTTSource.Execute;
var
    Host, UserName, Password, Line: String;
    Position: THABPosition;
begin
    inherited;

    // Create client
    MQTTClient := TTMSMQTTClient.Create(nil);
    MQTTClient.BrokerPort := 1883;  // 8883 for SSL
    MQTTClient.UseSSL := False;
    MQTTClient.OnConnectedStatusChanged := ConnectedStatusChanged;
    MQTTClient.OnSubscriptionAcknowledged := SubscriptionAcknowledged;
    MQTTClient.OnPublishReceived := PublishReceived;

    Position := Default(THABPosition);

    while not Terminated do begin
        if GetSettingBoolean(GroupName, 'Enabled', True) and GotFilterIfNeeded then begin
            // Connect to socket server
            Host := GetSettingString(GroupName, 'Host', '');
            Topic := GetSettingString(GroupName, 'Topic', '');
            UserName := GetSettingString(GroupName, 'UserName', '');
            Password := GetSettingString(GroupName, 'Password', '');
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
                      (Topic = GetSettingString(GroupName, 'Topic', '')) and
                      (UserName = GetSettingString(GroupName, 'UserName', '')) and
                      (Password = GetSettingString(GroupName, 'Password', '')) and
                      GetSettingBoolean(GroupName, 'Enabled', True) do begin

                    if MQTTClient.IsConnected then begin
                        Sleep(1000);
                    end else begin
                        try
                            SyncCallback(SourceID, False, 'Connecting to ' + Host + '...', Position);

                            MQTTClient.BrokerHostName := Host;
                            MQTTClient.Credentials.Username := UserName;
                            MQTTClient.Credentials.Password := Password;

                            MQTTClient.Connect;
                        except
                            SyncCallback(SourceID, False, 'No Connection to ' + Host, Position);
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
begin
    Position := Default(THABPosition);

    if AConnected then begin
        SyncCallback(SourceID, False, 'Subscribing to ' + Topic, Position);
        MQTTClient.Subscribe(Topic);
    end else begin
        SyncCallback(SourceID, False, 'Not Connected to broker', Position);
    end;
end;

procedure TMQTTSource.PublishReceived(ASender: TObject;
  APacketID: Word; ATopic: string; APayload: TArray<System.Byte>);
var
    Value: AnsiString;
    Position: THABPosition;
    TimeStamp, Sentence: String;
    JSONValue: TJSONValue;
begin
    Position := Default(THABPosition);

    Value := TEncoding.UTF8.GetString(APayload);

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

procedure TMQTTSource.SubscriptionAcknowledged(ASender: TObject; APacketID: Word; ASubscriptions: TTMSMQTTSubscriptions);
var
    Position: THABPosition;
begin
    Position := Default(THABPosition);

    SyncCallback(SourceID, True, 'Subscribed to ' + Topic, Position);
end;

constructor TMQTTSource.Create(ID: Integer; Group: String; Callback: TSourcePositionCallback);
begin
    inherited Create(ID, Group, Callback);
end;

end.
