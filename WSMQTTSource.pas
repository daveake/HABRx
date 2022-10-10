unit WSMQTTSource;

interface

uses Source, Classes, DateUtils, SysUtils, Miscellaneous,
     sgcWebSocket_Classes, sgcWebSocket_Protocol_Base_Client,
     sgcWebSocket_Protocol_MQTT_Client, sgcWebSocket_Protocols, sgcBase_Classes,
     sgcTCP_Classes, sgcWebSocket_Classes_Indy, sgcWebSocket_Client, sgcWebSocket,
     sgcWebSocket_Protocol_MQTT_Message, sgcSocket_Classes;

type
  TWSMQTTSource = class(TSource)
  private
    { Private declarations }
    WSClient: TsgcWebSocketClient;
    MQTTClient: TsgcWSPClient_MQTT;
    Filtered: Boolean;
    Topic, WhiteList, ExtraPayloads: String;
    procedure MQTTMQTTConnect(Connection: TsgcWSConnection;
      const Session: Boolean; const ReasonCode: Integer;
      const ReasonName: string;
      const ConnectProperties: TsgcWSMQTTCONNACKProperties);
    procedure MQTTMQTTDisconnect(Connection: TsgcWSConnection;
      ReasonCode: Integer; const ReasonName: string;
      DisconnectProperties: TsgcWSMQTTDISCONNECTProperties);
    procedure MQTTMQTTPublish(Connection: TsgcWSConnection; aTopic,
      aText: string; PublishProperties: TsgcWSMQTTPublishProperties);
  protected
    { Protected declarations }
    procedure Execute; override;
  public
    { Public declarations }
  public
    constructor Create(ID: Integer; Group: String; Callback: TSourcePositionCallback);
  end;

implementation

procedure TWSMQTTSource.Execute;
var
    Host, Port, UserName, Password: String;
    Position: THABPosition;
begin
    inherited;

    // Create clients
    WSClient := TsgcWebSocketClient.Create(nil);

    MQTTClient := TsgcWSPClient_MQTT.Create(nil);
    MQTTClient.Client := WSClient;

    MQTTClient.OnMQTTConnect := MQTTMQTTConnect;
    MQTTClient.OnMQTTDisconnect := MQTTMQTTDisconnect;
    MQTTClient.OnMQTTPublish := MQTTMQTTPublish;

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
                if WSClient.Active then begin
                    WSClient.Active := False;
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

                    if WSClient.Active then begin
                        Sleep(1000);
                    end else begin
                        try
                            WSClient.TLS := True;
                            WSClient.Host := Host;
                            WSClient.Port := StrToIntDef(Port, 1883);

                            if (UserName <> '') and (Password <> '') then begin
                                MQTTClient.Authentication.Username := UserName;
                                MQTTClient.Authentication.Password := Password;
                                MQTTClient.Authentication.Enabled := True;
                            end else begin
                                MQTTClient.Authentication.Enabled := False;
                            end;

                            SyncCallback(SourceID, False, 'Connecting to ' + Host + ':' + IntToStr(WSClient.Port) + '...', Position);

                            WSClient.Active := True;
                        except
                            SyncCallback(SourceID, False, 'No Connection to ' + Host + ':' + IntToStr(WSClient.Port) + '...', Position)
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

procedure TWSMQTTSource.MQTTMQTTConnect(Connection: TsgcWSConnection;  const Session: Boolean; const ReasonCode: Integer; const ReasonName: string; const ConnectProperties: TsgcWSMQTTCONNACKProperties);
var
    Position: THABPosition;
    Payloads, PayloadTopic, PayloadID: String;
begin
    Position := Default(THABPosition);

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
end;

procedure TWSMQTTSource.MQTTMQTTDisconnect(Connection: TsgcWSConnection;
  ReasonCode: Integer; const ReasonName: string;
  DisconnectProperties: TsgcWSMQTTDISCONNECTProperties);
var
    Position: THABPosition;
begin
    Position := Default(THABPosition);

    SyncCallback(SourceID, False, 'Disconnected from broker', Position);
end;

procedure TWSMQTTSource.MQTTMQTTPublish(Connection: TsgcWSConnection; aTopic,
  aText: string; PublishProperties: TsgcWSMQTTPublishProperties);
begin
    ProcessMQTTMessage(aTopic, aText);
end;

(*
procedure TWSMQTTSource.ConnectedStatusChanged(ASender: TObject;
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

procedure TWSMQTTSource.PublishReceived(ASender: TObject;
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

procedure TWSMQTTSource.SubscriptionAcknowledged(ASender: TObject; APacketID: Word; ASubscriptions: TTMSMQTTSubscriptions);
var
    Position: THABPosition;
begin
    Position := Default(THABPosition);

    SyncCallback(SourceID, True, 'Subscribed to ' + Topic + #10 + WhiteList + #10 + ExtraPayloads, Position);
end;
*)

constructor TWSMQTTSource.Create(ID: Integer; Group: String; Callback: TSourcePositionCallback);
begin
    inherited Create(ID, Group, Callback);
end;

end.
