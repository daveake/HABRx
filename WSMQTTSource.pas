unit WSMQTTSource;

interface

uses Source, Classes, DateUtils, SysUtils, Miscellaneous,
     sgcWebSocket_Classes, sgcWebSocket_Protocol_Base_Client,
     sgcWebSocket_Protocol_MQTT_Client, sgcWebSocket_Protocols, sgcBase_Classes,
     sgcTCP_Classes, sgcWebSocket_Classes_Indy, sgcWebSocket_Client, sgcWebSocket,
     sgcWebSocket_Protocol_MQTT_Message, sgcSocket_Classes, sgcWebSocket_Types;

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
    procedure WSClientException(Connection: TsgcWSConnection;
      E: Exception);
    procedure MQTTMQTTSubscribe(Connection: TsgcWSConnection;
      aPacketIdentifier: Word; aCodes: TsgcWSSUBACKS;
      SubscribeProperties: TsgcWSMQTTSUBACKProperties);
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

constructor TWSMQTTSource.Create(ID: Integer; Group: String; Callback: TSourcePositionCallback);
begin
    inherited Create(ID, Group, Callback);

    // Create clients
    WSClient := TsgcWebSocketClient.Create(nil);
    WsClient.OnException := WSClientException;

    MQTTClient := TsgcWSPClient_MQTT.Create(nil);
    MQTTClient.Client := WSClient;
end;

destructor TWSMQTTSource.Destroy;
begin
    WSClient.Free;
    MQTTClient.Free;

    inherited;
end;

procedure TWSMQTTSource.Execute;
var
    Host, Port, UserName, Password: String;
    Position: THABPosition;
begin
    inherited;

{$IFDEF ANDROID}
    WSClient.TLSOptions.OpenSSL_Options.LibPath := oslpDefaultFolder;
{$ENDIF}

    MQTTClient.HeartBeat.Interval := 30;            // Needed to stop server from disconnecting after 1 minute
    MQTTClient.HeartBeat.Enabled := True;

    MQTTClient.OnMQTTConnect := MQTTMQTTConnect;
    MQTTClient.OnMQTTDisconnect := MQTTMQTTDisconnect;
    MQTTClient.OnMQTTPublish := MQTTMQTTPublish;
    MQTTClient.OnMQTTSubscribe := MQTTMQTTSubscribe;

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
                            WSClient.TLSOptions.Version := tls1_0;

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

procedure TWSMQTTSource.WSClientException(Connection: TsgcWSConnection;
  E: Exception);
var
    Position: THABPosition;
begin
    Position := Default(THABPosition);

    SyncCallback(SourceID, False, 'WS Exception: ' + E.Message, Position);
end;


procedure TWSMQTTSource.MQTTMQTTSubscribe(Connection: TsgcWSConnection;
  aPacketIdentifier: Word; aCodes: TsgcWSSUBACKS;
  SubscribeProperties: TsgcWSMQTTSUBACKProperties);
var
    Position: THABPosition;
begin
    Position := Default(THABPosition);

    SyncCallback(SourceID, True, 'Subscribed to ' + Topic + #10 + WhiteList + #10 + ExtraPayloads, Position);
end;

end.
