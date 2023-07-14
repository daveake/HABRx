unit MQTTUplink;

interface

uses Classes, SysUtils, SyncObjs, Miscellaneous, TMS.MQTT.Global, TMS.MQTT.Client;

type
    TMQTTThread = class(TThread)
private
    CritSection: TCriticalSection;
    TopicTemplate, TelemetryTopic, TelemetryMessage, CarTopic, CarMessage, CarTopicTemplate: String;
    SendingCar: Boolean;
    StatusCallback: TStatusCallback;
    TMSMQTTClient1: TTMSMQTTClient;
    procedure SyncCallback(SourceID: Integer; Active, OK: Boolean; Status: String);
    procedure ConnectedStatusChanged(ASender: TObject; const AConnected: Boolean; AStatus: TTMSMQTTConnectionStatus);
  public
    procedure SendTelemetry(PayloadID, Sentence: String);
    procedure SendChase(Callsign, Position: String);
    procedure SetMQTTDetails(Server, UserName, Password, Topic, CarTopic: String);
    procedure Execute; override;
  published
    constructor Create(Callback: TStatusCallback);
end;

implementation

procedure TMQTTThread.SetMQTTDetails(Server, UserName, Password, Topic, CarTopic: String);
begin
    TopicTemplate := Topic;
    CarTopicTemplate := CarTopic;

    TMSMQTTClient1.BrokerHostName := Server;
    TMSMQTTClient1.BrokerPort := 1883;  // 8883 for SSL
    TMSMQTTClient1.UseSSL := False;
    TMSMQTTClient1.Credentials.Username := UserName;
    TMSMQTTClient1.Credentials.Password := Password;
end;

procedure TMQTTThread.SendTelemetry(PayloadID, Sentence: String);
begin
    CritSection.Enter;
    try
        TelemetryTopic := StringReplace(TopicTemplate, '$PAYLOAD$', PayloadID, [rfReplaceAll, rfIgnoreCase]);
        TelemetryMessage := Sentence;
    finally
        CritSection.Leave;
    end;
end;

procedure TMQTTThread.SendChase(Callsign, Position: String);
begin
    CritSection.Enter;
    try
        TelemetryTopic := StringReplace(CarTopicTemplate, '$CALLSIGN$', Callsign, [rfReplaceAll, rfIgnoreCase]);
        TelemetryMessage := Position;
    finally
        CritSection.Leave;
    end;
end;

procedure TMQTTThread.Execute;
var
    Msg, Topic: String;
    SentOK: Boolean;
begin
    TMSMQTTClient1.BrokerPort := 1883;  // 8883 for SSL
    TMSMQTTClient1.UseSSL := False;
    TMSMQTTClient1.OnConnectedStatusChanged := ConnectedStatusChanged;

    while not Terminated do begin
        if TMSMQTTClient1.BrokerHostName = '' then begin
            SyncCallback(0, False, True, 'Missing broker name');
        end else begin
            if not TMSMQTTClient1.IsConnected then begin
                try
                    TMSMQTTClient1.Connect;
                finally

                end;
            end;

            if TMSMQTTClient1.IsConnected then begin
                // Telemetry
                Msg := '';
                CritSection.Enter;
                try
                    if CarMessage <> '' then begin
                        Topic := CarTopic;
                        Msg := CarMessage;
                        SendingCar := True;
                    end else begin
                        Topic := TelemetryTopic;
                        Msg := TelemetryMessage;
                        SendingCar := False;
                    end;
                finally
                    CritSection.Leave;
                end;

                if (Msg <> '') and (Topic <> '') then begin
                    try
                        TMSMQTTClient1.Publish(Topic, Msg);
        //                AClient.IOHandler.CloseGracefully;
        //                AClient.Disconnect;
                        SentOK := True;
                        SyncCallback(0, True, SentOK, 'Published OK to broker');
                    except
                        SentOK := False;
                        SyncCallback(0, True, SentOK, 'Failed to publish');
                        // AClient.Disconnect;
                    end;

                    if SentOK then begin
                        CritSection.Enter;
                        try
                            if SendingCar then begin
                                CarMessage := '';
                            end else begin
                                TelemetryMessage := '';
                            end;
                        finally
                            CritSection.Leave;
                        end;
                    end;
                end;
            end;
        end;

        sleep(500);
    end;

    CritSection.Free;
    TMSMQTTClient1.TimeOutSettings.Free;
    TMSMQTTClient1.Free;
end;

procedure TMQTTThread.ConnectedStatusChanged(ASender: TObject;
  const AConnected: Boolean; AStatus: TTMSMQTTConnectionStatus);
var
    Position: THABPosition;
begin
    Position := Default(THABPosition);

    if AConnected then begin
        SyncCallback(0, False, True, 'Connected to broker');
    end else begin
        SyncCallback(0, False, False, 'Disconnected from broker');
    end;
end;


constructor TMQTTThread.Create(Callback: TStatusCallback);
begin
    CritSection := TCriticalSection.Create;

    TMSMQTTClient1 := TTMSMQTTClient.Create(nil);

    StatusCallback := Callback;

    inherited Create(False);
end;

procedure TMQTTThread.SyncCallback(SourceID: Integer; Active, OK: Boolean; Status: String);
begin
    Synchronize(
        procedure begin
            StatusCallback(SourceID, Active, OK, Status);
        end
    );
end;

end.
