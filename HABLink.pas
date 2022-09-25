unit HABLink;

interface

uses Classes, SysUtils, SyncObjs, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, Miscellaneous;

type
    THABLinkThread = class(TThread)
private
    CritSection: TCriticalSection;
    HablinkMessage, HablinkListener, ChasePosition: String;
    StatusCallback: TStatusCallback;
    ListenerSentOK: Boolean;
    procedure SyncCallback(SourceID: Integer; Active, OK: Boolean);
    procedure ClientConnected(Sender: TObject);
    procedure ClientDisconnected(Sender: TObject);
  public
    procedure SetListener(Device, Version, Callsign: String);
    procedure SendTelemetry(Sentence: String);
    procedure SendChasePosition(Position: String);
    procedure Execute; override;
  published
    constructor Create(Callback: TStatusCallback);
end;

implementation

procedure THABLinkThread.SendTelemetry(Sentence: String);
begin
    CritSection.Enter;
    try
        HablinkMessage := 'POSITION:SENTENCE=' + Sentence;
    finally
        CritSection.Leave;
    end;
end;

procedure THABLinkThread.SetListener(Device, Version, Callsign: String);
begin
    CritSection.Enter;
    try
        HablinkListener := 'LISTENER:TYPE=' + Device + ',VERSION=' + Version + ',CALLSIGN=' + Callsign;
        ListenerSentOK := False;
    finally
        CritSection.Leave;
    end;
end;

procedure THABLinkThread.SendChasePosition(Position: String);
begin
    CritSection.Enter;
    try
        ChasePosition := 'CHASE:POSITION=' + Position;
    finally
        CritSection.Leave;
    end;
end;


procedure THABLinkThread.Execute;
var
    Msg: String;
    AClient: TIdTCPClient;
    SendingListener, SendingChase, SentOK: Boolean;
begin
    AClient := TIdTCPClient.Create;
    AClient.OnConnected := ClientConnected;
    AClient.OnDisconnected := ClientDisconnected;

    while not Terminated do begin
        if not AClient.Connected then begin
            ListenerSentOK := False;
            // AClient.Host := 'hab.link';
            AClient.Host := 'hab.link';
            AClient.Port := 8887;
            try
                AClient.Connect;
                SyncCallback(0, False, True);
            except
                SyncCallback(0, False, False);
            end;
        end;

        if AClient.Connected then begin
            // Telemetry
            Msg := '';
            SendingListener := False;
            SendingChase := False;
            CritSection.Enter;
            try
                if (not ListenerSentOK) and (HablinkListener <> '') then begin
                    Msg := HablinkListener;
                    SendingListener := True;
                end else if ChasePosition <> '' then begin
                    Msg := ChasePosition;
                    SendingChase := True;
                end else begin
                    Msg := HablinkMessage;
                end;
            finally
                CritSection.Leave;
            end;

            if Msg = '' then begin
//                try
//                    AClient.IOHandler.ReadLn;
//                except
//                    AClient.Disconnect;
//                end;
            end else begin
                SentOK := False;

                try
                    AClient.IOHandler.WriteLn(Msg);
    //                AClient.IOHandler.CloseGracefully;
    //                AClient.Disconnect;
                    SentOK := True;
                except
                    SentOK := False;
                    AClient.Disconnect;
                end;

                if not SendingListener then begin
                    SyncCallback(0, True, SentOK);
                end;

                if SentOK then begin
                    CritSection.Enter;
                    try
                        if SendingListener then begin
                            ListenerSentOK := True;
                        end else if SendingChase then begin
                            ChasePosition := '';
                        end else begin
                            HablinkMessage := '';
                        end;
                    finally
                        CritSection.Leave;
                    end;
                end;
            end;
        end;

        sleep(100);
    end;
end;

procedure THABLinkThread.ClientConnected(Sender: TObject);
begin
    SyncCallback(0, False, True);
end;

procedure THABLinkThread.ClientDisconnected(Sender: TObject);
begin
    SyncCallback(0, False, False);
end;


constructor THABLinkThread.Create(Callback: TStatusCallback);
begin
    CritSection := TCriticalSection.Create;

    StatusCallback := Callback;

    inherited Create(False);
end;

procedure THABLinkThread.SyncCallback(SourceID: Integer; Active, OK: Boolean);
begin
    Synchronize(
        procedure begin
            StatusCallback(SourceID, Active, OK, '');
        end
    );
end;

end.
