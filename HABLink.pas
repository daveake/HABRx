unit HABLink;

interface

uses Classes, SysUtils, SyncObjs, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, Miscellaneous;

type
    THABLinkThread = class(TThread)
private
    CritSection: TCriticalSection;
    HablinkMessage, HablinkListener: String;
    StatusCallback: TStatusCallback;
    ListenerSentOK: Boolean;
    // procedure SyncCallback(SourceID: Integer; Active, OK: Boolean);
  public
    procedure SetListener(Device, Version, Callsign: String);
    procedure SendTelemetry(Sentence: String);
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

procedure THABLinkThread.Execute;
var
    Msg: String;
    AClient: TIdTCPClient;
    SendingListener, SentOK: Boolean;
begin
    AClient := TIdTCPClient.Create;

    while not Terminated do begin
        if not AClient.Connected then begin
            ListenerSentOK := False;
            // AClient.Host := 'hab.link';
            AClient.Host := '192.168.1.175';
            AClient.Port := 8887;
            AClient.Connect;
        end;

        if AClient.Connected then begin
            // Telemetry
            Msg := '';
            CritSection.Enter;
            try
                if (not ListenerSentOK) and (HablinkListener <> '') then begin
                    Msg := HablinkListener;
                    SendingListener := True;
                end else begin
                    Msg := HablinkMessage;
                    SendingListener := False;
                end;
            finally
                CritSection.Leave;
            end;

            if Msg <> '' then begin
                SentOK := False;

                try
                    AClient.IOHandler.WriteLn(Msg);
    //                AClient.IOHandler.CloseGracefully;
    //                AClient.Disconnect;
                    SentOK := True;
                except
                    SentOK := False;
                end;

                if SentOK then begin
                    CritSection.Enter;
                    try
                        if SendingListener then begin
                            ListenerSentOK := True;
                        end else begin
                            HablinkMessage := '';;
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

constructor THABLinkThread.Create(Callback: TStatusCallback);
begin
    CritSection := TCriticalSection.Create;

    StatusCallback := Callback;

    inherited Create(False);
end;

//procedure THABLinkThread.SyncCallback(SourceID: Integer; Active, OK: Boolean);
//begin
//    Synchronize(
//        procedure begin
//            StatusCallback(SourceID, Active, OK);
//        end
//    );
//end;

end.
