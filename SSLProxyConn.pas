unit SSLProxyConn;

interface

uses
  blcksock,
  synsock,
  ssl_openssl,
  SysUtils,
  Classes,
  uSynapseTCPServer, uHsCriticalSection;
type
  THssClientPeer = TSynapseTCPServerPeer;
  TSSLProxyConn = class(TObject)

  private
    SSLServer:TSynapseTCPServer;
    FSSLPort: Integer;
    FPort: Integer;
    FHost: string;
    FCS:THsCriticalSection;
    FLog: string;

    procedure SetSSLPort(const Value: Integer);
    procedure TCPServerConnect(AThread:THssClientPeer );
    procedure TCPServerDisconnect(AThread:THssClientPeer);
    procedure TCPServerExecute(AThread: THssClientPeer );
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: Integer);
    function GetLog: string;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
  public
    procedure AddLog(s:string);
    procedure Stop;
    procedure StartSSLServer;
    constructor Create;
    destructor Destroy; override;
  published

    property Log: string read GetLog;

    property SSLPort:Integer read FSSLPort write SetSSLPort;

    property Host: string read FHost write SetHost;
    property Port: Integer read FPort write SetPort;
    property Active: Boolean read GetActive write SetActive;
  end;
implementation

uses
  uHssSetupFile ;

{ TSSLProxyConn }
procedure TSSLProxyConn.AddLog(s: string);
begin
  FCS.Acquire('');
  try
    if Length(FLog) > 4000 then
      FLog := '';
    FLog := s + #13#10 + FLog;
  
  finally
    FCS.Release;
  end;
end;

constructor TSSLProxyConn.Create;
begin
  FCs := THsCriticalSection.Create;
end;

destructor TSSLProxyConn.Destroy;
begin
  Stop;
  FCS.Free;
  inherited;
end;

function TSSLProxyConn.GetActive: Boolean;
begin
  Result := Assigned(SSLServer);
  if Result and not SSLServer.Active then
  begin
    Stop;
    Result := False;
  end;
end;

function TSSLProxyConn.GetLog: string;
begin
  FCS.Acquire('');
  try
    Result := FLog;
  finally
    FCS.Release;

  end;
end;

procedure TSSLProxyConn.SetActive(const Value: Boolean);
begin
  if Value <> GetActive then
  begin
    if Value then

      StartSSLServer else
      Stop;
  end;

end;

procedure TSSLProxyConn.SetHost(const Value: string);
begin
  FHost := Value;
end;

procedure TSSLProxyConn.SetPort(const Value: Integer);
begin
  FPort := Value;
end;

procedure TSSLProxyConn.SetSSLPort(const Value: Integer);
begin
  FSSLPort := Value;
end;

procedure TSSLProxyConn.StartSSLServer;
begin
try
    SSLServer := TSynapseTCPServer.Create(IntToStr(FSSLPort),True,50,50);
    SSLServer.OnConnect := TCPServerConnect;
    SSLServer.OnExecute := TCPServerExecute;
    SSLServer.OnDisconnect := TCPServerDisconnect;
    SSLServer.ThreadClass := TSynapseTCPServerThread;
    SSLServer.ServerName := 'SSL';

      if SSL_CERT_ROOT <> '' then
      begin
        SSLServer.fSock.SSL.SSLType := LT_TLSv1_2;
        SSLServer.fSock.SSL.CertCAFile := SSL_CERT_ROOT;
        SSLServer.fSock.SSL.CertificateFile := SSL_CERT;
        SSLServer.fSock.SSL.PrivateKeyfILE := SSL_Key;
      end;
    SSLServer.Resume;
    AddLog('Server Listening');
except
  on e:Exception do
    AddLog(e.Message);


end;

end;

procedure TSSLProxyConn.Stop;
begin
  FreeAndNil(SSlServer);
  AddLog('Stopped');
end;

procedure TSSLProxyConn.TCPServerConnect(AThread: THssClientPeer);
var
  Client: TTCPBlockSocket;
begin
  AddLog('Connection Made');
  try
  Client := TTCPBlockSocket.Create;
  Client.Connect(Host, IntToStr(Port));
  AThread.Data := Client;
  AThread.FreeData := True;
  AddLog('Client Connected');
  except

    on e:exception do
    begin
      AddLog(e.Message);      raise;
    end;

  end;
end;

procedure TSSLProxyConn.TCPServerDisconnect(AThread: THssClientPeer);
begin
  AddLog('Disconnected Client');
end;

procedure TSSLProxyConn.TCPServerExecute(AThread: THssClientPeer);
var
  sPacket,sData: string;
  sReply: string;
  Client: TTCPBlockSocket;
  PutDataSize: Integer;
  PutData: TMemoryStream;
  Source,Target:TTcpBlockSocket;
  OriginalHost:string;

  procedure ProcessSource;
  begin
    // As a pass through ssl proxy, we need to understand and process the html correctly
    // and alter the host header to ensure that IIS Express and others dont block requests
    // coming from an outside source.
    PutDataSize := 0;
    sData := '';
    // First read the HTTP Header.
    // Http Header are standard #13#10 lines
    // End of header is determined by finding an Empty Line;
    repeat
      sPacket := Source.RecvString(-1);

      if sPacket <> '' then
      begin
        if Sametext( copy(sPacket,1,15),'content-length:') then
        begin
          // all PUT/POST commands will have content, which has length specified
          // we must read the exact correct amount and no more for efficent processing and
          // knowing the end of the message.
          PutDataSize := StrToInt(trim(copy(sPacket,16,100)) );
        end else
        if pos('Host:', spacket) = 1 then
        begin
          // we must alter the host, so that the reciever believes they were the original target
          // of the HTTP request, or it will be refused.
          sPacket := OriginalHost;
          OriginalHost := sPacket;
        end;
      end;
      // add header line to data, including the end-of-line empty line
      sData := sData + sPacket + #13#10;

    until sPAcket = '';
    PutData := nil;

    // Send the HTTP Header to the target
    Target.SendString(sData);
    AddLog(sData);
    if PutDataSize <> 0 then
    begin
     PutData := TMemoryStream.Create;
     try
       PutData.Size := PutDataSize;
       PutData.Position := 0;

       Source.RecvBufferEx(PutData.Memory,PutDataSize,-1);
       PutData.Position := 0;
       Target.SendStreamRaw(PutData);
     finally
       FreeAndNil(PutData);
     end;
    end;

  end;
begin
  try
    // Set the Original Host to the name of the Target Http Site
    // Typically this is intended as an IIS-Express LocalHost
    // and the proxy is running on the same machine so that it can access
    // the test server in LocalHost:Http and allow the rest of the network to
    // access SSL on any port and host designation such as Lan IP
    OriginalHost :=  'Host: '+Host+':'+IntToStr(Port);
    Source := aThread.Sock;
    Target := aThread.Data as TTCPBlockSocket;
    ProcessSource;

    // Switch Source and target to return the traffic back to the requester.
    Source := Target;
    Target := aThread.Sock;
    ProcessSource;
  except
    on e:Exception do
    begin
      AddLog('Error:'+e.Message);
      // All errrors must be raised to disconnect correctly.
      // its normal and expected for a peer disconnect.
      raise;
    end;
  end;
end;

end.
