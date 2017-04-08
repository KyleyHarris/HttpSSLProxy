unit SSLProxyConn;

interface

uses
  blcksock,
  synsock,
  ssl_openssl,
  SysUtils,
  Classes,
  uSynapseTCPServer,
  Contnrs,
  SyncObjs;
  
type
  THssClientPeer = TSynapseTCPServerPeer;
  THttpPacket = class
  private
    FHeaderReply: string;
    FBodyRecv: string;
    FHeaderRecv: string;
    FBodyReply: string;
    FTimeReply: TDateTime;
    FTimeRecv: TDateTime;
    procedure SetBodyRecv(const Value: string);
    procedure SetBodyReply(const Value: string);
    procedure SetHeaderRecv(const Value: string);
    procedure SetHeaderReply(const Value: string);
    procedure SetTimeRecv(const Value: TDateTime);
    procedure SetTimeReply(const Value: TDateTime);
  published
  public
    property TimeRecv: TDateTime read FTimeRecv write SetTimeRecv;
    property TimeReply: TDateTime read FTimeReply write SetTimeReply;
    property HeaderRecv: string read FHeaderRecv write SetHeaderRecv;
    property BodyRecv:string read FBodyRecv write SetBodyRecv;
    property HeaderReply:string read FHeaderReply write SetHeaderReply;
    property BodyReply:string read FBodyReply write SetBodyReply;
  end;

  TSSLProxyConn = class(TObject)
  private
    SSLServer:TSynapseTCPServer;
    FSSLPort: Integer;
    FPort: Integer;
    FHost: string;
    FCS:TCriticalSection;
    FLog: string;
    FPackets:TObjectList;
    FRemoveCompression: Boolean;

    procedure SetSSLPort(const Value: Integer);
    procedure TCPServerConnect(AThread:THssClientPeer );
    procedure TCPServerDisconnect(AThread:THssClientPeer);
    procedure TCPServerExecute(AThread: THssClientPeer );
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: Integer);
    function GetLog: string;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetRemoveCompression(const Value: Boolean);
  public
    procedure AddLog(s:string);
    procedure Stop;
    procedure StartSSLServer;
    constructor Create;
    destructor Destroy; override;
    function PacketCount: Integer;
    procedure Packet(aIndex: Integer; out aHeaderIn, aHeaderOut, aBodyIn, aBodyOut: string);
  published

    property Log: string read GetLog;

    property SSLPort:Integer read FSSLPort write SetSSLPort;

    property Host: string read FHost write SetHost;
    property Port: Integer read FPort write SetPort;
    property Active: Boolean read GetActive write SetActive;
    property RemoveCompression: Boolean read FRemoveCompression write SetRemoveCompression;
  end;
implementation

uses
  uHssSetupFile , Math;

{ TSSLProxyConn }
procedure TSSLProxyConn.AddLog(s: string);
begin
  FCS.Acquire;;
  try
    if Length(FLog) > 4000 then
      FLog := '';
    FLog := s + CRLF + FLog;
  
  finally
    FCS.Release;
  end;
end;

constructor TSSLProxyConn.Create;
begin
  FCs := TCriticalSection.Create;
  FPackets := TObjectList.Create(True);
  FRemoveCompression := True;
end;

destructor TSSLProxyConn.Destroy;
begin
  Stop;
  FreeAndNil(FPackets);
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
var
  P: THttpPacket;
  i: Integer;
begin
  FCS.Acquire;;
  try
    for i := 0 to FPackets.Count -1  do
    begin
      p := FPackets[i] as THttpPacket;

      Result := Result + FormatDateTime('dd-mm HH:NN:zzz',p.TimeRecv)+' '+ Copy( p.HeaderRecv, 1, Min( 50 , Pos(#13, p.HeaderRecv) -1)) + CRLF;
    end;
  finally
    FCS.Release;
  end;
end;

procedure TSSLProxyConn.Packet(aIndex: Integer; out aHeaderIn, aHeaderOut, aBodyIn, aBodyOut: string);
var
  P: THttpPacket;
begin
  FCS.Acquire;
  try
    if aIndex >= FPackets.Count then
      exit;
    P := FPackets[aIndex] as THttpPacket;
    aHeaderIn := p.HeaderRecv;
    aHeaderOut := p.HeaderReply;
    aBodyIn := p.BodyRecv;
    aBodyOut := p.BodyReply;
  finally
    FCs.Release;
  end;
end;

function TSSLProxyConn.PacketCount: Integer;
begin
  FCS.Acquire;
  try
    Result := FPackets.Count;
  finally
    FCs.Release;
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

procedure TSSLProxyConn.SetRemoveCompression(const Value: Boolean);
begin
  FRemoveCompression := Value;
end;

procedure TSSLProxyConn.SetSSLPort(const Value: Integer);
begin
  FSSLPort := Value;
end;

procedure TSSLProxyConn.StartSSLServer;
begin
  try
      SSLServer := TSynapseTCPServer.Create(IntToStr(FSSLPort),True,50);
      SSLServer.OnConnect := TCPServerConnect;
      SSLServer.OnExecute := TCPServerExecute;
      SSLServer.OnDisconnect := TCPServerDisconnect;
      SSLServer.ThreadClass := TSynapseTCPServerThread;
      SSLServer.ServerName := 'SSL';
      SSLServer.fSock.SSL.SSLType := LT_TLSv1_2;
      SSLServer.fSock.SSL.CertCAFile := SSL_CERT_ROOT;
      SSLServer.fSock.SSL.CertificateFile := SSL_CERT;
      SSLServer.fSock.SSL.PrivateKeyfILE := SSL_Key;
      SSLServer.Resume;
  except
    on e:Exception do
      AddLog(e.Message);
  end;

end;

procedure TSSLProxyConn.Stop;
begin
  FreeAndNil(SSlServer);
end;

procedure TSSLProxyConn.TCPServerConnect(AThread: THssClientPeer);
var
  Client: TTCPBlockSocket;
begin
  try
  Client := TTCPBlockSocket.Create;
  Client.Connect(Host, IntToStr(Port));
  AThread.Data := Client;
  AThread.FreeData := True;
  except

    on e:exception do
    begin
      AddLog(e.Message);
      raise;
    end;

  end;
end;

procedure TSSLProxyConn.TCPServerDisconnect(AThread: THssClientPeer);
begin
end;

procedure TSSLProxyConn.TCPServerExecute(AThread: THssClientPeer);
var
<<<<<<< HEAD
  sPacket,sData: AnsiString;
=======
  sPacket,sData: string;
>>>>>>> 9213ac9... Add IFDEFS for FPC
  PutDataSize: Integer;
  PutData: TMemoryStream;
  Source,Target:TTcpBlockSocket;
  OriginalHost, OriginalOrigin: AnsiString;
  HttpPacket: THttpPacket;
  bRecv: boolean;

  function StreamString: AnsiString;
  begin
    PutData.Position := 0;
    SetLength(Result, PutData.Size);
    PutData.ReadBuffer(Result[1], PutData.Size);
  end;
  procedure ProcessSource;
  begin
    // As a pass through ssl proxy, we need to understand and process the html correctly
    // and alter the host header to ensure that IIS Express and others dont block requests
    // coming from an outside source.
    PutDataSize := 0;
    sData := '';
    // First read the HTTP Header.
    // Http Header are standard CRLF lines
    // End of header is determined by finding an Empty Line;
    repeat
      sPacket := Source.RecvString(-1);

      if sPacket <> '' then
      begin
        if Sametext( copy(sPacket,1,16),'accept-encoding:') and FRemoveCompression then
        begin
          // remove any compression requests from the packet and the server made adhere to this.
          // usefull for debugging compressed packet content.
          continue;
        end else

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
        end else
        if pos('Origin:', spacket) = 1 then
        begin
          // we must alter the host, so that the reciever believes they were the original target
          // of the HTTP request, or it will be refused. also stripping out HTTPS origins..
          
          sPacket := OriginalOrigin;
          OriginalOrigin := sPacket;
        end;

      end;
      // add header line to data, including the end-of-line empty line
      sData := sData + sPacket + CRLF;

    until sPAcket = '';
    PutData := nil;

    if bRecv then
      HttpPacket.HeaderRecv := sData else
      HttpPacket.HeaderReply := sData;

    // Send the HTTP Header to the target
    Target.SendString(sData);
    if PutDataSize <> 0 then
    begin
     PutData := TMemoryStream.Create;
     try
       PutData.Size := PutDataSize;
       PutData.Position := 0;

       Source.RecvBufferEx(PutData.Memory,PutDataSize,-1);
       PutData.Position := 0;
       Target.SendStreamRaw(PutData);
       if bRecv then
         HttpPacket.BodyRecv := StreamString else
         HttpPacket.BodyReply := StreamString;
     finally
       FreeAndNil(PutData);
     end;
    end;

  end;
begin
  HttpPacket := THttpPacket.Create;
  HttpPacket.TimeRecv := now;
  FCS.Acquire;
  try
    FPackets.Insert(0,HttpPacket);
  finally
    FCs.Release;
  end;
  try
    try
      // Set the Original Host to the name of the Target Http Site
      // Typically this is intended as an IIS-Express LocalHost
      // and the proxy is running on the same machine so that it can access
      // the test server in LocalHost:Http and allow the rest of the network to
      // access SSL on any port and host designation such as Lan IP
      bRecv := True;
      OriginalHost :=  'Host: '+Host+':'+IntToStr(Port);
      OriginalOrigin :=  'Origin: http://'+Host+':'+IntToStr(Port);
      Source := aThread.Sock;
      Target := aThread.Data as TTCPBlockSocket;
      ProcessSource;

      bRecv := False;
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
  finally
    FCs.Acquire;
    try
      if FPackets.Count > 50 then
        FPackets.Delete(50);
    finally
      FCs.Release;
    end;
  end;
end;

{ THttpPacket }

procedure THttpPacket.SetBodyRecv(const Value: string);
begin
  FBodyRecv := Value;
end;

procedure THttpPacket.SetBodyReply(const Value: string);
begin
  FBodyReply := Value;
end;

procedure THttpPacket.SetHeaderRecv(const Value: string);
begin
  FHeaderRecv := Value;
end;

procedure THttpPacket.SetHeaderReply(const Value: string);
begin
  FHeaderReply := Value;
end;

procedure THttpPacket.SetTimeRecv(const Value: TDateTime);
begin
  FTimeRecv := Value;
end;

procedure THttpPacket.SetTimeReply(const Value: TDateTime);
begin
  FTimeReply := Value;
end;

end.
