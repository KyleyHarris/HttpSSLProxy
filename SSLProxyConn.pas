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

  TSSLProxyConn = class(TObject)
  private
    SSLServer:TSynapseTCPServer;
    FSSLPort: Integer;
    FPort: Integer;
    FHost: string;
    FLog: string;
    FRemoveCompression: Boolean;

    procedure SetSSLPort(const Value: Integer);
    procedure TCPServerConnect(AThread:THssClientPeer );
    procedure TCPServerDisconnect(AThread:THssClientPeer);
    procedure TCPServerExecute(AThread: THssClientPeer );
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: Integer);
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetRemoveCompression(const Value: Boolean);
  public
    Me:Integer;
    procedure Stop;
    procedure StartSSLServer;
    constructor Create;
    destructor Destroy; override;
  published


    property SSLPort:Integer read FSSLPort write SetSSLPort;

    property Host: string read FHost write SetHost;
    property Port: Integer read FPort write SetPort;
    property Active: Boolean read GetActive write SetActive;
    property RemoveCompression: Boolean read FRemoveCompression write SetRemoveCompression;
  end;

var
  GlobalCS : TCriticalSection;
implementation

uses
  uHssSetupFile , Math, uHssShareLog, DateUtils;

{ TSSLProxyConn }

constructor TSSLProxyConn.Create;
begin
  FRemoveCompression := True;
end;

destructor TSSLProxyConn.Destroy;
begin
  Stop;
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
      HssSystem(Format('HTTPS Proxy Started on https://localhost:%d Routing to http://%s:%d/',[FSSLPort, Host, Port]));

  except
    on e:Exception do
      HssSystem('CANNOT START PROXY ' +e.Message);
  end;

end;

procedure TSSLProxyConn.Stop;
begin
  FreeAndNil(SSlServer);
  HssSystem(Format('HTTPS Proxy Ended on https://localhost:%d Routing to http://%s:%d/',[FSSLPort, Host, Port]));
end;

procedure TSSLProxyConn.TCPServerConnect(AThread: THssClientPeer);
var
  Client: TTCPBlockSocket;
begin
  try
    Client := TTCPBlockSocket.Create;
    Client.RaiseExcept := True;
    Client.Tag := 1;
    Client.Connect(Host, IntToStr(Port));
    AThread.Data := Client;
    AThread.FreeData := True;
  except
    on e:exception do
    begin
      AThread.Log(e.ClassName+' '+ e.Message);
      raise;
    end;

  end;
end;

procedure TSSLProxyConn.TCPServerDisconnect(AThread: THssClientPeer);
begin
  AThread.Log('Disconnected');
end;

procedure TSSLProxyConn.TCPServerExecute(AThread: THssClientPeer);
var
  sPacket,sData: AnsiString;
  PutDataSize: Integer;
  PutData: TMemoryStream;
  Source,Target:TTcpBlockSocket;
  OriginalHost, OriginalOrigin: AnsiString;
  IncomingRequest: boolean;
  sCrossOriginRequest: Ansistring;
  sOriginalData: AnsiString;
  MessageType: string;
  Referer: string;


  function StreamString: AnsiString;
  begin
    PutData.Position := 0;
    SetLength(Result, PutData.Size);
    PutData.ReadBuffer(Result[1], PutData.Size);
  end;

  procedure ProcessSource;
  var
    temp: string;
    Header: TStringList;
    timer: TDateTime;
    i: Integer;
  begin
    Header := TStringList.Create;
    Header.NameValueSeparator := ':';
    // As a pass through ssl proxy, we need to understand and process the html correctly
    // and alter the host header to ensure that IIS Express and others dont block requests
    // coming from an outside source.
    PutDataSize := 0;
    sData := '';
    sOriginalData := '';
    // First read the HTTP Header.
    // Http Header are standard CRLF lines
    // End of header is determined by finding an Empty Line;
    if IncomingRequest then
      AThread.Log('Reading Client Header') else
      AThread.Log('Reading Server Header');
    repeat
      sPacket := Source.RecvString(-1);
      Header.Add(sPacket);
      AThread.Log(spacket);
    until sPAcket = '';

    if IncomingRequest then
      AThread.Log('Client Header Complete') else
      AThread.Log('Server Header Complete');

    sOriginalData := Header.Text;
    if IncomingRequest then
      MessageType := Copy(header[0], 1, pos(' ',Header[0])-1);

    if IncomingRequest then
    begin
      Referer := Header.Values['Referer'];
  //    if Referer <> '' then
   //     Header.Values['Referer'] := NewReferer;

    end;

    i := Header.IndexOfName('accept-encoding');
    if FRemoveCompression and (i>=0)then
      Header.Delete(i);

    temp := Header.Values['content-length'];
    if temp <> '' then
      PutDataSize := StrToInt(Temp)
    else
      PutDataSize := 0;

    temp := Header.Values['Host'];
    if IncomingRequest and (temp <> '' ) then
    begin
      AThread.Log('Replace Client Host - ' + OriginalHost);
      Header.Values['Host'] := OriginalHost;
      OriginalHost := temp;
    end;

    if IncomingRequest then
    begin

      temp := Header.Values['Origin'];
      Header.Values['Origin'] := OriginalOrigin;
      AThread.Log('Replace Client Origin - ' + OriginalOrigin);
      OriginalOrigin := temp;
      if IncomingRequest then
      begin
        sCrossOriginRequest := OriginalOrigin;
        AThread.Log('Record Origin - ' + OriginalOrigin);
      end;
    end;


    if not IncomingRequest then
    begin
      Header.Values['Access-Control-Allow-Origin'] := sCrossOriginRequest;
      AThread.Log('Set Access-Control-Allow-Origin - '+sCrossOriginRequest);
    end;
   for i := Header.Count - 1 downto 0 do
       if trim(Header[i]) = '' then
         Header.Delete(i);

    sData := Header.Text+#13#10;
    PutData  := nil;

    // Send the HTTP Header to the target
    if IncomingRequest then
      AThread.Log('Transmit Client Header to Server') else
      AThread.Log('Transmit Server Header to Client');
    Target.SendString(sData);

    if PutDataSize <> 0 then
    begin
     PutData := TMemoryStream.Create;
     try
       try
         PutData.Size := PutDataSize;
         PutData.Position := 0;
         if IncomingRequest then
           AThread.Log('Read Client Body. Size='+IntToStr(PutDataSize)) else
           AThread.Log('Read Server Body. Size='+IntToStr(PutDataSize));

         Source.RecvBufferEx(PutData.Memory,PutDataSize,-1);
         AThread.Log('Finished Reading Body');

         temp := Source.RecvBufferStr(1, 2);
         if temp <> '' then
         begin
           AThread.Log('Found content-overflow exceeding Content-Length');
           repeat
             PutData.Write(temp[1], length(temp));
             temp := Source.RecvBufferStr(1,5);
           until (temp = '' ) ;
         end else
         AThread.Log('Packet Complete. Total Size:'+IntToStr(PutData.Size));
       except
         on e:ESynapseError do
         begin
           if (e.ErrorCode <> 10054) and (e.ErrorCode<> 10060) then
             raise;
         end;
       end;
       AThread.Log('');
       PutData.Position := 0;
       if PutData.Size > 0 then
       begin
         AThread.Log('Sending Body');
         Target.SendStreamRaw(PutData);
         AThread.Log('Body Sent');
       end;
     finally
       FreeAndNil(PutData);
     end;
    end else

    if ((not IncomingRequest) and SameText(MessageType,'GET'))  or SameText(MessageType,'POST') then
    begin
     AThread.Log('Expecting Reply to GET/POST. Reading Bulk Data No-Length 10Sec wait');
     PutData := TMemoryStream.Create;
     try
       Timer := now;
       i := 25; // wait for first data for 10secs
       repeat
         try
         Source.RecvStreamRaw(PutData,i);
         except on e:ESynapseError do
         begin
           if (e.ErrorCode = 10054) then break;
           if  (e.ErrorCode<> 10060) then
           begin
             AThread.Log('Error while reading bulk data '+inttostr(e.ErrorCode));
             break;
           end else
           if PutData.Size > 0 then
             break;
         end;
         else raise;

         end;
         AThread.Log('Reading More Extra Data in Bulk Load 25ms Polling');
       until MilliSecondsBetween(now, timer) > 10000 ;
       AThread.Log('Completed Unknown Block Size Read. About to Send Block '+IntToStr(PutData.Size)+' Bytes');
       PutData.Position := 0;
       Target.SendStreamRaw(PutData);
       AThread.Log('Block Sent');
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
    IncomingRequest := True;
    OriginalHost := Host+':'+IntToStr(Port);
    OriginalOrigin :=  'http://'+Host+':'+IntToStr(Port);
    Source := aThread.Sock;
    Target := aThread.Data as TTCPBlockSocket;

    {
    Target.RecvByte(1);
    if not( (Target.LastError = 0) or  (Target.LastError = WSAETIMEDOUT)) then
      raise exception.create('disconnected from main host');
     }
    ProcessSource;

    IncomingRequest := False;
    // Switch Source and target to return the traffic back to the requester.
    Source := Target;
    Target := aThread.Sock;
    ProcessSource;
  except
    on e:Exception do
    begin
      AThread.Log(e.classname+ ' '+e.Message);
      // All errrors must be raised to disconnect correctly.
      // its normal and expected for a peer disconnect.
      raise;
    end;
  end;
end;

initialization
  GlobalCS := TCriticalSection.Create;
finalization
  FreeAndNil(GlobalCS);
end.
