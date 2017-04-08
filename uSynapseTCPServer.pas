 (*

  This source code is Copyright (C) 2006  Kyley Harris

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the author be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone  to use this
  software for any purpose, including commercial applications, and to alter it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original source. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original.
  3. This notice may not be removed or altered from any source distribution.
  5. This header must not be removed or altered.

  Any changes to the code would be appreciated to be emailed to the author
  for consideration to any improvements.

  Kyley Harris, Auckland, New Zealand
  Kyley@HarrisSoftware.com

  THIS CODE IS AN EXTENSION FOR THE FOLLOWING PROJECT
  Project : Ararat Synapse, Copyright (c)1999-2005, Lukas Gebauer
*)
unit uSynapseTCPServer;

interface

uses
  blcksock, Classes, ssl_openssl, synsock;

type

  TSynapseTCPServerPeer = class;

  TSynapseServerThreadEvent = procedure(AContext:TSynapseTCPServerPeer) of object;

  { internal use only. Please don't rely on this information publicly }
  TConnectionState =
  (csConnecting,
   csConnected,
   csDisconnecting,
   csDisconnected);

  TSynapseTCPServerThread = class;
  TSynapseTCPServerThreadClass = class of TSynapseTCPServerThread;

  TSynapseTCPServer = class(TThread)
  private
    FPeakConnections:integer;
    FPort:string;
    FWorkThreads:TThreadList;
    FClientSockets:TThreadList;
    FMaxClients:integer;
    FOnDisconnect: TSynapseServerThreadEvent;
    FOnConnect: TSynapseServerThreadEvent;
    FOnExecute: TSynapseServerThreadEvent;
    FSSLConnections: boolean;
    FActive: boolean;
    FThreadClass: TSynapseTCPServerThreadClass;
    FServerName: string;


    procedure DeactivateallSockets;
    procedure TerminateThreads;


    function ThreadCount:integer;
    procedure SetThreadClass(const Value: TSynapseTCPServerThreadClass);
    procedure SetServerName(const Value: string);

  public
    FSock:TTCPBlockSocket;
    function ClientCount:integer;
    constructor Create(APort:string; AUseSSL:boolean=false; AMaxClients:integer=-1);
    destructor Destroy; override;

    procedure BeforeExecute;virtual;
    procedure AfterExecute;virtual;
    procedure Execute; override;

    property OnExecute: TSynapseServerThreadEvent read FOnExecute write FOnExecute;
    // Occurs in the context of the peer thread
    property OnConnect: TSynapseServerThreadEvent read FOnConnect write FOnConnect;
    // Occurs in the context of the peer thread
    property OnDisconnect: TSynapseServerThreadEvent read FOnDisconnect write FOnDisconnect;


    property Active:boolean read FActive;
    property ClientSockets:TThreadList read FClientSockets;
    property SSLConnections:boolean read FSSLConnections ;
    property ThreadClass:TSynapseTCPServerThreadClass read FThreadClass write SetThreadClass;
    property ServerName:string read FServerName write SetServerName;
  end;

  TSynapseTCPServerPeer = class(TObject)
  private
    FOwner:TSynapseTCPServer;
    CSock: TSocket;
    FConnectState:TConnectionState;
    FFreeExtraData: boolean;
    FExtraData: TObject;
    FCreationDate:TDateTime;
    FInitialized:boolean;
    FLastAccessTime: TDateTime;
    FSocketLocation: string;
    function GetHasSocket: boolean;
    procedure SetConnectState(const Value: TConnectionState);
    procedure SetLastAccessTime(const Value: TDateTime);
    procedure SetSocketLocation(const Value: string);
    property ConnectState:TConnectionState read FConnectState write SetConnectState;
    function GetStatus: string;
  public

    ID:integer;
    Pass:integer;
    Sock:TTCPBlockSocket;
    procedure SocketTest;
    procedure InitializeSocket;
    property Data:TObject read FExtraData write FExtraData;
    property FreeData:boolean read FFreeExtraData write FFreeExtraData;
    property HasSocket:boolean read GetHasSocket ;
    constructor Create(AOwner:TSynapseTCPServer;hsock:tSocket);
    destructor Destroy;override;
    property Status:string read GetStatus;
    property LastAccessTime:TDateTime read FLastAccessTime write SetLastAccessTime;
    property SocketLocation:string read FSocketLocation write SetSocketLocation;

  end;

  TSynapseTCPServerThread = class(TThread)
  private
    FOwner:TSynapseTCPServer;
    procedure SetName;
  protected

    FPeer:TSynapseTCPServerPeer;
    ID:string;
    procedure BeforeExecute;virtual;
    procedure AfterExecute;virtual;
    procedure Execute; override;
    procedure Disconnect(var APeer:TSynapseTCPServerPeer);
    procedure Process(var APeer:TSynapseTCPServerPeer);
    property Peer: TSynapseTCPServerPeer read FPeer;
  public
    constructor Create(AOwner:TSynapseTCPServer;APeer:TSynapseTCPServerPeer=nil);
    destructor Destroy;override;
    property Server:TSynapseTCPServer read FOwner;
  end;

implementation


uses
{$IFDEF FPC}
  LCLIntf, LCLType,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, Math,
    uHssShareLog, DateUtils;

var
  PeerID:integer=0;

{$IFDEF MSWINDOWS}
type
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;
{$ENDIF}
  
{ TSynapseTCPServer }

procedure TSynapseTCPServer.AfterExecute;
begin

end;

procedure TSynapseTCPServer.BeforeExecute;
begin

end;

function TSynapseTCPServer.ClientCount: integer;
begin
  with FClientSockets.LockList do
  try
    result := Count;
  finally
    FClientSockets.UnlockList;
  end;
end;

constructor TSynapseTCPServer.Create(APort:string;AUseSSL:boolean; AMaxClients:integer);
begin
  FActive := true;
  FThreadClass := TSynapseTCPServerThread;

  { This will only allow (x) concurrent connections to the server. leave -1 for
  unlimited }
  FMaxClients := AMaxClients;
  FPort := APort;

  FSSLConnections := AUseSSL;
  Fsock :=TTCPBlockSocket.Create;

  FWorkThreads := TThreadList.Create;
  FClientSockets := TThreadList.Create;

  inherited create(True);
end;

procedure TSynapseTCPServer.DeactivateallSockets;
var
  i:integer;
begin
  with FClientSockets.LockList do
  try
    for i := 0 to Count -1 do
    with TSynapseTCPServerPeer(Items[i]) do
    begin
      ConnectState := csDisconnecting;
      Sock.CloseSocket;
    end;
  finally
    FClientSockets.UnlockList;
  end
end;

destructor TSynapseTCPServer.Destroy;
begin
  Terminate;WaitFor;
  HssSystem('Terminated Listening Socket '+FPort);
  try
    DeactivateallSockets;
    repeat
      Sleep(1);
    until ClientCount = 0;

    TerminateThreads;
    repeat
      Sleep(1);
    until ThreadCount = 0;
  finally
    fSock.free;
  end;
  FreeAndNil(FWorkThreads);
  FreeAndNil(FClientSockets);
  inherited;
end;

procedure TSynapseTCPServer.Execute;
var
  ClientSock:TSocket;
  NewPeer:TSynapseTCPServerPeer;
  FLastPingLog:TDateTime;
  I: integer;
begin
  FLastPingLog := now;

  NewPeer := nil;

  with fsock do
    begin
      CreateSocket;
      setLinger(true,10);
      bind('0.0.0.0',FPort);

      if LastError = 0 then
        listen;

      if LastError <> 0 then
        terminate;

      repeat
        if terminated then break;
        if canread(1000) then
          begin
            ClientSock:=accept ;
            if lastError=0 then
            begin
              try
                NewPeer := TSynapseTCPServerPeer.create(Self, ClientSock);

              except
                on e:exception do
                begin
                  HssLogExcept(e,'Synapse Bind Socket');
                end;

              end;
              if ( FMaxClients <> -1 ) and (ClientCount > FMaxClients) then
              begin
                HssLog('Dropping Client Socket because ClientCount Exceeds Max Clients '+inttostr(clientcount)+' ' +inttostr(FMaxClients),hltDebug);
                FreeAndNil(NewPeer);
              end;

            end else
            begin
              HssLog('Synapse Listener Error:'+IntToStr(LastError),hltException);
            end;
          end;



          //Do some testing.
          begin

            if MinutesBetween(now,FLastPingLog) > 2 then
            begin
              FLastPingLog := now;

              try

                with FClientSockets.LockList do
                try
                  HssLog('TCPLISTENER PING '+FPort+' Active TCP Connections '+IntToStr(Count)+'Active Threads '+IntToStr(ThreadCount)+' PEAK CONNECTIONS '+IntToStr(FPeakConnections),hltDebug);

                  for I := 0 to Count - 1 do
                    with TSynapseTCPServerPeer(Items[i]) do
                    try

                      if (not FInitialized) and (SecondsBetween(now,FCreationDate) > 10) then
                      begin
                        LoggingIP := Sock.GetRemoteSinIP;
                        HssLog('Flush Bad SOCKET ',hltException);
                        LoggingIP := '';
                        FConnectState := csDisconnecting;
                        Sock.CloseSocket;

                      end else
                      if FInitialized and (MinutesBetween(now,FLastAccessTime) > 3) then
                      begin
                        LoggingIP := '';
                        FConnectState := csDisconnecting;
                        Sock.CloseSocket;
                      end;


                    except
                      on e:exception do
                        HssLogExcept(e,'Test Peer to flush');


                    end;

                finally
                  FClientSockets.UnlockList;
                end;
              except
                on e:Exception do
                  HssLogExcept(e,'Flush TCP');
              end;

            end;


          end;
      until false;

    end;
  FActive := false;
end;




procedure TSynapseTCPServer.SetServerName(const Value: string);
begin
  FServerName := Value;
end;


procedure TSynapseTCPServer.SetThreadClass(
  const Value: TSynapseTCPServerThreadClass);
begin
  FThreadClass := Value;
end;

procedure TSynapseTCPServer.TerminateThreads;
var
  i:integer;
begin
  with FWorkThreads.LockList do
  try
    for i := 0 to Count -1 do
      TSynapseTCPServerThread(Items[i]).Terminate;
  finally
    FWorkThreads.UnlockList;
  end;
end;

function TSynapseTCPServer.ThreadCount: integer;
begin
  with FWorkThreads.LockList do
  try
    result := Count;
  finally
    FWorkThreads.UnlockList;
  end;

end;

{ TSynapseTCPServerThread }

constructor TSynapseTCPServerPeer.Create(AOwner:TSynapseTCPServer;hsock: tSocket);
var
  Socks:TList;
begin
  SocketLocation := 'CREATED';
  FCreationDate := now;
  FLastAccessTime := now;
  ID := InterlockedIncrement(PeerID);
  Csock := Hsock;
  FOwner := AOwner;
  ConnectState := csConnecting;

  Socks := FOwner.FClientSockets.LockList;
  try
    FOwner.ThreadClass.Create(FOwner,self);
    Socks.Add(self);
    AOwner.FPeakConnections := Max(AOwner.FPeakConnections,Socks.Count);
  finally
    FOwner.FClientSockets.UnlockList;
  end;

end;

procedure TSynapseTCPServerThread.AfterExecute;
begin

end;

procedure TSynapseTCPServerThread.BeforeExecute;
begin

end;

constructor TSynapseTCPServerThread.Create(AOwner: TSynapseTCPServer;APeer:TSynapseTCPServerPeer);
begin
  FPeer := APeer;
  FOwner := AOwner;
  FreeOnTerminate := true;
  with FOwner.FWorkThreads.LockList do
  try
    ID := IntToStr(Add(self))+AOwner.ServerName;
  finally
    FOwner.FWorkThreads.UnlockList;
  end;
  inherited Create(false);

end;

destructor TSynapseTCPServerThread.Destroy;
begin
  with FOwner.FWorkThreads.LockList do
  try
    Remove(self);
  finally
    FOwner.FWorkThreads.UnlockList;
  end;
  inherited;
end;

procedure TSynapseTCPServerThread.Disconnect(var APeer: TSynapseTCPServerPeer);
var
  KillThread:boolean;
begin
  KillThread := FPeer = APeer;

  if Assigned(APeer) then
  try
    if APeer.FInitialized and Assigned(FOwner.OnDisconnect) then
    try
      FOwner.OnDisconnect(APeer);
    except
      on e:exception do
      begin
        HssLogExcept(e,'Synapse Disconnect');
      end;
    end;
  finally
    FreeAndNil(APeer);
    if KillThread then
    begin
      FPeer := nil;
      Terminate;
    end;
  end;
end;

procedure TSynapseTCPServerThread.Execute;
begin
  SetName;
  BeforeExecute;
  try
    while not Terminated do
    try
      Sleep(1);
      if Assigned(Peer) then
      begin
        try
          Process(FPeer) ;
        finally
          if Assigned(Peer) then
          begin
            if Peer.ConnectState = csDisconnected then
                Terminate;
            
          end else terminate;
        end
      end else terminate; //    Sleep(1);
    except
      on e:ESynapseError do
      begin
        case e.ErrorCode of
          10091:
          begin
            if Peer.Sock.SSL.LastError <> 0 then
            begin
              OutputDebugText('EXECUTE SSLERROR '+IntToStr(Peer.Sock.SSL.LastError )+' '+Peer.Sock.SSL.LastErrorDesc );
              Disconnect(FPeer);
            end;
          end;
          else
          begin
           OutputDebugText(e.ClassName+' '+e.Message);
           Disconnect(FPeer);
          end;
        end;
      end;
      on e:exception do
      begin
       Disconnect(FPeer);
       HssLogExcept(e,'Synapse Server Thread Main Loop');
       terminate;
      end;
    end;

  finally
    AFterexecute;
  end;
  FreeAndNil(FPeer);
end;

destructor TSynapseTCPServerPeer.Destroy;
var
  Socks:TList;
begin
  try
    Socks := FOwner.FClientSockets.LockList ;
    try
      Socks.Remove(self);
    finally
      FOwner.FClientSockets.UnlockList;
    end;
    if FFreeExtraData then
      FreeAndNil(FExtraData);

    inherited;
  finally
    Sock.Free;
  end;
end;

procedure TSynapseTCPServerThread.Process(var APeer: TSynapseTCPServerPeer);
begin
  if Assigned(APeer) then
  try
    Inc(APeer.Pass);
    case APeer.FConnectState of
      csConnecting:
      begin
        APeer.SocketLocation := 'CONNECTING';
        APeer.InitializeSocket;
        if not APeer.FInitialized then
        begin
          Disconnect(APeer);
          exit;
        end;
        if Assigned(FOwner.OnConnect) then
           FOwner.OnConnect(APeer);
        if APeer.Sock.Socket <> INVALID_SOCKET then
           APeer.ConnectState := csConnected else
           Disconnect(APeer);
      end ;
      csConnected:
      begin
        if Assigned(FOwner.OnExecute) then
        begin
//          APeer.FLastAccessTime := now;
          APeer.SocketLocation := 'BEFORE OnEXECUTE';
          FOwner.OnExecute(APeer);
          APeer.FLastAccessTime := now;
          APeer.SocketLocation := 'AFTER OnEXECUTE';
        end else
          raise Exception.Create('Synapse Server OnExecute missing');
      end;
      csDisconnecting:
      begin
        APeer.SocketLocation := 'DISCONNECTING';
        Disconnect(APeer);
      end;
      csDisconnected: ;
    end;
  except
    on e:ESynapseError do
    begin
      case e.ErrorCode of
        10091:
        begin
          if APeer.Sock.SSL.LastError <> 0 then
          begin
            OutputDebugText('PROCESS SSLERROR '+IntToStr(APeer.Sock.SSL.LastError )+' '+APeer.Sock.SSL.LastErrorDesc);
            Disconnect(APeer);
          end;
        end;
        WSAETIMEDOUT:;
        else
        begin
          OutputDebugText(e.ClassName+' '+e.Message);
          Disconnect(APeer);
        end;
      end;
    end;
    on e:EAbort do
    begin
      Disconnect(APeer);
    end;
    on e:exception do
    begin
      HssLogExcept(e,'Process Socket');
      Disconnect(APeer);
    end;
  end;
end;
    
function TSynapseTCPServerPeer.GetHasSocket: boolean;
begin
  result := Sock.Socket <> INVALID_SOCKET;
end;

function TSynapseTCPServerPeer.GetStatus: string;
begin
  case FConnectState of
    csConnecting:result := 'Connecting';
    csConnected:result := 'Connected';
    csDisconnecting:result := 'Disconnecting';
    csDisconnected:result := 'Disconnected';
  end;
end;

procedure TSynapseTCPServerPeer.InitializeSocket;
begin
if not FInitialized then
begin
  try
  sock:=TTCPBlockSocket.create;
  Sock.RaiseExcept := TRUE;
  Sock.socket:=CSock;
  sock.GetSins;
  FFreeExtraData := True;

  if FOwner.SSLConnections then
  begin



    try
    Sock.SSL.CertCAFile := FOwner.fSock.SSL.CertCAFile;
    Sock.SSL.CertificateFile := FOwner.fSock.SSL.CertificateFile;
    Sock.SSL.PrivateKeyFile := FOwner.fSock.SSL.PrivateKeyFile;
    Sock.SSL.SSLType := LT_TLSv1_2;
//    Sock.SSLAcceptConnection;


      if (not Sock.SSLAcceptConnection) or
         (Sock.SSL.LastError <> 0) then
      begin
        HssLog('Error while accepting SSL connection: ' + Sock.SSL.LastErrorDesc, hltException);
        FInitialized := false;
      end;
    except
      on e:ESynapseError do
      begin
        FInitialized := false;
        case e.ErrorCode of
          10091:
          begin
            FInitialized := false;
            if Sock.SSL.LastError <> 0 then
            begin
              raise;
            end;
          end;
          else
          begin
           raise;
          end;
        end;
      end;


    end;

  end;
  finally
    FInitialized := true;
  end;
end;

end;

procedure TSynapseTCPServerPeer.SetConnectState(
  const Value: TConnectionState);
begin
  FConnectState := Value;
end;

procedure TSynapseTCPServerPeer.SetLastAccessTime(const Value: TDateTime);
begin
  FLastAccessTime := Value;
end;

procedure TSynapseTCPServerPeer.SetSocketLocation(const Value: string);
begin
  FSocketLocation := Value;
end;

procedure TSynapseTCPServerPeer.SocketTest;
begin
(*
  if (ConnectState <> csDisconnecting) and (Sock.LastError <> 0) and (Sock.LastError <> WSAETIMEDOUT) or (Sock.Socket = INVALID_SOCKET) then
  begin
    Sock.CloseSocket;
    if ConnectState < csDisconnecting then
      ConnectState := csDisconnecting;
  end else
  begin
    Sock.PeekByte(1);
    if (Sock.LastError <> 0) and (Sock.LastError <> WSAETIMEDOUT) then
      Sock.CloseSocket;
    if Sock.Socket = INVALID_SOCKET then
      ConnectState := csDisconnecting ;
  end;

  *)
end;

procedure TSynapseTCPServerThread.SetName;
{$IFDEF MSWINDOWS}
var
  ThreadNameInfo: TThreadNameInfo;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := PChar('Synapse Worker '+ID);
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags := 0;

  try
    RaiseException( $406D1388, 0, sizeof(ThreadNameInfo) div sizeof(LongWord), @ThreadNameInfo );
  except
  end;
{$ENDIF}
end;

end.
