unit uHssShareLog;

interface

uses
  Classes, SysUtils;
type
  TLogExceptionCallback = procedure (const sMessage:string) of object;
  TDbLogExceptionCallback = procedure (const sMessage:string) ;

  THssLogType = (hltDebug,hltException,hltVerbose,hltNormal,hltError,hltOutputDebugString);
  THssLogTypes = set of THssLogType;

procedure HssLogExcept(E:Exception;Alocation:string='');
procedure HssLog(AMessage:string;ALogType:THssLogType);
procedure OutputDebugText(const AText : String);

procedure HssSystem(AMessage:string);

function HssLogTypeToStr(ALogType:THssLogType):string;

type
  TLogProcedure = procedure(AMessage:string;ALogType:THssLogType);


var
  LogProc:TLogProcedure;

  LogTypes:THssLogTypes;

  ExceptionCallback:TLogExceptionCallback=nil;  //Makesure this is threadsafe
  DBExceptionCallback:TDbLogExceptionCallback=nil;

threadvar
  LoggingIP:string;
  LoggingInfo:string;
  HsThreadName:string;
implementation

uses
{$IFDEF MSWINDOWS}
  Windows;
{$ELSE}
  LazLogger;
{$ENDIF}

procedure OutputDebugText(const AText : String);
begin
{$IFDEF MSWINDOWS}
  OutputDebugString(PChar(AText));
{$ELSE}
  DebugLn(AText);
{$ENDIF}
end;

procedure HssSystem(AMessage:string);
begin
  HssLog(AMessage,hltNormal);
  if IsConsole then
    WriteLn(formatdatetime('dd-mmm-yyyy hh:nn.ss am/pm ',now),' System:', AMessage);
end;

procedure HssLogExcept(E:Exception;Alocation:string);
begin
  HssLog(E.ClassName+' '+ALocation+':'+E.Message,hltException);
end;

procedure HssLog(AMessage:string;ALogType:THssLogType);
begin

  if ALogType = hltOutputDebugString then
    OutputDebugText(LoggingIP+ ' ' +AMessage) else
  if Assigned(LogProc) and ( (ALogType in LogTypes ) or (ALogType = hltException)) then
    LogProc(AMessage,ALogType);

  if IsConsole and (ALogType = hltException) then
    HssSystem(AMessage);

  if (ALogType = hltException) and (assigned(DBExceptionCallback)) then
    DBExceptionCallback(LoggingIP+#9+AMessage);

end;

function HssLogTypeToStr(ALogType:THssLogType):string;
begin
  case ALogType of
    hltDebug:         Result := 'Debug';
    hltException:      Result := 'Exception';
    hltVerbose:        Result := 'Verbose';
    hltNormal:         Result := 'Normal';
    hltError:          Result := 'Error';
  else
    Result := 'Unknown Log Type';
  end;
end;

initialization
  LogTypes := [hltDebug..hltOutputDebugString];
end.

