program SSLProxy;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  {$IFDEF FASTMM}
  FASTMM4 ,
  {$ENDIF}
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  MainForm in 'MainForm.pas' {ProxyMainForm},
  SSLProxyConn in 'SSLProxyConn.pas',
  ProxyFrame in 'ProxyFrame.pas' {frameProxy: TFrame},
  uHssSetupFile in 'uHssSetupFile.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TProxyMainForm, ProxyMainForm);
  Application.Run;
end.
