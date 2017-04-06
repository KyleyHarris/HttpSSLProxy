program SSLProxy;

uses
  {$IFDEF FASTMM}
  FASTMM4 ,
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
