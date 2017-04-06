unit ProxyFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SSLProxyConn, StdCtrls, ExtCtrls, ActnList;

type
  TframeProxy = class(TFrame)
    incomingHost: TEdit;
    incomingPort: TEdit;
    outgoingHost: TEdit;
    outgoingPort: TEdit;
    btnGo: TButton;
    Memo1: TMemo;
    Timer1: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    Actions: TActionList;
    actGo: TAction;
    procedure Timer1Timer(Sender: TObject);
    procedure actGoUpdate(Sender: TObject);
    procedure actGoExecute(Sender: TObject);
  private
    FProxy: TSSLProxyConn;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Active: Boolean read GetActive write SetActive;
  end;

implementation

{$R *.dfm}

{ TFrame1 }

procedure TframeProxy.actGoExecute(Sender: TObject);
begin
  Active := not Active;
end;

procedure TframeProxy.actGoUpdate(Sender: TObject);
begin
  if FProxy.Active then
    actGo.Caption := 'Stop' else
    actGo.Caption := 'Start';
end;

procedure TframeProxy.AfterConstruction;
begin
  inherited;
  FProxy := TSSLProxyConn.Create;
end;

procedure TframeProxy.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FProxy);
end;

function TframeProxy.GetActive: Boolean;
begin
  result := FProxy.Active;
end;

procedure TframeProxy.SetActive(const Value: Boolean);
begin
  if Value then
  begin
    FProxy.SSLPort := StrToInt(incomingPort.Text);
    FProxy.Host := outgoingHost.Text;
    FProxy.Port := StrToInt(outgoingPort.Text);
  end;
  FProxy.Active := Value;
end;

procedure TframeProxy.Timer1Timer(Sender: TObject);
begin
  Memo1.Text := FProxy.Log;
end;

end.
