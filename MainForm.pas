unit MainForm;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  ProxyFrame,
  ExtCtrls,
  ActnList,
  StdCtrls,
  ComCtrls;
  
type

  { TProxyTabSheet }

  TProxyTabSheet = class(TTabSheet)
  private
    FProxyFrame: TframeProxy;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property ProxyFrame: TframeProxy read FProxyFrame;

  end;

  TProxyMainForm = class(TForm)
    ActionList1: TActionList;
    actStartAll: TAction;
    actStopAll: TAction;
    actDelete: TAction;
    actAdd: TAction;
    actSave: TAction;
    Panel1: TPanel;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    edtName: TEdit;
    Button5: TButton;
    pcProxy: TPageControl;
    Memo1: TMemo;
    Timer1: TTimer;
    Button6: TButton;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure actStartAllExecute(Sender: TObject);
    procedure actStopAllExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure pcProxyChange(Sender: TObject);
    procedure edtNameChange(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    TempLog: TStringList;
    procedure BuildPages;
    procedure BuildDefaultPages;
    function ProxyTabSheet:TProxyTabSheet;
    procedure BuildPage(aProxyName: string);
    function GetProxy(aIndex: Integer): TFrameProxy;
  public
    property Proxy[aIndex: Integer]: TFrameProxy read GetProxy;
    { Public declarations }
  end;

var
  ProxyMainForm: TProxyMainForm;

implementation

uses
  uHssSetupFile, uHssShareLog, SSLProxyConn;

{$R *.dfm}
const
  cProxyList = 'PROXY_LIST';
  cProxyPrefix = 'PROXY_';
  cIncomingPort = 'incomingPort';
  cOutgoingPort = 'outgoingPort';
  cOutgoingHost = 'outgoingHost';
  cCaption = 'Caption';
  cRemoveCompression = 'RemoveCompression';

{ TProxyMainForm }
procedure LogMessage(AMessage:string;ALogType:THssLogType);
begin
  GlobalCS.Acquire;
  try
    ProxyMainForm.TempLog.Add(AMessage);
  finally
    GlobalCS.Release;
  end;
end;


procedure TProxyMainForm.actAddExecute(Sender: TObject);
begin
  with ProxyTabSheet do
  begin
    Caption := 'New';
  end;
  pcProxy.ActivePageIndex := pcProxy.PageCount -1;
end;

procedure TProxyMainForm.actDeleteExecute(Sender: TObject);
begin
  pcProxy.ActivePage.Free;
end;

procedure TProxyMainForm.actDeleteUpdate(Sender: TObject);
begin
  actDelete.Enabled := pcProxy.ActivePage <> nil;
  edtName.Enabled := actDelete.Enabled;
end;

procedure TProxyMainForm.actSaveExecute(Sender: TObject);
var
  sections: TStringList;
  I: Integer;
  sectionName: string;
begin
  sections := TStringList.Create;
  try
    SetupFile.ReadSection(cProxyList, sections);
    for i := 0 to sections.Count - 1 do
      SetupFile.EraseSection(cProxyPrefix+sections.Names[i]);
    SetupFile.EraseSection(cProxyList);

    for I := 0 to pcProxy.PageCount - 1 do
    begin
      sectionName := cProxyPrefix+IntToStr(i);
      SetupFile.WriteString(cProxyList, sectionName, IntToStr(i));
      SetupFile.WriteInteger(sectionName, cIncomingPort, StrToInt(Proxy[i].incomingPort.Text));
      SetupFile.WriteInteger(sectionName, cOutgoingPort, StrToInt(Proxy[i].outgoingPort.Text) );
      SetupFile.WriteString(sectionName, cOutgoingHost, Proxy[i].outgoingHost.Text );
      SetupFile.WriteString(sectionName, cCaption, pcProxy.Pages[i].Caption );
      SetupFile.WriteBool(sectionName, cRemoveCompression, Proxy[i].cbRemoveCompression.Checked);

    end;

    SetupFile.UpdateFile;
  finally
    FreeAndNil(sections);

  end;

  
end;

procedure TProxyMainForm.actStartAllExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to pcProxy.PageCount - 1 do
    (pcProxy.Pages[i] as TProxyTabSheet).ProxyFrame.Active := True;
end;

procedure TProxyMainForm.actStopAllExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to pcProxy.PageCount - 1 do
    (pcProxy.Pages[i] as TProxyTabSheet).ProxyFrame.Active := False;
end;

procedure TProxyMainForm.BuildDefaultPages;
begin
  with ProxyTabSheet do
  begin
    ProxyFrame.incomingPort.Text := '8084';
    ProxyFrame.outgoingPort.Text := '56116';
    Caption := 'Web Client';
  end;
  with ProxyTabSheet do
  begin
    ProxyFrame.incomingPort.Text := '8085';
    ProxyFrame.outgoingPort.Text := '56117';
    Caption := 'Web Api';
  end;

end;


procedure TProxyMainForm.BuildPage(aProxyName: string);
var
  Page: TProxyTabSheet;
  sectionName: string;
begin
  sectionName := aProxyName;

  Page := ProxyTabSheet;
  Page.Caption := SetupFile.ReadString(sectionName, cCaption, 'No Name');
  Page.ProxyFrame.incomingPort.Text := IntToStr(SetupFile.ReadInteger(sectionName, cIncomingPort, 0));
  Page.ProxyFrame.outgoingHost.Text := SetupFile.ReadString(sectionName, cOutgoingHost, 'localhost');
  Page.ProxyFrame.outgoingPort.Text := IntToStr(SetupFile.ReadInteger(sectionName, cOutgoingPort, 0));
  Page.ProxyFrame.cbRemoveCompression.Checked := SetupFile.ReadBool(sectionName, cRemoveCompression, True);
end;

procedure TProxyMainForm.BuildPages;
var
  Sections: TStringList;
  I: Integer;
begin
  Sections := TStringList.Create;
  try
    SetupFile.ReadSection(cProxyList, Sections);
    if Sections.Count = 0 then
      BuildDefaultPages else
    begin
      for I := 0 to Sections.Count - 1 do
        BuildPage(Sections[i]);
    end;
  finally
    FreeAndNil(Sections);
  end;
end;

procedure TProxyMainForm.Button6Click(Sender: TObject);
begin
  GlobalCS.Acquire;
  try
    Memo1.Lines.Clear;
  finally
    GlobalCS.Release;

  end;
end;

procedure TProxyMainForm.edtNameChange(Sender: TObject);
begin
  pcProxy.ActivePage.Caption := edtName.Text;
end;

procedure TProxyMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  LogProc := nil;
  actStartAll.Execute;
  CanClose := True;
end;

procedure TProxyMainForm.FormCreate(Sender: TObject);
begin
  TempLog := TStringList.Create;
  LogProc := LogMessage;
  BuildPages;
  pcProxyChange(nil);
end;

procedure TProxyMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(TempLog);
end;

function TProxyMainForm.GetProxy(aIndex: Integer): TFrameProxy;
begin
  Result := (pcProxy.Pages[aIndex] as TProxyTabSheet).ProxyFrame;
end;

procedure TProxyMainForm.pcProxyChange(Sender: TObject);
begin
  if pcProxy.ActivePage <> nil then
    edtName.Text := pcProxy.ActivePage.Caption else
    edtName.Text := '';
end;

function TProxyMainForm.ProxyTabSheet: TProxyTabSheet;
begin
  Result := TProxyTabSheet.Create(self);
  Result.PageControl := pcProxy;
end;

procedure TProxyMainForm.Timer1Timer(Sender: TObject);
begin
  GlobalCS.Acquire;
  try
    if (TempLog.Count = 0) or (CheckBox1.Checked) then
      exit;
    Memo1.Lines.AddStrings(TempLog);
    TempLog.Clear;
  finally
    GlobalCS.Release;
  end;
end;

{ TProxyTabSheet }

procedure TProxyTabSheet.AfterConstruction;
var
  sName: string;
begin
  inherited;
  FProxyFrame := TframeProxy.Create(self.Owner);
  repeat
    sName := 'ProxyFrame'+FormatDateTime('HHNNSSZZZ', now);
    Sleep(1);
  until ProxyMainForm.FindComponent(sName) = nil;
  FProxyFrame.Name := sName;
end;

procedure TProxyTabSheet.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FreeAndNil(FProxyFrame);
end;

procedure TProxyTabSheet.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FProxyFrame then
      FProxyFrame := nil;
end;

procedure TProxyTabSheet.SetParent(AParent: TWinControl);
begin
  inherited;
  if Assigned(FProxyFrame) then
  begin
    FProxyFrame.Parent := self;
    FProxyFrame.Align := alClient;
  end;
end;

end.
