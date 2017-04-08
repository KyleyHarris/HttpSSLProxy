unit MainForm;

interface

uses
<<<<<<< HEAD
  Windows,
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
  
=======
{$IFDEF FPC}
  LCLIntf, LCLType,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ProxyFrame, ComCtrls, ExtCtrls, ActnList, StdCtrls;

>>>>>>> 9213ac9... Add IFDEFS for FPC
type
  TProxyTabSheet = class(TTabSheet)
  private
    FProxyFrame: TframeProxy;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

  public
    procedure AfterConstruction; override;
    property ProxyFrame: TframeProxy read FProxyFrame;

  end;

  TProxyMainForm = class(TForm)
    pcProxy: TPageControl;
    Panel1: TPanel;
    Button1: TButton;
    ActionList1: TActionList;
    actStartAll: TAction;
    Button2: TButton;
    actStopAll: TAction;
    Button3: TButton;
    actDelete: TAction;
    Button4: TButton;
    actAdd: TAction;
    edtName: TEdit;
    Label1: TLabel;
    Button5: TButton;
    actSave: TAction;
    procedure FormCreate(Sender: TObject);
    procedure actStartAllExecute(Sender: TObject);
    procedure actStopAllExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure pcProxyChange(Sender: TObject);
    procedure edtNameChange(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
  private
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
  uHssSetupFile;

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}
const
  cProxyList = 'PROXY_LIST';
  cProxyPrefix = 'PROXY_';
  cIncomingPort = 'incomingPort';
  cOutgoingPort = 'outgoingPort';
  cOutgoingHost = 'outgoingHost';
  cCaption = 'Caption';
  cRemoveCompression = 'RemoveCompression';

{ TProxyMainForm }

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

procedure TProxyMainForm.edtNameChange(Sender: TObject);
begin
  pcProxy.ActivePage.Caption := edtName.Text;
end;

procedure TProxyMainForm.FormCreate(Sender: TObject);
begin
  BuildPages;
  pcProxyChange(nil);
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
