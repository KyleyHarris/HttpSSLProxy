unit uHssSetupFile;

interface

uses
  SysUtils,
  Classes,
  IniFiles;

var
  SetupFileName:string = '';
  ApplicationName:string='';
  AppPath:string='';
  SetupFile:TMemIniFile;
  Section:string;

  SSL_CERT_ROOT:string;
  SSL_CERT:string;
  SSL_Key:string;

procedure RefreshSetupFile(AFileName:string='');

function GetParameter(AField:string;ADefault:string=''):string;
function GetParamInt(AField:string;ADefault:integer=0):integer;
function GetParamBool(AField:string;ADefault:boolean=false):boolean;

implementation



function GetParamBool(AField:string;ADefault:boolean=false):boolean;
begin
  result := GetParameter(AField,IntToStr(integer(ADefault))) = '1';
end;

function GetParamInt(AField:string;ADefault:integer=0):integer;
begin
  result := StrToIntDef(GetParameter(AField,''),ADefault);
end;

procedure RefreshSetupFile(AFileName:string);
begin
  if AFilename = '' then
    SetupFileName := ExtractFilePath(ParamStr(0))+Copy(ExtractFileName(ParamStr(0)),1,Pos('.',ExtractFileName(ParamStr(0)) ))+'ini' else
    SetupFileName := AFilename;
  FreeAndNil(SetupFile);
  SetupFile := TMemIniFile.Create(SetupFileName);

  SSL_CERT_ROOT := Getparameter('SSLCERTROOT');
  if SSL_CERT_ROOT <> '' then
    if (pos('\',SSL_CERT_ROOT) = 0) and (SSL_CERT_ROOT <> '') then
      SSL_CERT_ROOT := AppPath + SSL_CERT_ROOT;

  SSL_CERT := Getparameter('SSLCERT');
  if (pos('\',SSL_CERT) = 0) and (SSL_CERT <> '') then
    SSL_CERT := AppPath + SSL_CERT;
  SSL_Key := Getparameter('SSLKEY');
  if (pos('\',SSL_KEY) = 0) and (SSL_KEY <> '') then
    SSL_KEY := AppPath + SSL_KEY;

end;

function GetParameter(AField:string;ADefault:string):string;
var
  ASection:string;

function FixFile(AFile:string):string;
begin
  result := StringReplace(Afile,'$APPPATH',ExtractFilePath(ParamStr(0)),[rfReplaceAll,rfIgnoreCase]);
  result := StringReplace(Result,'\\','\',[rfReplaceAll,rfIgnoreCase]);
end;

begin
  if Section = '' then
    ASection := 'MAIN' else
    ASection := Section;
  result := FixFile(SetupFile.ReadString(ASection,AField,ADefault)) ;
end;

initialization
  ApplicationName := Copy(ExtractFileName(ParamStr(0)),1,Pos('.',ExtractFileName(ParamStr(0)) )-1);
  AppPath := ExtractFilePath(ParamStr(0)) ;
  RefreshSetupFile;
finalization

  FreeAndNil(SetupFile);
end.
