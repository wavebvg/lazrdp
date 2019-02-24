unit UFreeRDPConnectionOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Contnrs,
  IniFiles;

const
  DEFAULT_RDP_PORT = 3389;
  DEFAULT_HOSTNAME = 'localhost';

type

  { TFreeRDPConnectionOptions }

  TFreeRDPConnectionOptions = class(TPersistent)
  private
    FConnectionName: String;
    FUseAuthentication: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FHost: String;
    FPort: Word;
    FUser: String;
    FPassword: String;
    FUseCustomResolution: Boolean;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure Reset;
  published
    property ConnectionName: String read FConnectionName write FConnectionName;
    property User: String read FUser write FUser;
    property Password: String read FPassword write FPassword;
    property Host: String read FHost write FHost;
    property Port: Word read FPort write FPort default DEFAULT_RDP_PORT;
    property UseAuthentication: Boolean read FUseAuthentication write FUseAuthentication;
    property UseCustomResolution: Boolean read FUseCustomResolution write FUseCustomResolution;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
  end;

  { TFreeRDPOptionsList }

  TFreeRDPOptionsList = class(TComponent)
  private
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): TFreeRDPConnectionOptions;
    procedure SetItem(const AIndex: Integer; const AValue: TFreeRDPConnectionOptions);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Items[const AIndex: Integer]: TFreeRDPConnectionOptions read GetItem write SetItem; default;
    property Count: Integer read GetCount;

    function Add(const AOptions: TFreeRDPConnectionOptions): Integer;
    procedure Delete(const AIndex: Integer);

    procedure SaveToIniFile(const AIniFile: TIniFile);
    procedure LoadFromIniFile(const AIniFile: TIniFile);
  end;

implementation

{ TFreeRDPOptionsList }

constructor TFreeRDPOptionsList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TObjectList.Create;
end;

destructor TFreeRDPOptionsList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TFreeRDPOptionsList.Add(const AOptions: TFreeRDPConnectionOptions): Integer;
begin
  Result := FItems.Add(TFreeRDPConnectionOptions.Create);
  Items[Result].Assign(AOptions);
end;

procedure TFreeRDPOptionsList.Delete(const AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;

procedure TFreeRDPOptionsList.SaveToIniFile(const AIniFile: TIniFile);
var
  VSections: TStrings;
  VSectionName: String;
  i: Integer;
begin
  VSections := TStringList.Create;
  try
    AIniFile.ReadSections(VSections);
    for i := 0 to VSections.Count - 1 do
    begin
      VSectionName := VSections[i];
      if AIniFile.ValueExists(VSectionName, 'ConnectionName') then
        AIniFile.EraseSection(VSectionName);
    end;
  finally
    VSections.Free;
  end;
  for i := 0 to Count - 1 do
  begin
    VSectionName := Format('Section%d', [i]);
    AIniFile.WriteString(VSectionName, 'ConnectionName', Items[i].ConnectionName);
    AIniFile.WriteString(VSectionName, 'Host', Items[i].Host);
    if Items[i].Port <> DEFAULT_RDP_PORT then
      AIniFile.WriteInteger(VSectionName, 'Port', Items[i].Port);
    AIniFile.WriteBool(VSectionName, 'UseAuthentication', Items[i].UseAuthentication);
    if Items[i].UseAuthentication then
    begin
      AIniFile.WriteString(VSectionName, 'User', Items[i].User);
      AIniFile.WriteString(VSectionName, 'Password', Items[i].Password);
    end;
    AIniFile.WriteBool(VSectionName, 'UseCustomResolution', Items[i].UseCustomResolution);
    if Items[i].UseCustomResolution then
    begin
      AIniFile.WriteInteger(VSectionName, 'Width', Items[i].Width);
      AIniFile.WriteInteger(VSectionName, 'Height', Items[i].Height);
    end;
  end;
end;

procedure TFreeRDPOptionsList.LoadFromIniFile(const AIniFile: TIniFile);
var
  VSections: TStrings;
  VSectionName: String;
  i: Integer;
  VOptions: TFreeRDPConnectionOptions;
begin
  FItems.Clear;
  VSections := TStringList.Create;
  try
    AIniFile.ReadSections(VSections);
    for i := 0 to VSections.Count - 1 do
    begin
      VSectionName := VSections[i];
      if AIniFile.ValueExists(VSectionName, 'ConnectionName') then
      begin
        VOptions := TFreeRDPConnectionOptions.Create;
        FItems.Add(VOptions);
        VOptions.ConnectionName := AIniFile.ReadString(VSectionName, 'ConnectionName', '');
        VOptions.User := AIniFile.ReadString(VSectionName, 'User', '');
        VOptions.Password := AIniFile.ReadString(VSectionName, 'Password', '');
        VOptions.Host := AIniFile.ReadString(VSectionName, 'Host', DEFAULT_HOSTNAME);
        VOptions.Port := AIniFile.ReadInteger(VSectionName, 'Port', DEFAULT_RDP_PORT);
        VOptions.UseAuthentication := AIniFile.ReadBool(VSectionName, 'UseAuthentication', False);
        VOptions.UseCustomResolution := AIniFile.ReadBool(VSectionName, 'UseCustomResolution', False);
        VOptions.Width := AIniFile.ReadInteger(VSectionName, 'Width', 0);
        VOptions.Height := AIniFile.ReadInteger(VSectionName, 'Height', 0);
      end;
    end;
  finally
    VSections.Free;
  end;
end;

function TFreeRDPOptionsList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TFreeRDPOptionsList.GetItem(const AIndex: Integer): TFreeRDPConnectionOptions;
begin
  Result := FItems[AIndex] as TFreeRDPConnectionOptions;
end;

procedure TFreeRDPOptionsList.SetItem(const AIndex: Integer; const AValue: TFreeRDPConnectionOptions);
begin
  FItems[AIndex] := TFreeRDPConnectionOptions.Create;
  Items[AIndex].Assign(AValue);
end;

{ TFreeRDPConnectionOptions }

constructor TFreeRDPConnectionOptions.Create;
begin
  FPort := DEFAULT_RDP_PORT;
  FHost := DEFAULT_HOSTNAME;
end;

procedure TFreeRDPConnectionOptions.Assign(Source: TPersistent);
var
  VSource: TFreeRDPConnectionOptions;
begin
  if Source is TFreeRDPConnectionOptions then
  begin
    VSource := TFreeRDPConnectionOptions(Source);
    ConnectionName := VSource.ConnectionName;
    Host := VSource.Host;
    Port := VSource.Port;
    User := VSource.User;
    Password := VSource.Password;
    UseCustomResolution := VSource.UseCustomResolution;
    UseAuthentication := VSource.UseAuthentication;
    Height := VSource.Height;
    Width := VSource.Width;
  end
  else
    inherited Assign(Source);
end;

procedure TFreeRDPConnectionOptions.Reset;
begin
  Host := DEFAULT_HOSTNAME;
  Port := DEFAULT_RDP_PORT;
  User := '';
  Password := '';
  ConnectionName := '';
  UseCustomResolution := False;
  UseAuthentication := False;
  Height := 0;
  Width := 0;
end;

end.

