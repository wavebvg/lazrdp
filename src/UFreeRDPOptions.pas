unit UFreeRDPOptions;

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
  DEFAULT_FREERDP_PATH = 'xfreerdp';

type

  { TFreeRDPPreferences }

  TFreeRDPPreferences = class(TComponent)
  private
    FFreeRDPPath: String;
    FGrabKeyboard: Boolean;
  public
    procedure Reset;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToIniFile(const AIniFile: TCustomIniFile);
    procedure LoadFromIniFile(const AIniFile: TCustomIniFile);
  published
    property GrabKeyboard: Boolean read FGrabKeyboard write FGrabKeyboard;
    property FreeRDPPath: String read FFreeRDPPath write FFreeRDPPath;
  end;

  TFreeRDPSecOption = (soUndefined, soRDP, soTLS, soNLA, soExt);

  TFreeRDPGDIOption = (gdioUndefined, gdioHW, gdioSW);

  TFreeRDPCodecCashOption = (ccoUndefined, ccoRFX, ccoNSC, ccoJPEG);

  TFreeRDPRFXModeOption = (rfxmoUndefined, rfxmoImage, rfxmoVideo);

  { TFreeRDPConnectionOptions }

  TFreeRDPConnectionOptions = class(TPersistent)
  private
    FClipboard: Boolean;
    FCodecCash: TFreeRDPCodecCashOption;
    FGDI: TFreeRDPGDIOption;
    FIconFileName: String;
    FResolution: String;
    FRFX: Boolean;
    FRFXMode: TFreeRDPRFXModeOption;
    FSec: TFreeRDPSecOption;
    FSecExt: Boolean;
    FSecNLA: Boolean;
    FSecRDP: Boolean;
    FSecTLS: Boolean;
    FServerInfo: String;
    FConnectionName: String;
    FUseAuthentication: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FHost: String;
    FPort: Word;
    FUser: String;
    FPassword: String;
    FUseCustomResolution: Boolean;
    function GetResolution: String;
    function GetServerInfo: String;
    procedure SetResolution(const AValue: String);
    procedure SetServerInfo(const AValue: String);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure Reset;
  published
    property IconFileName: String read FIconFileName write FIconFileName;
    property ConnectionName: String read FConnectionName write FConnectionName;
    property User: String read FUser write FUser;
    property Password: String read FPassword write FPassword;
    property Host: String read FHost write FHost;
    property Port: Word read FPort write FPort default DEFAULT_RDP_PORT;
    property UseAuthentication: Boolean read FUseAuthentication write FUseAuthentication;
    property UseCustomResolution: Boolean read FUseCustomResolution write FUseCustomResolution;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Sec: TFreeRDPSecOption read FSec write FSec;
    property GDI: TFreeRDPGDIOption read FGDI write FGDI;
    property CodecCash: TFreeRDPCodecCashOption read FCodecCash write FCodecCash;
    property RFX: Boolean read FRFX write FRFX;
    property RFXMode: TFreeRDPRFXModeOption read FRFXMode write FRFXMode;
    property SecExt: Boolean read FSecExt write FSecExt;
    property SecNLA: Boolean read FSecNLA write FSecNLA;
    property SecRDP: Boolean read FSecRDP write FSecRDP;
    property SecTLS: Boolean read FSecTLS write FSecTLS;
    property Clipboard: Boolean read FClipboard write FClipboard;
    
    property ServerInfo: String read GetServerInfo write SetServerInfo;
    property Resolution: String read GetResolution write SetResolution;
  end;

  { TFreeRDPConnectionOptionsList }

  TFreeRDPConnectionOptionsList = class(TComponent)
  private
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): TFreeRDPConnectionOptions;
    procedure SetItem(const AIndex: Integer; const AValue: TFreeRDPConnectionOptions);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Items[const AIndex: Integer]: TFreeRDPConnectionOptions read GetItem write SetItem; default;
    property Count: Integer read GetCount;

    function Add(const AOptions: TFreeRDPConnectionOptions): Integer;
    procedure Delete(const AIndex: Integer);

    procedure SaveToIniFile(const AIniFile: TCustomIniFile);
    procedure LoadFromIniFile(const AIniFile: TCustomIniFile);
  end;

implementation

uses
  TypInfo;

{ TFreeRDPPreferences }

procedure TFreeRDPPreferences.Reset;
begin
  GrabKeyboard := False;
  FreeRDPPath := DEFAULT_FREERDP_PATH;
end;

procedure TFreeRDPPreferences.Assign(Source: TPersistent);
var
  VSource: TFreeRDPPreferences;
begin
  if Source is TFreeRDPPreferences then
  begin
    VSource := TFreeRDPPreferences(Source);
    GrabKeyboard := VSource.GrabKeyboard;
    FreeRDPPath := VSource.FreeRDPPath;
  end
  else
    inherited Assign(Source);
end;

procedure TFreeRDPPreferences.SaveToIniFile(const AIniFile: TCustomIniFile);
begin
  AIniFile.WriteBool('Main', 'GrabKeyboard', GrabKeyboard);
  AIniFile.WriteString('Main', 'FreeRDPPath', FreeRDPPath);
end;

procedure TFreeRDPPreferences.LoadFromIniFile(const AIniFile: TCustomIniFile);
begin
  GrabKeyboard := AIniFile.ReadBool('Main', 'GrabKeyboard', False);
  FreeRDPPath := AIniFile.ReadString('Main', 'FreeRDPPath', DEFAULT_FREERDP_PATH);
end;

{ TFreeRDPConnectionOptionsList }

constructor TFreeRDPConnectionOptionsList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TObjectList.Create;
end;

destructor TFreeRDPConnectionOptionsList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TFreeRDPConnectionOptionsList.Add(const AOptions: TFreeRDPConnectionOptions): Integer;
begin
  Result := FItems.Add(TFreeRDPConnectionOptions.Create);
  Items[Result].Assign(AOptions);
end;

procedure TFreeRDPConnectionOptionsList.Delete(const AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;

procedure TFreeRDPConnectionOptionsList.SaveToIniFile(const AIniFile: TCustomIniFile);
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
    AIniFile.WriteBool(VSectionName, 'RFX', Items[i].RFX);
    AIniFile.WriteString(VSectionName, 'Sec', GetEnumName(TypeInfo(TFreeRDPSecOption), Ord(Items[i].Sec)));
    AIniFile.WriteString(VSectionName, 'GDI', GetEnumName(TypeInfo(TFreeRDPGDIOption), Ord(Items[i].GDI)));
    AIniFile.WriteString(VSectionName, 'CodecCash', GetEnumName(TypeInfo(TFreeRDPCodecCashOption),
      Ord(Items[i].CodecCash)));
    AIniFile.WriteString(VSectionName, 'RFXMode', GetEnumName(TypeInfo(TFreeRDPRFXModeOption),
      Ord(Items[i].RFXMode)));
    AIniFile.WriteBool(VSectionName, 'SecExt', Items[i].SecExt);
    AIniFile.WriteBool(VSectionName, 'SecNLA', Items[i].SecNLA);
    AIniFile.WriteBool(VSectionName, 'SecRDP', Items[i].SecRDP);
    AIniFile.WriteBool(VSectionName, 'SecTLS', Items[i].SecTLS);
    AIniFile.WriteBool(VSectionName, 'Clipboard', Items[i].Clipboard);
    AIniFile.WriteString(VSectionName, 'IconFileName', Items[i].IconFileName);
  end;
end;

procedure TFreeRDPConnectionOptionsList.LoadFromIniFile(const AIniFile: TCustomIniFile);
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
        VOptions.RFX := AIniFile.ReadBool(VSectionName, 'RFX', False);
        VOptions.ConnectionName := AIniFile.ReadString(VSectionName, 'ConnectionName', '');
        VOptions.User := AIniFile.ReadString(VSectionName, 'User', '');
        VOptions.Password := AIniFile.ReadString(VSectionName, 'Password', '');
        VOptions.Host := AIniFile.ReadString(VSectionName, 'Host', DEFAULT_HOSTNAME);
        VOptions.Port := AIniFile.ReadInteger(VSectionName, 'Port', DEFAULT_RDP_PORT);
        VOptions.UseAuthentication := AIniFile.ReadBool(VSectionName, 'UseAuthentication', False);
        VOptions.UseCustomResolution := AIniFile.ReadBool(VSectionName, 'UseCustomResolution', False);
        VOptions.Width := AIniFile.ReadInteger(VSectionName, 'Width', 0);
        VOptions.Height := AIniFile.ReadInteger(VSectionName, 'Height', 0);
        VOptions.Sec := TFreeRDPSecOption(GetEnumValue(TypeInfo(TFreeRDPSecOption),
          AIniFile.ReadString(VSectionName, 'Sec', GetEnumName(TypeInfo(TFreeRDPSecOption), 0))));
        VOptions.GDI := TFreeRDPGDIOption(GetEnumValue(TypeInfo(TFreeRDPGDIOption),
          AIniFile.ReadString(VSectionName, 'GDI', GetEnumName(TypeInfo(TFreeRDPGDIOption), 0))));
        VOptions.CodecCash := TFreeRDPCodecCashOption(GetEnumValue(TypeInfo(TFreeRDPCodecCashOption),
          AIniFile.ReadString(VSectionName, 'CodecCash', GetEnumName(TypeInfo(TFreeRDPCodecCashOption), 0))));
        VOptions.RFXMode := TFreeRDPRFXModeOption(GetEnumValue(TypeInfo(TFreeRDPRFXModeOption),
          AIniFile.ReadString(VSectionName, 'RFXMode', GetEnumName(TypeInfo(TFreeRDPRFXModeOption), 0))));
        VOptions.SecExt := AIniFile.ReadBool(VSectionName, 'SecExt', False);
        VOptions.SecNLA := AIniFile.ReadBool(VSectionName, 'SecNLA', True);
        VOptions.SecRDP := AIniFile.ReadBool(VSectionName, 'SecRDP', True);
        VOptions.SecTLS := AIniFile.ReadBool(VSectionName, 'SecTLS', True);
        VOptions.Clipboard := AIniFile.ReadBool(VSectionName, 'Clipboard', True);
        VOptions.IconFileName := AIniFile.ReadString(VSectionName, 'IconFileName', '');
      end;
    end;
  finally
    VSections.Free;
  end;
end;

function TFreeRDPConnectionOptionsList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TFreeRDPConnectionOptionsList.GetItem(const AIndex: Integer): TFreeRDPConnectionOptions;
begin
  Result := FItems[AIndex] as TFreeRDPConnectionOptions;
end;

procedure TFreeRDPConnectionOptionsList.SetItem(const AIndex: Integer; const AValue: TFreeRDPConnectionOptions);
begin
  FItems[AIndex] := TFreeRDPConnectionOptions.Create;
  Items[AIndex].Assign(AValue);
end;

procedure TFreeRDPConnectionOptionsList.AssignTo(Dest: TPersistent);
var
  i: Integer;
begin
  if Dest is TStrings then
  begin
    TStrings(Dest).BeginUpdate;
    try
      TStrings(Dest).Clear;
      for i := 0 to Count - 1 do
        TStrings(Dest).AddObject(Items[i].ConnectionName, Items[i]);
    finally
      TStrings(Dest).EndUpdate;
    end;
  end
  else
    inherited AssignTo(Dest);
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
  FServerInfo := '';
  FResolution := '';
  if Source is TFreeRDPConnectionOptions then
  begin
    VSource := TFreeRDPConnectionOptions(Source);
    ConnectionName := VSource.ConnectionName;
    IconFileName := VSource.IconFileName;
    Host := VSource.Host;
    Port := VSource.Port;
    User := VSource.User;
    Password := VSource.Password;
    Sec := VSource.Sec;
    GDI := VSource.GDI;
    RFXMode := VSource.RFXMode;
    CodecCash := VSource.CodecCash;
    UseCustomResolution := VSource.UseCustomResolution;
    UseAuthentication := VSource.UseAuthentication;
    Height := VSource.Height;
    Width := VSource.Width;
    RFX := VSource.RFX;
    SecExt  := VSource.SecExt ;
    SecNLA := VSource.SecNLA;
    SecRDP := VSource.SecRDP;
    SecTLS := VSource.SecTLS;
    Clipboard := VSource.Clipboard;
    
  end
  else
    inherited Assign(Source);
end;

function TFreeRDPConnectionOptions.GetResolution: String;
begin
  if FResolution <> '' then
    Result := FResolution
  else
  begin
    if (Width > 0) and (Height > 0) then
      Result := Format('%dx%d', [Width, Height])
    else
      Result := '';
  end;
end;

procedure TFreeRDPConnectionOptions.SetResolution(const AValue: String);
var
  VWHSepPos: Integer;
begin
  FResolution := AValue;
  VWHSepPos := Pos('x', AValue);
  if VWHSepPos > 0 then
  begin
    Width := StrToIntDef(Copy(AValue, 1, VWHSepPos - 1), 0);
    Height := StrToIntDef(Copy(AValue, VWHSepPos + 1, Length(AValue) - VWHSepPos), 0);
  end
  else
  begin
    Width := 0;
    Height := 0;
  end;
end;

function TFreeRDPConnectionOptions.GetServerInfo: String;
begin
  if FServerInfo <> '' then
    Result := FServerInfo
  else
  begin
    if Port = DEFAULT_RDP_PORT then
      Result := Host
    else
      Result := Format('%s:%d', [Host, Port]);
  end;
end;

procedure TFreeRDPConnectionOptions.SetServerInfo(const AValue: String);
var
  VPortSepPos: Integer;
begin
  FServerInfo := AValue;
  VPortSepPos := Pos(':', AValue);
  if VPortSepPos > 0 then
  begin
    Host := Copy(AValue, 1, VPortSepPos - 1);
    Port := StrToIntDef(Copy(AValue, VPortSepPos + 1, Length(AValue) - VPortSepPos), DEFAULT_RDP_PORT);
  end
  else
  begin
    Host := AValue;
    Port := DEFAULT_RDP_PORT;
  end;
end;

procedure TFreeRDPConnectionOptions.Reset;
begin
  FClipboard := True;
  FSecExt := False;
  FSecNLA := True;
  FSecRDP := True;
  FSecTLS := True;
  FRFXMode := rfxmoUndefined;
  FRFX := False;
  FCodecCash := ccoUndefined;
  FGDI := gdioUndefined;
  FSec := soUndefined;
  IconFileName := '';
  FServerInfo := '';
  FResolution := '';
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
