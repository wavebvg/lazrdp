unit UDMFreeRDP;

{$mode objfpc}{$H+}

interface

uses
  UDialogFreeRDPOptions,
  UDialogFreeRDPPreferences,
  UFreeRDPOptions,
  SysUtils,
  Classes,
  IniFiles;

type

  { TDMFreeRDP }

  TDMFreeRDP = class(TDataModule)
  private
    FIniStream: TStream;
    FPreferences: TFreeRDPPreferences;
    FIniFile: TIniFile;
    FConnections: TFreeRDPConnectionOptionsList;
    FDialogFreeRDPOptions: TDialogFreeRDPOptions;
    FDialogFreeRDPPreferences: TDialogFreeRDPPreferences;
    function GetDialogFreeRDPOptions: TDialogFreeRDPOptions;
    function GetDialogFreeRDPPreferences: TDialogFreeRDPPreferences;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SaveConnections;
    procedure SaveOptions;
    procedure EditOptions;

    property DialogFreeRDPOptions: TDialogFreeRDPOptions read GetDialogFreeRDPOptions;
    property DialogFreeRDPPreferences: TDialogFreeRDPPreferences read GetDialogFreeRDPPreferences;
    property Preferences: TFreeRDPPreferences read FPreferences;

    property Connections: TFreeRDPConnectionOptionsList read FConnections;
  end;

var
  DMFreeRDP: TDMFreeRDP;

implementation

uses
  BaseUnix;

{$R *.lfm}

{ TDMFreeRDP }

constructor TDMFreeRDP.Create(AOwner: TComponent);

  function TryCreateIni(const AFileName: String): Boolean;
  var
    VFileName: String;
    VHandle: THandle;
  begin
    Result := False;
    VFileName := ExpandFileName(AFileName);
    if ForceDirectories(ExtractFileDir(VFileName)) then
    begin
      if not FileExists(VFileName) then
      begin
        VHandle := FileCreate(AFileName, fmCreate, 438);
        if VHandle = feInvalidHandle then
          Exit
        else
          FileClose(VHandle);
      end;
      if FpAccess(VFileName, W_OK) = 0 then
        FIniFile := TIniFile.Create(VFileName)
      else
        Exit;
      Result := True;
    end;
  end;

begin
  inherited Create(AOwner);
  ChDir(ExtractFileDir(ParamStr(0)));
  if not TryCreateIni('lazrdp.ini') and not TryCreateIni(Format('%s/.local/share/lazrdp/lazrdp.ini',
    [GetUserDir])) then
    FIniFile := TMemIniFile.Create('');
  FConnections := TFreeRDPConnectionOptionsList.Create(Self);
  FConnections.LoadFromIniFile(FIniFile);
  FPreferences := TFreeRDPPreferences.Create(Self);
  FPreferences.LoadFromIniFile(FIniFile);
end;

destructor TDMFreeRDP.Destroy;
begin
  SaveConnections;
  SaveOptions;
  FIniFile.Free;
  FIniStream.Free;
  inherited Destroy;
end;

procedure TDMFreeRDP.SaveConnections;
begin
  Connections.SaveToIniFile(FIniFile);
end;

function TDMFreeRDP.GetDialogFreeRDPOptions: TDialogFreeRDPOptions;
begin
  if not Assigned(FDialogFreeRDPOptions) then
    FDialogFreeRDPOptions := TDialogFreeRDPOptions.Create(Self);
  Result := FDialogFreeRDPOptions;
end;

function TDMFreeRDP.GetDialogFreeRDPPreferences: TDialogFreeRDPPreferences;
begin
  if not Assigned(FDialogFreeRDPPreferences) then
    FDialogFreeRDPPreferences := TDialogFreeRDPPreferences.Create(Self);
  Result := FDialogFreeRDPPreferences;
end;

procedure TDMFreeRDP.SaveOptions;
begin
  Preferences.SaveToIniFile(FIniFile);
end;

procedure TDMFreeRDP.EditOptions;
begin
  DialogFreeRDPPreferences.Preferences := Preferences;
  if DialogFreeRDPPreferences.Execute then
  begin
    Preferences.Assign(DialogFreeRDPPreferences.Preferences);
    SaveOptions;
  end;
end;

end.
