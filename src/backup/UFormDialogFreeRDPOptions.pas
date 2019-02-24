unit UFormDialogFreeRDPOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ComCtrls,
  ExtCtrls,
  StdCtrls, ExtDlgs, Menus, RTTICtrls,
  UFreeRDPOptions;

type

  { TFormDialogFreeRDPOptions }

  TFormDialogFreeRDPOptions = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    CheckBoxRFX: TTICheckBox;
    CheckBoxUseAuthentication: TTICheckBox;
    CheckBoxUseResolution: TTICheckBox;
    CheckBoxClipboard: TTICheckBox;
    ComboBoxCodecCash: TTIComboBox;
    ComboBoxRFXMode: TTIComboBox;
    ComboBoxSec: TTIComboBox;
    ComboBoxGDI: TTIComboBox;
    EditResolution: TTIEdit;
    EditResolution1: TEdit;
    ImageIcon: TImage;
    LabelCodecCash: TLabel;
    LabelRFXMode: TLabel;
    LabelSec: TLabel;
    LabelGDI: TLabel;
    LabelServer: TLabel;
    LabelUser: TLabel;
    LabelPassword: TLabel;
    LabelResolution: TLabel;
    MenuItemSelect: TMenuItem;
    MenuItemClear: TMenuItem;
    OpenPictureDialog: TOpenPictureDialog;
    PanelIcon: TPanel;
    PanelDisplayCodecs: TPanel;
    PanelDisplayMode: TPanel;
    PanelDisplaySec: TPanel;
    PanelServer: TPanel;
    PanelSecurity: TPanel;
    PanelUseAuthentication: TPanel;
    PanelConnection: TPanel;
    PanelUseCustomResolution: TPanel;
    PopupMenuIcon: TPopupMenu;
    PropertyLinkMain: TMultiPropertyLink;
    PageControlFreeRDPOptions: TPageControl;
    PanelName: TPanel;
    PanelButtons: TPanel;
    TabSheetDisplay: TTabSheet;
    TabSheetSecurity: TTabSheet;
    TabSheetConnection: TTabSheet;
    EditConnectionName: TTIEdit;
    EditUser: TTIEdit;
    EditPassword: TTIEdit;
    EditServer: TTIEdit;
    CheckBoxSecExt: TTICheckBox;
    CheckBoxSecNLA: TTICheckBox;
    CheckBoxSecRDP: TTICheckBox;
    CheckBoxSecTLS: TTICheckBox;
    procedure CheckBoxRFXPropertyLinkAfterWrite(Sender: TObject);
    procedure CheckBoxUseAuthenticationPropertyLinkAfterWrite(Sender: TObject);
    procedure CheckBoxUseResolutionPropertyLinkAfterWrite(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure ComboBoxSecPropertyLinkAfterWrite(Sender: TObject);
    procedure MenuItemClearClick(Sender: TObject);
    procedure PanelIconClick(Sender: TObject);
  private
    FOptions: TFreeRDPConnectionOptions;
    procedure SetOptions(const AValue: TFreeRDPConnectionOptions);
    procedure UpdateIcon;
  protected
    procedure Loaded; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property Options: TFreeRDPConnectionOptions read FOptions write SetOptions;
  end;

var
  FormDialogFreeRDPOptions: TFormDialogFreeRDPOptions;

implementation

const
  SecAliases: array[TFreeRDPSecOption] of String = (
    {soUndefined} 'Default',
    {soRDP}       'RDP',
    {soTLS}       'TLS',
    {soNLA}       'NLA',
    {soExt}       'NLA extended'
    );
  GDIAliases: array[TFreeRDPGDIOption] of String = (
    {gdioUndefined} 'Default',
    {gdioHW}        'Hardware',
    {gdioSW}        'Software'
    );
  CodecCashAliases: array[TFreeRDPCodecCashOption] of String = (
    {ccoUndefined} 'Default',
    {ccoRFX}       'RFX',
    {ccoNSC}       'NSC',
    {ccoJPEG}      'JPEG'
    );
  RFXModeAliases: array[TFreeRDPRFXModeOption] of String = (
    {rfxmoUndefined} 'Default',
    {rfxmoImage}     'Image',
    {rfxmoVideo}     'Video'
    );

{$R *.lfm}

{ TFormDialogFreeRDPOptions }

constructor TFormDialogFreeRDPOptions.Create(TheOwner: TComponent);
var
  TIObject: TFreeRDPConnectionOptions;
begin
  inherited Create(TheOwner);
  CreateEnumAliasValues(TypeInfo(TFreeRDPSecOption), ComboBoxSec.Link.AliasValues, SecAliases);
  CreateEnumAliasValues(TypeInfo(TFreeRDPGDIOption), ComboBoxGDI.Link.AliasValues, GDIAliases);
  CreateEnumAliasValues(TypeInfo(TFreeRDPCodecCashOption), ComboBoxCodecCash.Link.AliasValues, CodecCashAliases);
  CreateEnumAliasValues(TypeInfo(TFreeRDPRFXModeOption), ComboBoxRFXMode.Link.AliasValues, RFXModeAliases);
  FOptions := TFreeRDPConnectionOptions.Create;
  PropertyLinkMain.TIObject := Options;
end;

destructor TFormDialogFreeRDPOptions.Destroy;
begin
  FOptions.Free;
  inherited Destroy;
end;

procedure TFormDialogFreeRDPOptions.Loaded;
begin
  inherited Loaded;
end;

procedure TFormDialogFreeRDPOptions.CheckBoxUseResolutionPropertyLinkAfterWrite(Sender: TObject);
begin
  PanelUseCustomResolution.Enabled := Options.UseCustomResolution;
end;

procedure TFormDialogFreeRDPOptions.ComboBoxChange(Sender: TObject);
begin
  TCustomComboBox(Sender).EditingDone;
end;

procedure TFormDialogFreeRDPOptions.ComboBoxSecPropertyLinkAfterWrite(Sender: TObject);
begin
  PanelDisplaySec.Enabled := Options.Sec = soUndefined;
end;

procedure TFormDialogFreeRDPOptions.MenuItemClearClick(Sender: TObject);
begin
  Options.IconFileName := '';
  UpdateIcon;
end;

procedure TFormDialogFreeRDPOptions.PanelIconClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    Options.IconFileName := OpenPictureDialog.FileName;
    UpdateIcon;
  end;
end;

procedure TFormDialogFreeRDPOptions.CheckBoxUseAuthenticationPropertyLinkAfterWrite
  (Sender: TObject);
begin
  PanelUseAuthentication.Enabled := Options.UseAuthentication;
end;

procedure TFormDialogFreeRDPOptions.CheckBoxRFXPropertyLinkAfterWrite(Sender: TObject);
begin
  PanelDisplayMode.Enabled := Options.RFX;
end;

procedure TFormDialogFreeRDPOptions.SetOptions(const AValue: TFreeRDPConnectionOptions);
begin
  if FOptions = AValue then
    Exit;
  FOptions.Assign(AValue);
  PropertyLinkMain.SetLinks;
  ComboBoxSecPropertyLinkAfterWrite(nil);
  UpdateIcon;
end;

procedure TFormDialogFreeRDPOptions.UpdateIcon;
begin
  if FileExists(Options.IconFileName) then
  begin
    try
      ImageIcon.Picture.LoadFromFile(Options.IconFileName);
    except
      on E: Exception do
        WriteLn(E.ClassName);
    end;
  end
  else
    ImageIcon.Picture.Clear;
end;

end.
