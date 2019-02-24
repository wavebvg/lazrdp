unit UFormDialogFreeRDPPreferences;

{$mode objfpc}{$H+}

interface

uses
  UFreeRDPOptions,
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ComCtrls,
  ExtCtrls,
  StdCtrls,
  RTTICtrls;

type

  { TFormDialogFreeRDPPreferences }

  TFormDialogFreeRDPPreferences = class(TForm)
    ButtonPath: TButton;
    ButtonCancel: TButton;
    ButtonOk: TButton;
    MultiPropertyLink: TMultiPropertyLink;
    OpenDialog: TOpenDialog;
    PageControlOptions: TPageControl;
    PanelButtons: TPanel;
    TabSheetCommon: TTabSheet;
    CheckBoxGrabKeyboard: TTICheckBox;
    EditFreeRDPPath: TTIEdit;
    procedure ButtonPathClick(Sender: TObject);
  private
    FPreferences: TFreeRDPPreferences;
    procedure SetPreferences(const AValue: TFreeRDPPreferences);
  public
    constructor Create(TheOwner: TComponent); override;
    property Preferences: TFreeRDPPreferences read FPreferences write SetPreferences;
  published
  end;

var
  FormDialogFreeRDPPreferences: TFormDialogFreeRDPPreferences;

implementation

{$R *.lfm}

{ TFormDialogFreeRDPPreferences }

constructor TFormDialogFreeRDPPreferences.Create(TheOwner: TComponent);
begin
  FPreferences := TFreeRDPPreferences.Create(Self);
  inherited Create(TheOwner);
  MultiPropertyLink.TIObject := Preferences;
end;

procedure TFormDialogFreeRDPPreferences.ButtonPathClick(Sender: TObject);
begin
  OpenDialog.FileName := Preferences.FreeRDPPath;
  if OpenDialog.Execute then
    Preferences.FreeRDPPath := OpenDialog.FileName;
end;

procedure TFormDialogFreeRDPPreferences.SetPreferences(const AValue: TFreeRDPPreferences);
begin
  if FPreferences = AValue then
    Exit;
  FPreferences.Assign(AValue);
  MultiPropertyLink.SetLinks;
end;

end.

