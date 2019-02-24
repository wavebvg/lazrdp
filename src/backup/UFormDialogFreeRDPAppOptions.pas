unit UFormDialogFreeRDPAppOptions;

{$mode objfpc}{$H+}

interface

uses
  UFreeRDPOptions,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls;

type

  { TFormDialogFreeRDPAppOptions }

  TFormDialogFreeRDPAppOptions = class(TForm)
    ButtonCancel: TButton;
    ButtonOk: TButton;
    CheckBoxGrabKeyboard: TCheckBox;
    PageControlOptions: TPageControl;
    PanelButtons: TPanel;
    TabSheetCommon: TTabSheet;
  private
    FAppOptions: TFreeRDPAppOptions;
    procedure SetAppOptions(const AValue: TFreeRDPAppOptions);

  public
    procedure Init;
    procedure Deinit;

    property AppOptions: TFreeRDPAppOptions read FAppOptions write SetAppOptions;
  end;

var
  FormDialogFreeRDPAppOptions: TFormDialogFreeRDPAppOptions;

implementation

{$R *.lfm}

{ TFormDialogFreeRDPAppOptions }

procedure TFormDialogFreeRDPAppOptions.SetAppOptions(const AValue: TFreeRDPAppOptions);
begin
  if FAppOptions = AValue then
    Exit;
  FAppOptions := AValue;
end;

procedure TFormDialogFreeRDPAppOptions.Init;
begin
  CheckBoxGrabKeyboard.Checked := AppOptions.GrabKeyboard;
end;

procedure TFormDialogFreeRDPAppOptions.Deinit;
begin
  AppOptions.GrabKeyboard
end;

end.

