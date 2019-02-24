unit UDialogFreeRDPPreferences;

{$mode objfpc}{$H+}

interface

uses
  UFormDialogFreeRDPPreferences,
  UFreeRDPOptions,
  Classes, SysUtils;

type

  { TDialogFreeRDPPreferences }

  TDialogFreeRDPPreferences = class(TComponent)
  private
    function GetForm: TFormDialogFreeRDPPreferences;
  private
    FPreferences: TFreeRDPPreferences;
    procedure SetPreferences(const AValue: TFreeRDPPreferences);
    property Form: TFormDialogFreeRDPPreferences read GetForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    procedure Reset;
  published
    property Preferences: TFreeRDPPreferences read FPreferences write SetPreferences;
  end;

implementation

uses
  Controls;

{ TDialogFreeRDPPreferences }

constructor TDialogFreeRDPPreferences.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPreferences := TFreeRDPPreferences.Create(Self);
end;

destructor TDialogFreeRDPPreferences.Destroy;
begin
  inherited Destroy;
end;

function TDialogFreeRDPPreferences.GetForm: TFormDialogFreeRDPPreferences;
begin
  if not Assigned(FormDialogFreeRDPPreferences) then
    FormDialogFreeRDPPreferences := TFormDialogFreeRDPPreferences.Create(Self);
  Result := FormDialogFreeRDPPreferences;
end;

procedure TDialogFreeRDPPreferences.SetPreferences(const AValue: TFreeRDPPreferences);
begin
  Preferences.Assign(AValue);
end;

function TDialogFreeRDPPreferences.Execute: Boolean;
begin
  Form.Preferences := Preferences;
  Result := Form.ShowModal = mrOk;
  if Result then
    Preferences := Form.Preferences;
end;

procedure TDialogFreeRDPPreferences.Reset;
begin
  Preferences.Reset;
end;

end.


