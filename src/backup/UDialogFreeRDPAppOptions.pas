unit UDialogFreeRDPAppOptions;

{$mode objfpc}{$H+}

interface

uses
  UFormDialogFreeRDPAppOptions,
  UFreeRDPOptions,
  Classes, SysUtils;

type

  { TDialogFreeRDPAppOptions }

  TDialogFreeRDPAppOptions = class(TComponent)
  private
    function GetForm: TFormDialogFreeRDPAppOptions;
  private
    FAppOptions: TFreeRDPAppOptions;
    procedure SetAppOptions(const AValue: TFreeRDPAppOptions);
    property Form: TFormDialogFreeRDPAppOptions read GetForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    procedure Reset;
  published
    property AppOptions: TFreeRDPAppOptions read FAppOptions write SetAppOptions;
  end;

implementation

uses
  Controls;

{ TDialogFreeRDPAppOptions }

constructor TDialogFreeRDPAppOptions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAppOptions := TFreeRDPAppOptions.Create(Self);
end;

destructor TDialogFreeRDPAppOptions.Destroy;
begin
  inherited Destroy;
end;

function TDialogFreeRDPAppOptions.GetForm: TFormDialogFreeRDPAppOptions;
begin
  if not Assigned(FormDialogFreeRDPAppOptions) then
    FormDialogFreeRDPAppOptions := TFormDialogFreeRDPAppOptions.Create(Self);
  Result := FormDialogFreeRDPAppOptions;
end;

procedure TDialogFreeRDPAppOptions.SetAppOptions(const AValue: TFreeRDPAppOptions);
begin
  AppOptions.Assign(AValue);
end;

function TDialogFreeRDPAppOptions.Execute: Boolean;
begin
  Form.AppOptions.Assign(AppOptions);
  Form.Init;
  try
    Result := Form.ShowModal = mrOk;
  finally
    Form.Deinit;
  end;
  if Result then
    AppOptions.Assign(Form.AppOptions);
end;

procedure TDialogFreeRDPAppOptions.Reset;
begin
  AppOptions.Reset;
end;

end.


