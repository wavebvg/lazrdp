unit UDialogFreeRDPOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  UFreeRDPOptions,
  UFormDialogFreeRDPOptions;

type

  { TDialogFreeRDPOptions }

  TDialogFreeRDPOptions = class(TComponent)
  private
    function GetForm: TFormDialogFreeRDPOptions;
  private
    FOptions: TFreeRDPConnectionOptions;
    procedure SetOptions(const AValue: TFreeRDPConnectionOptions);
    property Form: TFormDialogFreeRDPOptions read GetForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    procedure Reset;
  published
    property Options: TFreeRDPConnectionOptions read FOptions write SetOptions;
  end;

implementation

uses
  Controls;

{ TDialogFreeRDPOptions }

constructor TDialogFreeRDPOptions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := TFreeRDPConnectionOptions.Create;
end;

destructor TDialogFreeRDPOptions.Destroy;
begin
  FOptions.Free;
  inherited Destroy;
end;

function TDialogFreeRDPOptions.GetForm: TFormDialogFreeRDPOptions;
begin
  if not Assigned(FormDialogFreeRDPOptions) then
    FormDialogFreeRDPOptions := TFormDialogFreeRDPOptions.Create(Self);
  Result := FormDialogFreeRDPOptions;
end;

procedure TDialogFreeRDPOptions.SetOptions(const AValue: TFreeRDPConnectionOptions);
begin
  if FOptions = AValue then
    Exit;
  FOptions.Assign(AValue);
end;

function TDialogFreeRDPOptions.Execute: Boolean;
begin
  Form.Options:= Options;
  Result := Form.ShowModal = mrOk;
  if Result then
    Options := Form.Options;
end;

procedure TDialogFreeRDPOptions.Reset;
begin
  Options.Reset;
end;

end.
