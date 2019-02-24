unit UFrameViewFreeRDP;

{$mode objfpc}{$H+}

interface

uses
  UFreeRDP,
  Classes,
  SysUtils,
  Forms,
  Controls,
  UFreeRDPOptions;

type

  { TFrameViewFreeRDP }

  TFrameViewFreeRDP = class(TFrame)
    ScrollBox: TScrollBox;
  private
    FFreeRDP: TFreeRDP;
    function GetActive: Boolean;
    function GetOnXEnter: TNotifyEvent;
    function GetOnXExit: TNotifyEvent;
    procedure SetActive(const AValue: Boolean);
    procedure SetOnXEnter(const AValue: TNotifyEvent);
    procedure SetOnXExit(const AValue: TNotifyEvent);
    procedure FreeRDPResize(Sender: TObject);
  protected 
    procedure UpdateFreeRDPPosition;
    procedure Resize; override;
  public
    constructor Create(TheOwner: TComponent); override;

    property FreeRDP: TFreeRDP read FFreeRDP;
    property Active: Boolean read GetActive write SetActive;
    property OnXEnter: TNotifyEvent read GetOnXEnter write SetOnXEnter;
    property OnXExit: TNotifyEvent read GetOnXExit write SetOnXExit;
  end;

implementation

{$R *.lfm}

{ TFrameViewFreeRDP }

constructor TFrameViewFreeRDP.Create(TheOwner: TComponent);
begin
  FFreeRDP := TFreeRDP.Create(Self);
  inherited Create(TheOwner);
  FFreeRDP.Parent := ScrollBox;
  FFreeRDP.OnResize := @FreeRDPResize;
end;

procedure TFrameViewFreeRDP.SetActive(const AValue: Boolean);
begin
  FFreeRDP.Active := AValue;
end;

function TFrameViewFreeRDP.GetOnXEnter: TNotifyEvent;
begin
  Result := FFreeRDP.OnXEnter;
end;

function TFrameViewFreeRDP.GetActive: Boolean;
begin
  Result := FFreeRDP.Active;
end;

function TFrameViewFreeRDP.GetOnXExit: TNotifyEvent;
begin
  Result := FFreeRDP.OnXExit;
end;

procedure TFrameViewFreeRDP.SetOnXEnter(const AValue: TNotifyEvent);
begin
  FFreeRDP.OnXEnter := AValue;
end;

procedure TFrameViewFreeRDP.SetOnXExit(const AValue: TNotifyEvent);
begin
  FFreeRDP.OnXExit := AValue;
end;

procedure TFrameViewFreeRDP.SetOptions(const AValue: TFreeRDPConnectionOptions);
begin
  FFreeRDP.Options := AValue;
  UpdateFreeRDPPosition;
end;

procedure TFrameViewFreeRDP.FreeRDPResize(Sender: TObject);
begin
  UpdateFreeRDPPosition;
end;

procedure TFrameViewFreeRDP.UpdateFreeRDPPosition;
var
  VLeft, VTop: Integer;
begin
  if Options.UseCustomResolution then
  begin
    FreeRDP.Align := alCustom;
    VLeft := Round((Width - FreeRDP.Width) / 2);
    if VLeft < 0 then
      VLeft := 0;
    VTop := Round((Height - FreeRDP.Height) / 2);
    if VTop < 0 then
      VTop := 0;
    FreeRDP.SetBounds(VLeft, VTop, FreeRDP.Width, FreeRDP.Height);
  end
  else
  begin
    FreeRDP.Align := alClient;
  end;
end;

procedure TFrameViewFreeRDP.Resize;
begin
  inherited Resize;
  UpdateFreeRDPPosition;
end;

end.
