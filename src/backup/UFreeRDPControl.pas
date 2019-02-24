unit UFreeRDPControl;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Controls,
  Xlib,
  glib2,
  Forms,
  unixtype,
  x,
  gtk2,
  gdk2,
  UTF8Process,
  UFreeRDPOptions,
  ExtCtrls,
  Graphics,
  fgl, LMessages;

type

  { TFreeRDP }

  TFreeRDP = class(TWinControl)
  private
    FOnRunned: TNotifyEvent;
    FOutLines: TStrings;
    FOutBuffer: TMemoryStream;
    FTimer: TTimer;
    FFreeRDPPath: String;
    FOption: TFreeRDPConnectionOptions;
    FProcess: TProcessUTF8;
    function GetActive: Boolean;
    //function GetFreeRDPXWindow: TWindow;
    procedure SetActive(const AValue: Boolean);
    procedure SetOptions(const AValue: TFreeRDPConnectionOptions);
    procedure Stop;
    procedure TimerTimer(Sender: TObject);
    procedure ParseOutputResult;
    //procedure UpdateFixedDrawPosition;
    procedure DrawControlRunned(Sender: TObject);
  protected
    procedure DestroyHandle; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Resize; override;
    procedure Run;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocus; override;
    property Active: Boolean read GetActive write SetActive;
    //property FreeRDPXWindow: TWindow read GetFreeRDPXWindow;
    procedure Test;
  published
    property FreeRDPPath: String read FFreeRDPPath write FFreeRDPPath;
    property Options: TFreeRDPConnectionOptions read FOption write SetOptions;
    property OnRunned: TNotifyEvent read FOnRunned write FOnRunned;
  end;

implementation

uses
  Process,
  gtk2int,
  gdk2x,
  gdk2pixbuf,
  GTK2Proc,
  Gtk2Def,
  Gtk2WSControls,
  Pipes,
  WSLCLClasses,
  LCLType,
  Gtk2WSForms,
  Gtk2WSStdCtrls,
  InterfaceBase,
  xkblib,
  UFreeRDPUtils,
  LCLIntf;

type

  { TWSFreeRDP }

  TWSFreeRDP = class(TGtk2WSWinControl)
  published 
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
  end;

function FreeRDPWidgetDestroy(Widget: PGtkWidget; {%H-}Data: gPointer): GBoolean; cdecl;
begin
  FreeWidgetInfo(Widget);
  Result := True;
end;

//{ TFreeRDPControl }
//
//constructor TFreeRDPControl.Create(TheOwner: TComponent);
//begin
//  inherited Create(TheOwner);
//  FCanvas := TControlCanvas.Create;
//  TControlCanvas(FCanvas).Control := Self;
//end;
//
//destructor TFreeRDPControl.Destroy;
//begin
//  FCanvas.Free;
//  //FThread.Free;
//  inherited Destroy;
//end;
//
//procedure TFreeRDPControl.UMDoListen(var Message: TLMessage);
//begin
//  //if not Assigned(FThread) then
//  //  FThread := TFreeRDPXEventsThread.Create(Self);
//end;
//
//procedure TFreeRDPControl.UMUngrab(var Message: TLMessage);
//var
//  VForm: TCustomForm;
//begin
//  VForm := GetParentForm(Self);
//  GrabKeyBoardToForm(VForm);
//end;
//
//procedure TFreeRDPControl.UMGrub(var Message: TLMessage);
//var
//  VForm: TCustomForm;
//begin
//  VForm := GetParentForm(Self);
//  ReleaseKeyBoardFromForm(VForm);
//end;
//
//procedure TFreeRDPControl.UMMapFreeRDPXWindow(var Message: TLMessage);
//begin
//  FFreeRDPXWindow := Message.LParam;
//  DoRunned;
//end;
//
//procedure TFreeRDPControl.UMUnMapFreeRDPXWindow(var Message: TLMessage);
//begin
//  FFreeRDPXWindow := 0;
//end;
//
//procedure TFreeRDPControl.DoRunned;
//begin
//  if Assigned(FOnRunned) then
//    FOnRunned(Self);
//end;
//
//procedure TFreeRDPControl.Resize;
//begin
//  inherited Resize;
//  WriteLn(Width, 'x', Height);
//end;
//
//procedure TFreeRDPControl.DestroyHandle;
//begin
//  //FThread.WaitFor;
//  inherited DestroyHandle;
//end;

{ TWSFreeRDP }

class function TWSFreeRDP.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  NewWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  if csDesigning in AWinControl.ComponentState then
    Result := inherited CreateHandle(AWinControl, AParams)
  else
  begin
    NewWidget := gtk_event_box_new;

    gtk_widget_set_events(NewWidget, GDK_ALL_EVENTS_MASK);
    GTK_WIDGET_SET_FLAGS(NewWidget, GTK_CAN_FOCUS);

    WidgetInfo := GetWidgetInfo(NewWidget, True);
    WidgetInfo^.LCLObject := AWinControl;
    WidgetInfo^.Style := AParams.Style;
    WidgetInfo^.ExStyle := AParams.ExStyle;
    WidgetInfo^.WndProc := {%H-}PtrUInt(AParams.WindowClass.lpfnWndProc);

    // set allocation
    Allocation.X := AParams.X;
    Allocation.Y := AParams.Y;
    Allocation.Width := AParams.Width;
    Allocation.Height := AParams.Height;
    gtk_widget_size_allocate(NewWidget, @Allocation);

    if csDesigning in AWinControl.ComponentState then
    begin
      // at designtime setup normal handlers
      TGtk2WidgetSet(WidgetSet).FinishCreateHandle(AWinControl, NewWidget, AParams);
    end
    else
    begin
      // at runtime
      g_signal_connect(GPointer(NewWidget), 'destroy',
        TGTKSignalFunc(@FreeRDPWidgetDestroy), WidgetInfo);
    end;
    Result := HWND({%H-}PtrUInt(Pointer(NewWidget)));
  end;
end;

{ TFreeRDP }

constructor TFreeRDP.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  //
  FProcess := TProcessUTF8.Create(Self);
  FProcess.Options := FProcess.Options + [poUsePipes, poStderrToOutPut];
  //
  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csDoubleClicks,
    csRequiresKeyboardInput] - [csSetCaption, csNoFocus, csNeedsBorderPaint];
  BorderStyle := bsNone;
  BorderWidth := 0;

  Color := clRed;
  FOutBuffer := TMemoryStream.Create;
  FOutLines := TStringList.Create;
  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := @TimerTimer;
  FTimer.Interval := 100;
  FOption := TFreeRDPConnectionOptions.Create;
  FFreeRDPPath := 'xfreerdp';
end;

destructor TFreeRDP.Destroy;
begin
  Stop;
  FOption.Free;
  FOutLines.Free;
  FOutBuffer.Free;
  FProcess.Free;
  inherited Destroy;
end;

procedure TFreeRDP.SetFocus;
begin
  inherited SetFocus;
end;

procedure TFreeRDP.TimerTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  try
    FTimer.Interval := 100;
    ParseOutputResult;
  finally
    FTimer.Enabled := True;
  end;
end;

procedure TFreeRDP.Stop;
begin
  if Assigned(FProcess) then
  begin
    FProcess.Terminate(0);
    FProcess.WaitOnExit;
  end;
end;

function TFreeRDP.GetActive: Boolean;
begin
  Result := Assigned(FProcess) and FProcess.Running;
end;

procedure TFreeRDP.SetActive(const AValue: Boolean);
begin
  if Active xor AValue then
  begin
    if AValue then
      Run
    else
      Stop;
  end;
end;

procedure TFreeRDP.SetOptions(const AValue: TFreeRDPConnectionOptions);
begin
  if FOption = AValue then
    Exit;
  FOption.Assign(AValue);
  if Options.UseCustomResolution and (Options.Width > 0) and (Options.Height > 0) then
  begin
    Align := alNone;
    Width := Options.Width;
    Height := Options.Height;
  end else
    Align := alClient;
end;

procedure TFreeRDP.Run;

  procedure AddParam(const AFormat: String; const AArgs: array of const);
  begin
    FProcess.Parameters.Add(Format(AFormat, AArgs));
  end;

begin
  Stop;
  FProcess.Executable := FreeRDPPath;
  FProcess.Parameters.Clear;
  FProcess.Parameters.Delimiter := ' ';
  AddParam('/v:%s', [Options.Host]);
  if Options.Port <> DEFAULT_RDP_PORT then
    AddParam('/port:%d', [Options.Port]);
  if Options.UseCustomResolution and (Options.Width > 0) and (Options.Height > 0) then
  begin
    AddParam('/w:%d', [Options.Width]);
    AddParam('/h:%d', [Options.Height]);
  end
  else
  begin
    AddParam('/w:%d', [Width]);
    AddParam('/h:%d', [Height]);
    AddParam('/dynamic-resolution', []);
  end;
  if Options.UseAuthentication then
  begin
    if Options.User <> '' then
      AddParam('/u:%s', [Options.User]);
    if Options.Password <> '' then
      AddParam('/p:%s', [Options.Password]);
  end;
  AddParam('/parent-window:%d', [GetXWindow(Self)]);

  FProcess.Execute;
  ParseOutputResult;
end;

procedure TFreeRDP.Test;
begin
  WriteLn('Test');
  SetFocus;
end;

procedure TFreeRDP.ParseOutputResult;
const
  ERROR_TEXT = ' ERR';
var
  i: Integer;
  VLine, VError: String;
  VPos: Integer;
begin
  if not Assigned(FProcess) then
    Exit;
  if not Assigned(FProcess.Output) then
    Exit;
  FOutBuffer.Size := FProcess.Output.NumBytesAvailable + 1;
  if (FOutBuffer.Size > 0) and (FProcess.Output.Read((FOutBuffer.Memory)^, FOutBuffer.Size - 1) > 0) then
  begin
    PChar(FOutBuffer.Memory)[FOutBuffer.Size - 1] := #0;
    FOutLines.Text := StrPas(PChar(FOutBuffer.Memory));
    for i := 0 to FOutLines.Count - 1 do
    begin
      VLine := FOutLines[i];
      WriteLn(VLine);
      VPos := Pos(ERROR_TEXT, VLine);
      if VPos > 0 then
      begin
        Inc(VPos);
        VError := Copy(VLine, VPos, Length(VLine) - VPos + 1);
        raise EFreeRDP.Create(Self, VError);
      end;
    end;
  end;
end;

//procedure TFreeRDP.UpdateFixedDrawPosition;
//var
//  VLeft, VTop: Integer;
//begin
//  if not Active then
//    Exit;
//  if Options.UseCustomResolution then
//  begin
//    FDrawControl.Align := alCustom;
//    VLeft := Round((Width - Options.Width) / 2);
//    if VLeft < 0 then
//      VLeft := 0;
//    VTop := Round((Height - Options.Height) / 2);
//    if VTop < 0 then
//      VTop := 0;
//    FDrawControl.SetBounds(VLeft, VTop, Options.Width, Options.Height);
//  end
//  else
//  begin
//    FDrawControl.Align := alClient;
//  end;
//end;

procedure TFreeRDP.DoEnter;
begin
  inherited DoEnter;
end;

procedure TFreeRDP.DoExit;
begin
  inherited DoExit;
end;

procedure TFreeRDP.Resize;
begin
  inherited Resize;
  //UpdateFixedDrawPosition;
end;

procedure TFreeRDP.DrawControlRunned(Sender: TObject);
begin
  if Assigned(FOnRunned) then
    FOnRunned(Self);
end;

procedure TFreeRDP.DestroyHandle;
begin
  Stop;
  inherited DestroyHandle;
end;

initialization
  RegisterWSComponent(TFreeRDP, TWSFreeRDP);

end.
