unit UFreeRDP;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Controls,
  glib2,
  Forms,
  Xlib,
  LCLType,
  unixtype,
  x,
  gdk2,
  UTF8Process,
  UFreeRDPOptions,
  ExtCtrls,
  fgl,
  LMessages;

const
  UM_FREERDP = LM_USER + 434;
  UM_GRAB = UM_FREERDP;
  UM_UNGRAB = UM_FREERDP + 1;
  UM_MAP = UM_FREERDP + 2;
  UM_UNMAP = UM_FREERDP + 3;

type

  { TMapKeySymEvent }

  TMapKeySymEvent = class(specialize TFPGMap<cuint, TXEvent>)
  public
    function IsMember(const AEvent: TXEvent): Boolean;
    procedure Include(const AEvent: TXEvent);
    procedure Exclude(const AEvent: TXEvent);
  end;

  { TFreeRDP }

  TFreeRDP = class(TWinControl)
  private
    FOldFocusControl: TWinControl;
    FKeyPressEvent: guint;
    FKeyReleaseEvent: guint;
    FOnXEnter: TNotifyEvent;
    FOnXExit: TNotifyEvent;
    FXWindow: TWindow;
    FGrabbed: Boolean;
    FFocusThread: TThread;
    FPressedKeys: TMapKeySymEvent;
    FWindowHack: TWinControl;
    FOutLines: TStrings;
    FOutBuffer: TMemoryStream;
    FTimer: TTimer;
    FFreeRDPPath: String;
    FOption: TFreeRDPConnectionOptions;
    FProcess: TProcessUTF8;
    FUnGrabTimer: TTimer;
    function GetActive: Boolean;
    procedure ProcessKeyEvent(const AEvent: PGdkEvent);
    procedure SendKeyEvent(AEvent: TXEvent);
    procedure SetActive(const AValue: Boolean);
    procedure SetOptions(const AValue: TFreeRDPConnectionOptions);
    procedure Stop;
    procedure Run;
    procedure TimerTimer(Sender: TObject);
    procedure ParseOutputResult;
    procedure ReleaseAllKeys;
    procedure DoXEnter;
    procedure DoXExit;

    procedure UMGrab(var AMsg: TLMessage); message UM_GRAB;
    procedure UMUngrab(var AMsg: TLMessage); message UM_UNGRAB;
    procedure UMMap(var AMsg: TLMessage); message UM_MAP;
    procedure UMUnmap(var AMsg: TLMessage); message UM_UNMAP;
    procedure BindKeyboardListener;
    procedure UnbindKeyboardListener;
    procedure UnGrabTimerTimer(Sender: TObject);
    procedure UpdateSmartSizing;
  protected
    procedure Resize; override;
    procedure DestroyHandle; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Active: Boolean read GetActive write SetActive;
  published
    property FreeRDPPath: String read FFreeRDPPath write FFreeRDPPath;
    property Options: TFreeRDPConnectionOptions read FOption write SetOptions;
    property OnXEnter: TNotifyEvent read FOnXEnter write FOnXEnter;
    property OnXExit: TNotifyEvent read FOnXExit write FOnXExit;
  end;

  { EFreeRDP }

  EFreeRDP = class(Exception)
  private
    FControl: TObject;
  public
    constructor Create(const AControl: TObject; const msg: String); overload;
    constructor CreateFmt(const AControl: TObject; const msg: String; const args: array of const); overload;
    destructor Destroy; override;

    property Control: TObject read FControl;
  end;

function GetXWindow(const AHandle: HWND): TWindow;

implementation

uses
  Process,
  gtk2,
  gdk2x,
  GTK2Proc,
  Gtk2Def,
  gtk2int,
  Graphics,
  LCLIntf,
  InterfaceBase,
  Gtk2WSControls,
  WSLCLClasses,
  syncobjs;

const
  SecCommandValues: array[TFreeRDPSecOption] of String = (
    {soUndefined} '',
    {soRDP}       'rdp',
    {soTLS}       'tls',
    {soNLA}       'nla',
    {soExt}       'ext'
    );
  GDICommandValues: array[TFreeRDPGDIOption] of String = (
    {gdioUndefined} '',
    {gdioHW}       'hw',
    {gdioSW}       'sh'
    );
  CodecCashCommandValues: array[TFreeRDPCodecCashOption] of String = (
    {ccoUndefined} '',
    {ccoRFX}       'rfx',
    {ccoNSC}       'nsc',
    {ccoJPEG}      'jpeg'
    );
  RFXModeCommandValues: array[TFreeRDPRFXModeOption] of String = (
    {rfxmoUndefined} '',
    {rfxmoImage}     'image',
    {rfxmoVideo}     'video'
    );

type

  { TFreeRDPFocusThread }

  TFreeRDPFocusThread = class(TThread)
  private
    FStartEvent: TEvent;
    FDisplay: PDisplay;
    FControlHandle: HWND;
  protected
    procedure Execute; override;
  public
    constructor Create(const AControlHandle: HWND);
    destructor Destroy; override;
  end;

  { TWSFreeRDP }

  TWSFreeRDP = class(TGtk2WSWinControl)
  private
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

function FreeRDPWidgetDestroy(Widget: PGtkWidget; {%H-}Data: gPointer): GBoolean; cdecl;
begin
  FreeWidgetInfo(Widget);
  Result := True;
end;

function GetXWindow(const AHandle: HWND): TWindow;
var
  VWidget: PGtkWidget;
  VWindow: PGdkWindow;
begin
  VWidget := PGtkWidget(AHandle);
  VWindow := VWidget^.window;
  Result := GDK_WINDOW_XWINDOW(VWindow);
end;

procedure SendGdkKeyEvent(const AEvent: PGdkEvent; const AWindow: TWindow);
var
  VScreen: PGdkScreen;
  VEvent: TXEvent;
begin
  VScreen := gdk_screen_get_default();
  FillChar(VEvent, SizeOf(VEvent), 0);
  if AEvent^._type = GDK_KEY_PRESS then
    VEvent._type := KeyPress
  else
    VEvent._type := KeyRelease;
  VEvent.xkey.root := GDK_WINDOW_XID(gdk_screen_get_root_window(VScreen));
  VEvent.xkey.window := AWindow;
  VEvent.xkey.subwindow := None;
  VEvent.xkey.time := AEvent^.key.time;
  VEvent.xkey.state := AEvent^.key.state;
  VEvent.xkey.keycode := AEvent^.key.hardware_keycode;

  XSendEvent(gdk_display, AWindow, False, NoEventMask, @VEvent);
end;

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

class procedure TWSFreeRDP.DestroyHandle(const AWinControl: TWinControl);
begin
  FreeWidgetInfo(Pointer(AWinControl.Handle));
  inherited DestroyHandle(AWinControl);
end;

{ TMapKeySymEvent }

function TMapKeySymEvent.IsMember(const AEvent: TXEvent): Boolean;
var
  VKeySym: TKeySym;
begin
  VKeySym := XKeycodeToKeysym(gdk_display, AEvent.xkey.keycode, CurrentTime);
  Result := IndexOf(VKeySym) >= 0;
end;

procedure TMapKeySymEvent.Include(const AEvent: TXEvent);
var
  VKeySym: TKeySym;
begin
  VKeySym := XKeycodeToKeysym(gdk_display, AEvent.xkey.keycode, CurrentTime);
  KeyData[VKeySym] := AEvent;
end;

procedure TMapKeySymEvent.Exclude(const AEvent: TXEvent);
var
  VKeySym: TKeySym;
begin
  VKeySym := XKeycodeToKeysym(gdk_display, AEvent.xkey.keycode, CurrentTime);
  Remove(VKeySym);
end;

{ EFreeRDP }

constructor EFreeRDP.Create(const AControl: TObject; const msg: String);
begin
  CreateFmt(AControl, msg, []);
end;

constructor EFreeRDP.CreateFmt(const AControl: TObject; const msg: String; const args: array of const);
begin
  inherited CreateFmt(msg, args);
  FControl := AControl;
end;

destructor EFreeRDP.Destroy;
begin
  inherited Destroy;
end;

{ TFreeRDPFocusThread }

constructor TFreeRDPFocusThread.Create(const AControlHandle: HWND);
begin
  inherited Create(True);
  FControlHandle := AControlHandle;
  FStartEvent := TSimpleEvent.Create;
  Start;
  FStartEvent.WaitFor(INFINITE);
end;

destructor TFreeRDPFocusThread.Destroy;
begin
  FStartEvent.Free;
  inherited Destroy;
end;

procedure TFreeRDPFocusThread.Execute;
var
  VEvent: TXEvent;
  VOrigWindow: TWindow;
  VMappedWindow: TWindow = 0;
begin
  VOrigWindow := GetXWindow(FControlHandle);
  FDisplay := XOpenDisplay(nil);
  try
    XSelectInput(FDisplay, VOrigWindow, StructureNotifyMask or SubstructureNotifyMask);
    FillChar(VEvent, SizeOf(VEvent), 0);
    FStartEvent.SetEvent;
    while not Terminated do
    begin
      XNextEvent(FDisplay, @VEvent);
      if not G_IS_OBJECT(Pointer(FControlHandle)) then
        Terminate;
      case VEvent._type of
        MapNotify:
        begin
          WriteLn('MapNotify ', VEvent.xmap.window, ' event ', VEvent.xmap.event);
          if VMappedWindow = 0 then
          begin
            PostMessage(FControlHandle, UM_MAP, VEvent.xmap.event, VEvent.xmap.window);
            XSelectInput(FDisplay, VEvent.xmap.window, EnterWindowMask or LeaveWindowMask);
            VMappedWindow := VEvent.xmap.window;
          end;
        end;
        DestroyNotify:
        begin
          WriteLn('DestroyNotify ', VEvent.xmap.window, ' event ', VEvent.xmap.event);
          if VMappedWindow = VEvent.xmap.window then
          begin
            VMappedWindow := 0;
            Terminate;
          end;
        end;
        UnmapNotify:
        begin
          WriteLn('UnmapNotify ', VEvent.xmap.window, ' event ', VEvent.xmap.event);
          if VMappedWindow = VEvent.xmap.window then
          begin
            if G_IS_OBJECT(Pointer(FControlHandle)) then
              PostMessage(FControlHandle, UM_UNMAP, VEvent.xmap.event, VEvent.xmap.window);
          end;
        end;
        EnterNotify:
        begin
          PostMessage(FControlHandle, UM_GRAB, 0, VEvent.xcrossing.window);
          WriteLn('EnterNotify');
        end;
        LeaveNotify:
        begin
          PostMessage(FControlHandle, UM_UNGRAB, 0, VEvent.xcrossing.window);
          WriteLn('LeaveNotify');
        end;
      end;
    end;
  finally
    XCloseDisplay(FDisplay);
  end;
  WriteLn('Terminated');
end;

function FreeRDPKeyboardEvent(Widget: PGtkWidget; Event: PGdkEvent; Data: gpointer): gboolean; cdecl;
var
  VFreeRDP: TFreeRDP;
begin
  VFreeRDP := TFreeRDP(Data);
  VFreeRDP.ProcessKeyEvent(Event);
  Result := True;
end;

{ TFreeRDP }

constructor TFreeRDP.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FUnGrabTimer := TTimer.Create(Self);
  FUnGrabTimer.Interval := 100;
  FUnGrabTimer.Enabled := False;
  FUnGrabTimer.OnTimer := @UnGrabTimerTimer;
  FWindowHack := TWinControl.Create(Self);
  FWindowHack.Parent := Self;
  TabStop := True;
  FPressedKeys := TMapKeySymEvent.Create;

  FProcess := TProcessUTF8.Create(Self);
  FProcess.Options := FProcess.Options + [poUsePipes, poStderrToOutPut];

  ControlStyle := ControlStyle + [csCaptureMouse, csClickEvents, csDoubleClicks, csRequiresKeyboardInput] -
    [csSetCaption, csNoFocus, csNeedsBorderPaint];
  BorderStyle := bsNone;
  BorderWidth := 0;

  FOutBuffer := TMemoryStream.Create;
  FOutLines := TStringList.Create;
  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := @TimerTimer;
  FTimer.Interval := 100;
  FOption := TFreeRDPConnectionOptions.Create;
  FFreeRDPPath := DEFAULT_FREERDP_PATH;
end;

destructor TFreeRDP.Destroy;
begin
  FPressedKeys.Free;
  FOption.Free;
  FOutLines.Free;
  FOutBuffer.Free;
  FProcess.Free;
  inherited Destroy;
end;

procedure TFreeRDP.TimerTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  try
    FTimer.Interval := 1000;
    ParseOutputResult;
  finally
    FTimer.Enabled := True;
  end;
end;

function TFreeRDP.GetActive: Boolean;
begin
  Result := Assigned(FProcess) and FProcess.Running;
end;

procedure TFreeRDP.ProcessKeyEvent(const AEvent: PGdkEvent);
var
  VScreen: PGdkScreen;
  VEvent: TXEvent;
begin
  if FGrabbed then
  begin
    VScreen := gdk_screen_get_default();
    FillChar(VEvent, SizeOf(VEvent), 0);
    VEvent.xkey.root := GDK_WINDOW_XID(gdk_screen_get_root_window(VScreen));
    VEvent.xkey.subwindow := None;
    VEvent.xkey.time := AEvent^.key.time;
    VEvent.xkey.state := AEvent^.key.state;
    VEvent.xkey.keycode := AEvent^.key.hardware_keycode;
    if AEvent^._type = GDK_KEY_PRESS then
      VEvent._type := x.KeyPress
    else
      VEvent._type := x.KeyRelease;
    SendKeyEvent(VEvent);
  end;
end;

procedure TFreeRDP.SendKeyEvent(AEvent: TXEvent);
begin
  if FXWindow <> 0 then
  begin
    AEvent.xkey.window := FXWindow;
    WriteLn('SendKeyEvent');
    XSendEvent(gdk_display, FXWindow, False, NoEventMask, @AEvent);

    if AEvent._type = x.KeyPress then
    begin
      AEvent._type := x.KeyRelease;
      FPressedKeys.Include(AEvent);
    end
    else
      FPressedKeys.Exclude(AEvent);
  end;
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
  if Options.UseCustomResolution and (Options.Width > 0) and (Options.Height > 0) and not Options.SmartSizing then
  begin
    Align := alNone;
    Width := Options.Width;
    Height := Options.Height;
  end
  else
  begin
    Align := alClient;
    Width := Parent.Width;
    Height := Parent.Height;
  end;
end;

procedure TFreeRDP.Stop;
var
  VDummyEvent: TXClientMessageEvent;
begin
  if HandleAllocated then
    UnbindKeyboardListener;
  if Active then
  begin
    FProcess.Terminate(0);
    FProcess.WaitOnExit;
  end;
  if Assigned(FFocusThread) then
  begin
    FFocusThread.Terminate;
    if HandleAllocated then
    begin
      FillChar(VDummyEvent, SizeOf(VDummyEvent), 0);
      VDummyEvent._type := ClientMessage;
      VDummyEvent.display := gdk_display;
      VDummyEvent.window := GetXWindow(Handle);
      VDummyEvent.format := 32;
      XSendEvent(gdk_display, VDummyEvent.window, 0, SubstructureNotifyMask or SubstructureRedirectMask,
        PXEvent(@VDummyEvent));
      XFlush(gdk_display);
    end;
    FFocusThread.WaitFor;
    FreeAndNil(FFocusThread);
  end;
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
    AddParam('/size:%s', [Options.Resolution]);
    if Options.SmartSizing then
      AddParam('/smart-sizing:%s', [Options.Resolution]);
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
  if Options.Sec <> soUndefined then
    AddParam('/sec:%s', [SecCommandValues[Options.Sec]]);
  if Options.GDI <> gdioUndefined then
    AddParam('/gdi:%s', [GDICommandValues[Options.GDI]]);
  if Options.CodecCash <> ccoUndefined then
    AddParam('/codec-cache:%s', [CodecCashCommandValues[Options.CodecCash]]);
  if Options.RFXMode <> rfxmoUndefined then
    AddParam('/rfx-mode:%s', [RFXModeCommandValues[Options.RFXMode]]);
  if Options.RFX then
    AddParam('/rfx', []);

  if Options.SecExt then
    AddParam('+sec-ext', []);
  if not Options.SecNLA then
    AddParam('-sec-nla', []);
  if not Options.SecRDP then
    AddParam('-sec-rdp', []);
  if not Options.SecTLS then
    AddParam('-sec-tls', []);
  if not Options.Clipboard then
    AddParam('-clipboard', []);
  AddParam('-grab-keyboard', []);
  AddParam('/parent-window:%d', [GetXWindow(Handle)]);
  FFocusThread := TFreeRDPFocusThread.Create(Handle);
  FProcess.Execute;
  ParseOutputResult;
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

procedure TFreeRDP.ReleaseAllKeys;
var
  i: Integer;
begin
  for i := FPressedKeys.Count - 1 downto 0 do
    SendKeyEvent(FPressedKeys.Data[i]);
end;

procedure TFreeRDP.DoXEnter;
begin
  if Assigned(FOnXEnter) then
    FOnXEnter(Self);
end;

procedure TFreeRDP.DoXExit;
begin
  if Assigned(FOnXExit) then
    FOnXExit(Self);
end;

procedure TFreeRDP.UMGrab(var AMsg: TLMessage);
begin
  FUnGrabTimer.Enabled := False;
  if Application.Active and not FGrabbed then
  begin
    FGrabbed := True;
    FOldFocusControl := Screen.ActiveControl;
    SetFocus;
    BindKeyboardListener;
    DoXEnter;
    WriteLn(DateTimeToStr(Now, True), ' Enter');
  end;
end;

procedure TFreeRDP.UMUngrab(var AMsg: TLMessage);
begin
  FUnGrabTimer.Enabled := True;
end;

procedure TFreeRDP.UnGrabTimerTimer(Sender: TObject);
begin
  FUnGrabTimer.Enabled := False;
  if Application.Active then
  begin
    UnbindKeyboardListener;
    ReleaseAllKeys;
    FGrabbed := False;
    DoXExit;
    WriteLn(DateTimeToStr(Now, True), ' Exit');
  end;
end;

procedure TFreeRDP.UpdateSmartSizing;
begin
  if FXWindow <> 0 then
    XMoveResizeWindow(gdk_display, FXWindow, 0, 0, Width, Height);
end;

procedure TFreeRDP.Resize;
begin
  inherited Resize;
  if Options.SmartSizing then
    UpdateSmartSizing;
end;

procedure TFreeRDP.UMMap(var AMsg: TLMessage);
var
  VParams: TXWindowAttributes;
begin
  FXWindow := AMsg.LParam;
  if (XGetWindowAttributes(gdk_display, FXWindow, @VParams) <> 0) and Options.UseCustomResolution and
    not Options.SmartSizing then
  begin
    Width := VParams.Width;
    Height := VParams.Height;
  end;
  if Options.SmartSizing then
    UpdateSmartSizing;
end;

procedure TFreeRDP.UMUnmap(var AMsg: TLMessage);
begin
  FXWindow := 0;
end;

procedure TFreeRDP.BindKeyboardListener;
var
  VInstance: PGObject;
begin
  UnbindKeyboardListener;
  VInstance := G_OBJECT(PGtkWidget(Handle));
  FKeyPressEvent := g_signal_connect(VInstance, 'key-press-event', G_CALLBACK(@FreeRDPKeyboardEvent), Self);
  FKeyReleaseEvent := g_signal_connect(VInstance, 'key-release-event', G_CALLBACK(@FreeRDPKeyboardEvent), Self);
end;

procedure TFreeRDP.UnbindKeyboardListener;
var
  VInstance: gpointer;
begin
  VInstance := G_OBJECT(PGtkWidget(Handle));
  if FKeyPressEvent <> 0 then
  begin
    g_signal_handler_disconnect(VInstance, FKeyPressEvent);
    FKeyPressEvent := 0;
  end;
  if FKeyReleaseEvent <> 0 then
  begin
    g_signal_handler_disconnect(VInstance, FKeyReleaseEvent);
    FKeyReleaseEvent := 0;
  end;
end;

procedure TFreeRDP.DestroyHandle;
begin
  Stop;
  inherited DestroyHandle;
end;

initialization
  RegisterWSComponent(TFreeRDP, TWSFreeRDP);

end.
