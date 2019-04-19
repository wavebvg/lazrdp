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

    procedure UMGrab(var {%H-}AMsg: TLMessage); message UM_GRAB;
    procedure UMUngrab(var {%H-}AMsg: TLMessage); message UM_UNGRAB;
    procedure UMMap(var AMsg: TLMessage); message UM_MAP;
    procedure UMUnmap(var {%H-}AMsg: TLMessage); message UM_UNMAP;
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
  syncobjs,
  InterfaceBase,
  Gtk2WSControls,
  WSLCLClasses;

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

  { TMapWindowHandle }

  TMapWindowHandle = class(specialize TFPGMap<TWindow, TFreeRDP>)
  private
    FLock: TCriticalSection;
    function GetHandle(const AWindow: TWindow): TFreeRDP;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Include(const AWindow: TWindow; const AHandle: TFreeRDP);
    procedure Exclude(const AWindow: TWindow);
    procedure Remove(const AControl: TFreeRDP);
    property Handles[const AWindow: TWindow]: TFreeRDP read GetHandle;
  end;

  { TFreeRDPEvents }

  TFreeRDPEvents = class(TThread)
  private
    FHandle: TWindow;
    FStartEvent: TEvent;
    FMapControlByOwnXWindow, FMapControlByFreeRDPXWindow: TMapWindowHandle;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Terminate;

    procedure Subscribe(const AControl: TFreeRDP);
    procedure Unsubscribe(const AControl: TFreeRDP);
  end;

  { TWSFreeRDP }

  TWSFreeRDP = class(TGtk2WSWinControl)
  private
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

var
  Events: TFreeRDPEvents;

function FreeRDPWidgetDestroy(Widget: PGtkWidget; {%H-}Data: gPointer): GBoolean; cdecl;
begin
  FreeWidgetInfo(Widget);
  Result := True;
end;

function GetXWindow(const AHandle: TWinControl): TWindow;
var
  VWidget: PGtkWidget;
  VWindow: PGdkWindow;
begin
  VWidget := {%H-}PGtkWidget(AHandle.Handle);
  VWindow := VWidget^.window;
  Result := GDK_WINDOW_XWINDOW(VWindow);
end;

procedure SendGdkKeyEvent(const AEvent: PGdkEvent; const AWindow: TWindow);
var
  VScreen: PGdkScreen;
  VEvent: TXEvent;
begin
  VScreen := gdk_screen_get_default();
  FillChar({%H-}VEvent, SizeOf(VEvent), 0);
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

type
  TClientMessageType = (cmtTerminate, cmtSubscribe, cmtUnSubscribe);

procedure SendClientMessage(const AHandle: TWindow; const AType: TClientMessageType; const AWindow: THandle);
var
  VEvent: TXEvent;
begin
  FillChar({%H-}VEvent, SizeOf(VEvent), 0);
  VEvent.xclient._type := ClientMessage;
  VEvent.xclient.window := AHandle;
  VEvent.xclient.format := 32;
  VEvent.xclient.Data.l[0] := Ord(AType);
  VEvent.xclient.Data.l[1] := AWindow;
  XSendEvent(gdk_display, AHandle, 0, 0, @VEvent);
  XFlush(gdk_display);
end;

{ TFreeRDPEvents }

constructor TFreeRDPEvents.Create;
begin
  inherited Create(True);
  FMapControlByOwnXWindow := TMapWindowHandle.Create;
  FMapControlByFreeRDPXWindow := TMapWindowHandle.Create;
  FStartEvent := TSimpleEvent.Create;
  Start;
  FStartEvent.WaitFor(INFINITE);
end;

destructor TFreeRDPEvents.Destroy;
begin
  FMapControlByOwnXWindow.Free;
  FMapControlByFreeRDPXWindow.Free;
  FStartEvent.Free;
  inherited Destroy;
end;

procedure TFreeRDPEvents.Terminate;
begin
  inherited;
  SendClientMessage(FHandle, cmtTerminate, 0);
end;

procedure TFreeRDPEvents.Subscribe(const AControl: TFreeRDP);
begin
  FMapControlByOwnXWindow.Include(GetXWindow(AControl), AControl);
  WriteLn('MapControlByOwnXWindow.Count = ', FMapControlByOwnXWindow.Count);
  SendClientMessage(FHandle, cmtSubscribe, GetXWindow(AControl));
end;

procedure TFreeRDPEvents.Unsubscribe(const AControl: TFreeRDP);
begin
  FMapControlByOwnXWindow.Exclude(GetXWindow(AControl));
  WriteLn('MapControlByOwnXWindow.Count = ', FMapControlByOwnXWindow.Count);
  FMapControlByFreeRDPXWindow.Remove(AControl);
  WriteLn('MapControlByFreeRDPXWindow.Count = ', FMapControlByFreeRDPXWindow.Count);
  if not Application.Terminated then
    SendClientMessage(FHandle, cmtUnSubscribe, GetXWindow(AControl));
end;

procedure TFreeRDPEvents.Execute;
var
  VEvent: TXEvent;
  VScreen: PGdkScreen;
  VRoot, VWindow: TWindow;
  VDisplay: PDisplay;
  VControl: TFreeRDP;
begin
  VDisplay := XOpenDisplay(nil);
  try
    VScreen := gdk_screen_get_default();
    VRoot := GDK_WINDOW_XID(gdk_screen_get_root_window(VScreen));
    FHandle := XCreateSimpleWindow(VDisplay, VRoot, 10, 10, 10, 10, 0, 0, 0);
    try
      XSelectInput(VDisplay, FHandle, StructureNotifyMask);
      FStartEvent.SetEvent;
      while not Terminated do
      begin
        FillChar({%H-}VEvent, SizeOf(VEvent), 0);
        XNextEvent(VDisplay, @VEvent);
        if not Terminated then
          case VEvent._type of
            CreateNotify:
            begin
              WriteLn('TFreeRDPEvents.Execute CreateNotify (window = ', VEvent.xmap.window,
                ', event = ', VEvent.xmap.event, ')');
            end;
            DestroyNotify:
            begin
              WriteLn('TFreeRDPEvents.Execute DestroyNotify (window = ', VEvent.xmap.window,
                ', event = ', VEvent.xmap.event, ')');
            end;
            MapNotify:
            begin
              WriteLn('TFreeRDPEvents.Execute MapNotify (window = ', VEvent.xmap.window,
                ', event = ', VEvent.xmap.event, ')');
              if VEvent.xmap.event <> VEvent.xmap.window then
              begin
                VControl := FMapControlByOwnXWindow.Handles[VEvent.xmap.event];
                PostMessage(VControl.Handle, UM_MAP, VEvent.xmap.event, VEvent.xmap.window);
                FMapControlByFreeRDPXWindow.Include(VEvent.xmap.window, VControl);
                WriteLn('MapControlByFreeRDPXWindow.Count = ', FMapControlByFreeRDPXWindow.Count);
                XSelectInput(VDisplay, VEvent.xmap.window, EnterWindowMask or LeaveWindowMask);
              end;
            end;
            UnmapNotify:
            begin
              WriteLn('TFreeRDPEvents.Execute UnmapNotify (window = ', VEvent.xmap.window,
                ', event = ', VEvent.xmap.event, ')');
              if VEvent.xmap.event <> VEvent.xmap.window then
              begin
                VControl := FMapControlByOwnXWindow.Handles[VEvent.xmap.event];
                PostMessage(VControl.Handle, UM_UNMAP, VEvent.xmap.event, VEvent.xmap.window);
                FMapControlByFreeRDPXWindow.Exclude(VEvent.xmap.window);
                WriteLn('MapControlByFreeRDPXWindow.Count = ', FMapControlByFreeRDPXWindow.Count);
              end;
            end;
            EnterNotify:
            begin
              VControl := FMapControlByFreeRDPXWindow.Handles[VEvent.xmap.event];
              PostMessage(VControl.Handle, UM_GRAB, 0, VEvent.xcrossing.window);
              WriteLn('TFreeRDPEvents.Execute EnterNotify (window = ', VEvent.xcrossing.window, ', handle = ',
                VControl.Handle, ')');
            end;
            LeaveNotify:
            begin
              VControl := FMapControlByFreeRDPXWindow.Handles[VEvent.xmap.event];
              PostMessage(VControl.Handle, UM_UNGRAB, 0, VEvent.xcrossing.window);
              WriteLn('TFreeRDPEvents.Execute LeaveNotify (window = ', VEvent.xcrossing.window, ', handle = ',
                VControl.Handle, ')');
            end;
            ClientMessage:
            begin
              WriteLn('TFreeRDPEvents.Execute ClientMessage');
              case TClientMessageType(VEvent.xclient.Data.l[0]) of
                cmtTerminate:
                begin
                  WriteLn('TFreeRDPEvents.Execute cmtTerminate');
                end;
                cmtSubscribe:
                begin
                  WriteLn('TFreeRDPEvents.Execute cmtSubscribe');
                  VWindow := VEvent.xclient.Data.l[1];
                  XSelectInput(VDisplay, VWindow, StructureNotifyMask or SubstructureNotifyMask);
                end;
                cmtUnSubscribe:
                begin
                  WriteLn('TFreeRDPEvents.Execute cmtUnSubscribe');
                  VWindow := VEvent.xclient.Data.l[1];
                  XSelectInput(VDisplay, VWindow, 0);
                end;
              end;
            end;
          end;
      end;
    finally
      XDestroyWindow(VDisplay, FHandle);
    end;
  finally
    XCloseDisplay(VDisplay);
  end;
  WriteLn('Terminated');
end;

class function TWSFreeRDP.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  VNewWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  if csDesigning in AWinControl.ComponentState then
    Result := inherited CreateHandle(AWinControl, AParams)
  else
  begin
    VNewWidget := gtk_event_box_new;
    gtk_widget_set_events(VNewWidget, GDK_ALL_EVENTS_MASK);
    GTK_WIDGET_SET_FLAGS(VNewWidget, GTK_CAN_FOCUS);

    WidgetInfo := GetWidgetInfo(VNewWidget, True);
    WidgetInfo^.LCLObject := AWinControl;
    WidgetInfo^.Style := AParams.Style;
    WidgetInfo^.ExStyle := AParams.ExStyle;
    WidgetInfo^.WndProc := {%H-}PtrUInt(AParams.WindowClass.lpfnWndProc);

    // set allocation
    Allocation.X := AParams.X;
    Allocation.Y := AParams.Y;
    Allocation.Width := AParams.Width;
    Allocation.Height := AParams.Height;
    gtk_widget_size_allocate(VNewWidget, @Allocation);

    if csDesigning in AWinControl.ComponentState then
    begin
      // at designtime setup normal handlers
      TGtk2WidgetSet(WidgetSet).FinishCreateHandle(AWinControl, VNewWidget, AParams);
    end
    else
    begin
      // at runtime
      g_signal_connect(GPointer(VNewWidget), 'destroy',
        TGTKSignalFunc(@FreeRDPWidgetDestroy), WidgetInfo);
    end;
    Result := HWND({%H-}PtrUInt(Pointer(VNewWidget)));
  end;
end;

class procedure TWSFreeRDP.DestroyHandle(const AWinControl: TWinControl);
var
  VWidget: PGtkWidget;
begin
  VWidget := {%H-}PGtkWidget(AWinControl.Handle);
  FreeWidgetInfo(VWidget);
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

{ TMapWindowHandle }

constructor TMapWindowHandle.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TMapWindowHandle.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

function TMapWindowHandle.GetHandle(const AWindow: TWindow): TFreeRDP;
begin
  FLock.Enter;
  try
    Result := KeyData[AWindow];
  finally
    FLock.Leave;
  end;
end;

procedure TMapWindowHandle.Include(const AWindow: TWindow; const AHandle: TFreeRDP);
begin
  FLock.Enter;
  try
    KeyData[AWindow] := AHandle;
  finally
    FLock.Leave;
  end;
end;

procedure TMapWindowHandle.Exclude(const AWindow: TWindow);
var
  VIndex: Integer;
begin
  FLock.Enter;
  try
    VIndex := IndexOf(AWindow);
    if VIndex >= 0 then
      Delete(VIndex);
  finally
    FLock.Leave;
  end;
end;

procedure TMapWindowHandle.Remove(const AControl: TFreeRDP);
var
  VIndex: Integer;
begin
  FLock.Enter;
  try
    VIndex := IndexOfData(AControl);
    if VIndex >= 0 then
      Delete(VIndex);
  finally
    FLock.Leave;
  end;
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

function FreeRDPKeyboardEvent({%H-}Widget: PGtkWidget; Event: PGdkEvent; Data: gpointer): gboolean; cdecl;
begin
  TFreeRDP(Data).ProcessKeyEvent(Event);
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
    FillChar({%H-}VEvent, SizeOf(VEvent), 0);
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
begin
  Events.Unsubscribe(Self);
  if HandleAllocated then
    UnbindKeyboardListener;
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
  AddParam('/parent-window:%d', [GetXWindow(Self)]);
  Events.Subscribe(Self);
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
  WriteLn('UMGrab');
  FUnGrabTimer.Enabled := False;
  if Application.Active and not FGrabbed then
  begin
    FGrabbed := True;
    FOldFocusControl := Screen.ActiveControl;
    SetFocus;
    BindKeyboardListener;
    DoXEnter;
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
  end;
end;

procedure TFreeRDP.UpdateSmartSizing;
begin
  if FXWindow <> 0 then
    XMoveResizeWindow(gdk_display, FXWindow, 0, 0, Width, Height);
end;

procedure TFreeRDP.DestroyHandle;
begin
  Stop;
  inherited DestroyHandle;
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
  WriteLn('TFreeRDP.UMMap');
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
  WriteLn('TFreeRDP.UMUnmap');
  FXWindow := 0;
end;

procedure TFreeRDP.BindKeyboardListener;
var
  VInstance: PGObject;
begin
  UnbindKeyboardListener;
  VInstance := G_OBJECT({%H-}PGtkWidget(Handle));
  FKeyPressEvent := g_signal_connect(VInstance, 'key-press-event', G_CALLBACK(@FreeRDPKeyboardEvent), Self);
  FKeyReleaseEvent := g_signal_connect(VInstance, 'key-release-event', G_CALLBACK(@FreeRDPKeyboardEvent), Self);
end;

procedure TFreeRDP.UnbindKeyboardListener;
var
  VInstance: gpointer;
begin
  VInstance := G_OBJECT({%H-}PGtkWidget(Handle));
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

initialization
  RegisterWSComponent(TFreeRDP, TWSFreeRDP);
  Events := TFreeRDPEvents.Create;

finalization
  Events.Terminate;
  Events.WaitFor;
  Events.Free;

end.
