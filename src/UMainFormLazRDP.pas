// TODO : Отображать, при попытке подключения, диалог подключения
// TODO : Добавить фрейм для отображения реального размера окна FreeRDP
// TODO : 

unit UMainFormLazRDP;

{$mode objfpc}{$H+}

interface

uses
  Controls,
  Graphics,
  SysUtils,
  Classes,
  SQLite3Conn,
  Dialogs,
  Forms,
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  Menus,
  ActnList,
  UFreeRDPOptions,
  UFrameViewFreeRDP,
  UFreeRDP,
  IniFiles,
  Types;

type
  TApplicationMode = (amNormal, amFullScreen, amHideDecoration);

  { TMainFormLazRDP }

  TMainFormLazRDP = class(TForm)
    ActionConnectionRunAlways: TAction;
    ActionPreferences: TAction;
    ActionToogleDecoration: TAction;
    ActionExit: TAction;
    ActionToogleFullScreen: TAction;
    ActionConnectionRun: TAction;
    ActionConnectionAdd: TAction;
    ActionConnectionEdit: TAction;
    ActionConnectionDelete: TAction;
    ActionList: TActionList;
    ImageListIcons: TImageList;
    ListBoxServers: TListBox;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuTrayConnections: TMenuItem;
    MenuTrayPreferences: TMenuItem;
    MenuTrayExit: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemFullScreen: TMenuItem;
    MenuItemActions: TMenuItem;
    MenuConnectionRun: TMenuItem;
    MenuConnectionAdd: TMenuItem;
    MenuConnectionEdit: TMenuItem;
    MenuConnectionDelete: TMenuItem;
    PageControlConnections: TPageControl;
    PanelNormal: TPanel;
    PanelServers: TPanel;
    PanelDisplay: TPanel;
    PopupMenuTray: TPopupMenu;
    PopupMenuConnections: TPopupMenu;
    SplitterChannels: TSplitter;
    TrayIcon: TTrayIcon;
    procedure ActionConnectionAddExecute(Sender: TObject);
    procedure ActionConnectionDeleteExecute(Sender: TObject);
    procedure ActionConnectionDeleteUpdate(Sender: TObject);
    procedure ActionConnectionEditExecute(Sender: TObject);
    procedure ActionConnectionEditUpdate(Sender: TObject);
    procedure ActionConnectionRunAlwaysExecute(Sender: TObject);
    procedure ActionConnectionRunExecute(Sender: TObject);
    procedure ActionConnectionRunUpdate(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionPreferencesExecute(Sender: TObject);
    procedure ActionToogleDecorationExecute(Sender: TObject);
    procedure ActionToogleDecorationUpdate(Sender: TObject);
    procedure ActionToogleFullScreenExecute(Sender: TObject);
    procedure ActionToogleFullScreenUpdate(Sender: TObject);
    procedure ButtonFocusClick(Sender: TObject);
    procedure ListBoxServersDblClick(Sender: TObject);
    procedure PageControlConnectionsChange(Sender: TObject);
    procedure PageControlConnectionsCloseTabClicked(Sender: TObject);
    procedure PopupMenuTrayClose(Sender: TObject);
    procedure PopupMenuTrayPopup(Sender: TObject);
  private
    FInTray: Boolean;
    FIcons: TBooleanDynArray;
    FOldBoundsRect: TRect;
    FApplicationMode: TApplicationMode;
    FOldApplicationException: TExceptionEvent;
    procedure ApplicationException(Sender: TObject; E: Exception);
    procedure ConnectToFreeRDP(const AOptionIndex: Integer);
    procedure FreeRDPXEnter(Sender: TObject);
    procedure FreeRDPXExit(Sender: TObject);
    function GetActiveFrame: TFrameViewFreeRDP;
    function GetApplicationMode: TApplicationMode;
    procedure SetApplicationMode(const AValue: TApplicationMode);
    procedure UpdateConnectionList(const AUpdated: Boolean);
    procedure UpdateCaption;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ApplicationMode: TApplicationMode read GetApplicationMode write SetApplicationMode;
    property ActiveFrame: TFrameViewFreeRDP read GetActiveFrame;
  end;

var
  MainFormLazRDP: TMainFormLazRDP;

implementation

uses
  UDMFreeRDP,
  gtk2,
  gdk2x,
  GTK2Proc,
  Gtk2Def,
  WSLCLClasses;

{$R *.lfm}

{ TMainFormLazRDP }

constructor TMainFormLazRDP.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FOldApplicationException := Application.OnException;
  Application.OnException := @ApplicationException;

  UpdateConnectionList(False);
  TrayIcon.Show;
end;

destructor TMainFormLazRDP.Destroy;
begin
  ReleaseKeyBoardFromForm(Self);
  Application.OnException := FOldApplicationException;
  inherited Destroy;
end;

procedure TMainFormLazRDP.SetApplicationMode(const AValue: TApplicationMode);
var
  VFrame: TFrameViewFreeRDP;

  procedure SetNewPageControlSize(const AWidth, AHeight: Integer);
  var
    VHBorder, VWBorder: Integer;
  begin
    PageControlConnections.ShowTabs := False;
    PageControlConnections.Align := alCustom;
    VWBorder := (PageControlConnections.Width - PageControlConnections.ClientWidth) div 2;
    VHBorder := (PageControlConnections.Height - PageControlConnections.ClientHeight) div 2;
    PageControlConnections.BoundsRect :=
      Rect(-VWBorder, -VHBorder, AWidth + VWBorder, AHeight + VHBorder);
  end;

begin
  if AValue = ApplicationMode then
    Exit;
  VFrame := ActiveFrame;
  if not Assigned(VFrame) then
    Exit;
  case ApplicationMode of
    amNormal:
    begin
      FOldBoundsRect := BoundsRect;
    end;
    amFullScreen:
    begin
      SetWindowFullScreen(Self, False);
    end;
    amHideDecoration:
    begin
      gtk_window_set_decorated(PGtkWindow(Handle), True);
      gtk_window_set_resizable(PGtkWindow(Handle), True);
      gtk_widget_set_size_request(PGtkWidget(Handle), -1, -1);
      Application.ProcessMessages;
      Width := FOldBoundsRect.Width;
      Height := FOldBoundsRect.Height;
    end;
  end;
  FApplicationMode := AValue;
  case ApplicationMode of
    amNormal:
    begin
      PanelServers.Visible := True;
      SplitterChannels.Visible := True;
      PageControlConnections.ShowTabs := True;
      PageControlConnections.Align := alClient;
      Menu := MainMenu;
    end;
    amFullScreen:
    begin
      PanelServers.Visible := False;
      SplitterChannels.Visible := False;
      Menu := nil;
      Application.ProcessMessages;
      SetWindowFullScreen(Self, True);
      SetNewPageControlSize(Screen.Width, Screen.Height);
    end;
    amHideDecoration:
    begin
      PanelServers.Visible := False;
      SplitterChannels.Visible := False;
      Menu := nil;
      gtk_window_set_resizable(PGtkWindow(Handle), False);
      gtk_window_set_decorated(PGtkWindow(Handle), False);
      gtk_widget_set_size_request(PGtkWidget(Handle), VFrame.FreeRDP.Width, VFrame.FreeRDP.Height);
      SetNewPageControlSize(VFrame.FreeRDP.Width, VFrame.FreeRDP.Height);
    end;
  end;
end;

procedure TMainFormLazRDP.ApplicationException(Sender: TObject; E: Exception);
var
  VError: EFreeRDP;
  i: Integer;
  VPage: TTabSheet;
  VFrame: TFrameViewFreeRDP;
begin
  if ApplicationMode = amFullScreen then
    ApplicationMode := amNormal;
  if E is EFreeRDP then
  begin
    VError := EFreeRDP(E);
    for i := 0 to PageControlConnections.PageCount - 1 do
    begin
      VPage := PageControlConnections.Pages[i];
      if VPage.ControlCount > 0 then
      begin
        VFrame := VPage.Controls[0] as TFrameViewFreeRDP;
        if VFrame.FreeRDP = VError.Control then
        begin
          ApplicationMode := amNormal;
          MessageDlg('FreeRDP', VError.Message, mtError, [mbOK], 0);
          ReleaseKeyBoardFromForm(Self);
          VPage.Free;
          Exit;
        end;
      end;
    end;
  end;
  if Assigned(FOldApplicationException) then
    FOldApplicationException(Sender, E);
end;

function TMainFormLazRDP.GetActiveFrame: TFrameViewFreeRDP;
begin
  if Assigned(PageControlConnections.ActivePage) then
    if PageControlConnections.ActivePage.ControlCount > 0 then
      Result := PageControlConnections.ActivePage.Controls[0] as TFrameViewFreeRDP
    else
      Result := nil
  else
    Result := nil;
end;

function TMainFormLazRDP.GetApplicationMode: TApplicationMode;
begin
  Result := FApplicationMode;
end;

procedure TMainFormLazRDP.ActionConnectionAddExecute(Sender: TObject);
begin
  DMFreeRDP.DialogFreeRDPOptions.Reset;
  if DMFreeRDP.DialogFreeRDPOptions.Execute then
  begin
    DMFreeRDP.Connections.Add(DMFreeRDP.DialogFreeRDPOptions.Options);
    UpdateConnectionList(True);
  end;
end;

procedure TMainFormLazRDP.ActionConnectionDeleteExecute(Sender: TObject);
begin
  DMFreeRDP.Connections.Delete(ListBoxServers.ItemIndex);
  UpdateConnectionList(True);
end;

procedure TMainFormLazRDP.ActionConnectionDeleteUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := ListBoxServers.ItemIndex >= 0;
end;

procedure TMainFormLazRDP.ActionConnectionEditExecute(Sender: TObject);
begin
  DMFreeRDP.DialogFreeRDPOptions.Options := DMFreeRDP.Connections[ListBoxServers.ItemIndex];
  if DMFreeRDP.DialogFreeRDPOptions.Execute then
  begin
    DMFreeRDP.Connections[ListBoxServers.ItemIndex] := DMFreeRDP.DialogFreeRDPOptions.Options;
    UpdateConnectionList(True);
  end;
end;

procedure TMainFormLazRDP.ActionConnectionEditUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := ListBoxServers.ItemIndex >= 0;
end;

procedure TMainFormLazRDP.ConnectToFreeRDP(const AOptionIndex: Integer);
var
  VNewTab: TTabSheet;
  VView: TFrameViewFreeRDP;
  VOptions: TFreeRDPConnectionOptions;
begin
  VNewTab := PageControlConnections.AddTabSheet;
  VView := TFrameViewFreeRDP.Create(VNewTab);
  VOptions := DMFreeRDP.Connections[AOptionIndex];
  VView.FreeRDP.FreeRDPPath := DMFreeRDP.Preferences.FreeRDPPath;
  VView.Align := alClient;
  VView.Options := VOptions;
  VView.Parent := VNewTab;
  VView.OnXEnter := @FreeRDPXEnter;
  VView.OnXExit := @FreeRDPXExit;
  VNewTab.Caption := VOptions.ConnectionName;
  if FIcons[AOptionIndex] then
    VNewTab.ImageIndex := ListBoxServers.ItemIndex
  else
    VNewTab.ImageIndex := -1;
  PageControlConnections.ActivePage := VNewTab;
  Application.ProcessMessages;
  VView.Active := True;
  UpdateCaption;
end;

procedure TMainFormLazRDP.FreeRDPXEnter(Sender: TObject);
begin
  if DMFreeRDP.Preferences.GrabKeyboard then
    GrabKeyBoardToForm(Self);
end;

procedure TMainFormLazRDP.FreeRDPXExit(Sender: TObject);
begin
  if DMFreeRDP.Preferences.GrabKeyboard then
    ReleaseKeyBoardFromForm(Self);
end;

procedure TMainFormLazRDP.ActionConnectionRunUpdate(Sender: TObject);
begin
  if FInTray then
    TCustomAction(Sender).Enabled := True
  else
  if Application.Active then
    if ListBoxServers.Focused then
      TCustomAction(Sender).Enabled := ListBoxServers.ItemIndex >= 0;
end;

procedure TMainFormLazRDP.ActionConnectionRunExecute(Sender: TObject);
begin
  ConnectToFreeRDP(ListBoxServers.ItemIndex);
end;

procedure TMainFormLazRDP.ActionConnectionRunAlwaysExecute(Sender: TObject);
var
  VMenuItem: TMenuItem;
begin
  VMenuItem := TCustomAction(Sender).ActionComponent as TMenuItem;
  ConnectToFreeRDP(VMenuItem.MenuIndex);
end;

procedure TMainFormLazRDP.ActionExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainFormLazRDP.ActionPreferencesExecute(Sender: TObject);
begin
  DMFreeRDP.EditOptions;
end;

procedure TMainFormLazRDP.ActionToogleDecorationExecute(Sender: TObject);
begin
  if ApplicationMode = amNormal then
    ApplicationMode := amHideDecoration
  else
    ApplicationMode := amNormal;
end;

procedure TMainFormLazRDP.ActionToogleDecorationUpdate(Sender: TObject);
var
  VFrame: TFrameViewFreeRDP;
begin
  VFrame := ActiveFrame;
  TCustomAction(Sender).Enabled := Assigned(VFrame) and VFrame.Options.UseCustomResolution;
end;

procedure TMainFormLazRDP.ActionToogleFullScreenExecute(Sender: TObject);
begin
  if ApplicationMode = amNormal then
    ApplicationMode := amFullScreen
  else
    ApplicationMode := amNormal;
end;

procedure TMainFormLazRDP.ActionToogleFullScreenUpdate(Sender: TObject);
begin
  TCustomAction(Sender).Enabled := Assigned(ActiveFrame);
end;

procedure TMainFormLazRDP.ButtonFocusClick(Sender: TObject);
var
  VFrame: TFrameViewFreeRDP;
begin
  VFrame := ActiveFrame;
  if Assigned(VFrame) then
    VFrame.SetFocus;
end;

procedure TMainFormLazRDP.ListBoxServersDblClick(Sender: TObject);
begin
  ActionConnectionRun.ActionComponent := Sender as TComponent;
  ActionConnectionRun.Execute;
end;

procedure TMainFormLazRDP.PageControlConnectionsChange(Sender: TObject);
begin
  UpdateCaption;
end;

procedure TMainFormLazRDP.PageControlConnectionsCloseTabClicked(Sender: TObject);
begin
  if ApplicationMode <> amNormal then
    ApplicationMode := amNormal;
  Sender.Free;
  UpdateCaption;
end;

procedure TMainFormLazRDP.PopupMenuTrayPopup(Sender: TObject);
begin
  FInTray := True;
end;

procedure TMainFormLazRDP.PopupMenuTrayClose(Sender: TObject);
begin
  FInTray := False;
end;

procedure TMainFormLazRDP.UpdateConnectionList(const AUpdated: Boolean);
var
  i: Integer;
  VPicture: TPicture;
  VBitmap: TBitmap;

  procedure AddDefault;
  begin
    FIcons[i] := False;
    VBitmap.Clear;
    ImageListIcons.Add(VBitmap, nil);
  end;

var
  VOptions: TFreeRDPConnectionOptions;
  VMenuItem: TMenuItem;

begin
  if AUpdated then
    DMFreeRDP.SaveConnections;
  ListBoxServers.Items.Assign(DMFreeRDP.Connections);
  SetLength(FIcons, DMFreeRDP.Connections.Count);
  ImageListIcons.Clear;
  MenuTrayConnections.Clear;
  MenuTrayConnections.Visible := DMFreeRDP.Connections.Count > 0;
  VPicture := TPicture.Create;
  VBitmap := TBitmap.Create;
  try
    for i := 0 to DMFreeRDP.Connections.Count - 1 do
    begin
      VOptions := DMFreeRDP.Connections[i];
      if (VOptions.IconFileName <> '') and FileExists(VOptions.IconFileName) then
      begin
        FIcons[i] := True;
        try
          VPicture.LoadFromFile(DMFreeRDP.Connections[i].IconFileName);
          VBitmap.Assign(VPicture.Graphic);
          ImageListIcons.Add(VBitmap, nil);
        except
          on E: Exception do
            AddDefault;
        end;
      end
      else
        AddDefault;
      VMenuItem := TMenuItem.Create(Self);
      VMenuItem.Caption := DMFreeRDP.Connections[i].ConnectionName;
      VMenuItem.Action := ActionConnectionRunAlways;
      VMenuItem.ImageIndex := i;
      MenuTrayConnections.Add(VMenuItem);
    end;
  finally
    VBitmap.Free;
    VPicture.Free;
  end;
end;

procedure TMainFormLazRDP.UpdateCaption;
var
  VFrame: TFrameViewFreeRDP;
  VPicture: TPicture;
begin
  VFrame := ActiveFrame;
  if Assigned(VFrame) then
  begin
    Caption := Format('%s [%s] - %s', [VFrame.Options.ConnectionName, VFrame.Options.ServerInfo, ApplicationName]);
    if FileExists(VFrame.Options.IconFileName) then
    begin
      VPicture := TPicture.Create;
      try
        VPicture.LoadFromFile(VFrame.Options.IconFileName);
        Icon.Assign(VPicture.Graphic);
        Application.ProcessMessages;
      finally
        VPicture.Free;
      end;
    end;
  end
  else
  begin
    Caption := ApplicationName;
    Icon := Application.Icon;
  end;
end;

end.
