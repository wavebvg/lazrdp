program lazrdp;

{$Define UseCThreads}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMainFormLazRDP, UDialogFreeRDPOptions,
  UDMFreeRDP, UFormDialogFreeRDPOptions, UFreeRDP, UFreeRDPOptions,
  UFormDialogFreeRDPPreferences, UDialogFreeRDPPreferences, UFrameViewFreeRDP;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'LazRDP';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TDMFreeRDP, DMFreeRDP);
  Application.CreateForm(TMainFormLazRDP, MainFormLazRDP);
  Application.Run;
end.

