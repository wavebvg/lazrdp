unit UFrameFreeRDPControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls;

type
  TFrameFreeRDPControl = class(TFrame)
  private

  public
    procedure Run;
    property Active: Boolean read GetActive write SetActive;
  published
    property FreeRDPPath: String read FFreeRDPPath write FFreeRDPPath;
    property Options: TFreeRDPConnectionOptions read FOption write SetOptions;
  end;

implementation

{$R *.lfm}

end.

