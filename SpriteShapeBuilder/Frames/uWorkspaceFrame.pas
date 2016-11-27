unit uWorkspaceFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uRenditionConstructorFrame, uGraphicItemWorkspaceFrame;

type
  TWorkspaceFrame = class(TFrame)
    GraphicItemWorkspaceFrame: TGraphicItemWorkspaceFrame;
    RenditionConstructorFrame: TRenditionConstructorFrame;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
