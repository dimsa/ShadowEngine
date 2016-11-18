unit uShapeFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts;

type
  TShapeFrame = class(TFrame)
    Shape_Inst: TLayout;
    AddCircleBtn: TCornerButton;
    AddPolyBtn: TCornerButton;
    DelShapeBtn: TCornerButton;
    EdtShapeBtn: TCornerButton;
    DelPointBtn: TCornerButton;
    AddPointBtn: TCornerButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
