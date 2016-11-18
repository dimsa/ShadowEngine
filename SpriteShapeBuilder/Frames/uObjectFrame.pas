unit uObjectFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts;

type
  TObjectFrame = class(TFrame)
    Object_Inst: TLayout;
    AddObjectBtn: TCornerButton;
    DelObjectBtn: TCornerButton;
    EdtObjectBtn: TCornerButton;
    CloneObjectBtn: TCornerButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
