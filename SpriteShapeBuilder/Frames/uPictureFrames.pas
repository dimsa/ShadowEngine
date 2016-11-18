unit uPictureFrames;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts;

type
  TPictureFrame = class(TFrame)
    Picture_Inst: TLayout;
    AddPictureBtn: TCornerButton;
    DelPictureBtn: TCornerButton;
    ClonePictureBtn: TCornerButton;
    EditPictureBtn: TCornerButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
