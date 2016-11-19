unit uPictureFrames;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, uImagerPresenter;

type
  TPictureFrame = class(TFrame)
    Picture_Inst: TLayout;
    AddPictureBtn: TCornerButton;
    DelPictureBtn: TCornerButton;
    ClonePictureBtn: TCornerButton;
    EditPictureBtn: TCornerButton;
    procedure AddPictureBtnClick(Sender: TObject);
    procedure DelPictureBtnClick(Sender: TObject);
    procedure EditPictureBtnClick(Sender: TObject);
    procedure ClonePictureBtnClick(Sender: TObject);
  private
    FImager: TImagerPresenter;
    { Private declarations }
  public
    procedure Init(const APresenter: TImagerPresenter);
    { Public declarations }
  end;

implementation

{$R *.fmx}

{ TPictureFrame }

procedure TPictureFrame.AddPictureBtnClick(Sender: TObject);
begin
  FImager.AddImg;
end;

procedure TPictureFrame.ClonePictureBtnClick(Sender: TObject);
begin
  FImager.CloneImg;
end;

procedure TPictureFrame.DelPictureBtnClick(Sender: TObject);
begin
  FImager.DelImg;
end;

procedure TPictureFrame.EditPictureBtnClick(Sender: TObject);
begin
  FImager.ShowOptions;
end;

procedure TPictureFrame.Init(const APresenter: TImagerPresenter);
begin
  FImager := APresenter;
end;

end.
