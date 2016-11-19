unit uShapeFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, uObjecterPresenter;

type
  TShapeFrame = class(TFrame)
    Shape_Inst: TLayout;
    AddCircleBtn: TCornerButton;
    AddPolyBtn: TCornerButton;
    DelShapeBtn: TCornerButton;
    EdtShapeBtn: TCornerButton;
    PointLayout: TLayout;
    DelPointBtn: TCornerButton;
    AddPointBtn: TCornerButton;
    procedure AddCircleBtnClick(Sender: TObject);
    procedure AddPolyBtnClick(Sender: TObject);
    procedure EdtShapeBtnClick(Sender: TObject);
    procedure DelPointBtnClick(Sender: TObject);
    procedure AddPointBtnClick(Sender: TObject);
    procedure DelShapeBtnClick(Sender: TObject);
  private
    FObjecter: TObjecterPresenter;
  public
     procedure Init(const APresenter: TObjecterPresenter);
  end;

implementation

{$R *.fmx}

procedure TShapeFrame.AddCircleBtnClick(Sender: TObject);
begin
  FObjecter.AddCircle;
end;

procedure TShapeFrame.AddPointBtnClick(Sender: TObject);
begin
  FObjecter.AddPoint;
end;

procedure TShapeFrame.AddPolyBtnClick(Sender: TObject);
begin
  FObjecter.AddPoly;
end;

procedure TShapeFrame.DelPointBtnClick(Sender: TObject);
begin
  FObjecter.DelPoint;
end;

procedure TShapeFrame.DelShapeBtnClick(Sender: TObject);
begin
  FObjecter.DelShape;
end;

procedure TShapeFrame.EdtShapeBtnClick(Sender: TObject);
begin
  FObjecter.ShowOptions;
end;

procedure TShapeFrame.Init(const APresenter: TObjecterPresenter);
begin
  FObjecter := APresenter;
end;

end.
