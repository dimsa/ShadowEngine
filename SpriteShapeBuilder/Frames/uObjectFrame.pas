unit uObjectFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, uObjecterPresenter;

type
  TObjectFrame = class(TFrame)
    Object_Inst: TLayout;
    AddObjectBtn: TCornerButton;
    DelObjectBtn: TCornerButton;
    EdtObjectBtn: TCornerButton;
    CloneObjectBtn: TCornerButton;
    procedure AddObjectBtnClick(Sender: TObject);
    procedure DelObjectBtnClick(Sender: TObject);
    procedure EdtObjectBtnClick(Sender: TObject);
    procedure CloneObjectBtnClick(Sender: TObject);
  private
    { Private declarations }
    FObjecter: TObjecterPresenter;
  public
     procedure Init(const APresenter: TObjecterPresenter);
    { Public declarations }
  end;

implementation

{$R *.fmx}

{ TObjectFrame }

procedure TObjectFrame.AddObjectBtnClick(Sender: TObject);
begin
  FObjecter.AddObj;
end;

procedure TObjectFrame.CloneObjectBtnClick(Sender: TObject);
begin
  FObjecter.CloneObj;
end;

procedure TObjectFrame.DelObjectBtnClick(Sender: TObject);
begin
  FObjecter.DelObj;
end;

procedure TObjectFrame.EdtObjectBtnClick(Sender: TObject);
begin
  FObjecter.ShowOptions;
end;

procedure TObjectFrame.Init(const APresenter: TObjecterPresenter);
begin
  FObjecter := APresenter;
end;

end.
