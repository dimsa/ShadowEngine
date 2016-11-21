unit uStatusSelectorFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, uMainPresenter;

type
  TStatusSelectorFrame = class(TFrame)
    InsrumentTabs: TLayout;
    LineOneLayout: TLayout;
    Object_rect: TRectangle;
    Object_img: TImage;
    Picture_rect: TRectangle;
    Picture_img: TImage;
    Shape_rect: TRectangle;
    Shape_img: TImage;
    LineTwoLayout: TLayout;
    Rendition_rect: TRectangle;
    Rendition_img: TImage;
    Sounder_rect: TRectangle;
    Sounder_img: TImage;
    procedure Picture_imgClick(Sender: TObject);
    procedure Object_imgClick(Sender: TObject);
    procedure Shape_imgClick(Sender: TObject);
  private
    FPresenter: TMainPresenter;
    { Private declarations }
  public
    procedure Init(const APresenter: TMainPresenter);
    { Public declarations }
  end;

implementation

{$R *.fmx}

{ TStatusSelectorFrame }

procedure TStatusSelectorFrame.Init(const APresenter: TMainPresenter);
begin
  FPresenter := APresenter;
end;

procedure TStatusSelectorFrame.Object_imgClick(Sender: TObject);
begin
  FPresenter.InitObjecter;
end;

procedure TStatusSelectorFrame.Picture_imgClick(Sender: TObject);
begin
  FPresenter.InitImager;
end;

procedure TStatusSelectorFrame.Shape_imgClick(Sender: TObject);
begin
  FPresenter.InitShaper;
end;

end.
