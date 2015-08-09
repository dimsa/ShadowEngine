unit mainUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  uEasyDevice, uDemoGame;

type
  TmainForm = class(TForm)
    mainImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  mainForm: TmainForm;
  Game: TDemoGame;
  DrawSelect: Boolean;
implementation

uses
  uNewFigure, uIntersectorClasses{$IFDEF VER290}, System.Math.Vectors {$ENDIF};

{$R *.fmx}

procedure TmainForm.FormCreate(Sender: TObject);
var
  vFigure: TNewFigure;
  vCircle: TCircle;
  vPoly: TPolygon;
begin
  Game := TDemoGame.Create;
  Game.Image := mainImage;
  DrawSelect := False;
  Game.Prepare;

  SetLength(vPoly, 3);
  vPoly[0] := PointF(25, 25);
  vPoly[1] := PointF(37, 37);
  vPoly[2] := PointF(-80, -80);

  vCircle.X := 26;
  vCircle.Y := -26;
  vCircle.Radius := 99;

  vFigure := TNewFigure.Create(TNewFigure.cfPoly);
  vFigure.SetData(vPoly);
end;

procedure TmainForm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  DrawSelect := Not DrawSelect;
end;

procedure TmainForm.FormResize(Sender: TObject);
var
  vSize: tPointF;
begin
  vSize := getDisplaySizeInPx;
  Game.Resize(vSize);
  mainImage.Position.X := 0;
  mainImage.Position.Y := 0;
  mainImage.Width:=round(vSize.X + 0.4);
  mainImage.Height:=round(vSize.Y + 0.4);
  mainImage.Bitmap.Width:=round(vSize.X + 0.4);
  mainImage.Bitmap.Height:=round(vSize.Y + 0.4);

end;

end.


