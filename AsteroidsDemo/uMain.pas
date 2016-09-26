unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  System.Math.Vectors,
  uSoEngine, uGame;

type
  TAsteroidsVsYou = class(TForm)
  published
    MainImg: TImage;
    procedure FormCreate(Sender: TObject);
  private
    FEngine: TSoEngine;
    FGame: TGame;
    procedure OnEndPaintDefault(ASender: TObject; AImage: TImage);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AsteroidsVsYou: TAsteroidsVsYou;

implementation

{$R *.fmx}

procedure TAsteroidsVsYou.FormCreate(Sender: TObject);
begin
  FEngine := TSoEngine.Create(MainImg);
  FEngine.WorldManager.OnEndPaint := OnEndPaintDefault;
  FGame := TGame.Create(FEngine.TemplateManager, FEngine.WorldManager, FEngine.UnitManager);

  FEngine.Start;
end;

procedure TAsteroidsVsYou.OnEndPaintDefault(ASender: TObject;
  AImage: TImage);
begin
{$IFDEF RELEASE}
  Exit;
{$ENDIF}
  with AImage do
  begin
    //Bitmap.Canvas.Blending:=true;
    Bitmap.Canvas.SetMatrix(tMatrix.Identity);
    Bitmap.Canvas.Fill.Color := TAlphaColorRec.Brown;
    Bitmap.Canvas.Font.Size := 12;
    Bitmap.Canvas.Font.Style := [TFontStyle.fsBold];
    Bitmap.Canvas.Font.Family := 'arial';
{$IFDEF CONDITIONALEXPRESSIONS}
{$IF CompilerVersion >= 19.0}
    Bitmap.Canvas.FillText(RectF(15, 15, 165, 125),
      'FPS=' + floattostr(FEngine.Fps), false, 1, [], TTextAlign.Leading);
{$ENDIF}{$ENDIF}
{$IFDEF VER260}
    Bitmap.Canvas.FillText(RectF(15, 15, 165, 125),
      'FPS=' + floattostr(FEngine.Fps), false, 1, [], TTextAlign.taLeading);
{$ENDIF}
  end;
end;

end.
