unit mainUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Platform, FMX.VirtualKeyboard,
  uEasyDevice, uDemoGame;

type
  TmainForm = class(TForm)
    mainImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
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
  uNewFigure, uIntersectorClasses;

{$R *.fmx}

procedure TmainForm.FormCreate(Sender: TObject);
begin
  Game := TDemoGame.Create;
  Game.Image := mainImage;
  DrawSelect := False;
  Game.Prepare;
// mainImage.Canvas.st
end;

procedure TmainForm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  Game.DrawFigures := not Game.DrawFigures;

  if ReturnPressed(Key) then
  begin
    case Game.GameStatus of
      gsMenu2: Game.GameStatus := gsMenu1;
      gsMenu3: Game.GameStatus := gsMenu2;
      gsStatics: Game.GameStatus := gsMenu1;
      gsAbout: Game.GameStatus := gsMenu1;
      gsStoryMode, gsRelaxMode, gsSurvivalMode, gsGameOver, gsComix1, gsComix2, gsComix3: Game.GameStatus := gsMenu1;
     end;
  end;
end;

procedure TmainForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if ReturnPressed(Key) then
    Key := 0;
end;

procedure TmainForm.FormResize(Sender: TObject);
var
  vSize: tPointF;
begin
  vSize := getDisplaySizeInPx;
  Game.Resize;
  mainImage.Position.X := 0;
  mainImage.Position.Y := 0;
  mainImage.Width := Round(vSize.X + 0.4);
  mainImage.Height := Round(vSize.Y + 0.4);
  mainImage.Bitmap.Width := Round(vSize.X + 0.4);
  mainImage.Bitmap.Height := Round(vSize.Y + 0.4);
end;


end.


