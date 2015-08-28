unit mainUnit;

interface

uses
  System.Types, System.UITypes, FMX.Forms, FMX.Objects, FMX.Controls,
  System.Classes, FMX.Types, FMX.Dialogs, System.SysUtils,
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
  vAlign: Integer;
implementation

{$R *.fmx}

procedure TmainForm.FormCreate(Sender: TObject);
begin
  Game := TDemoGame.Create;
  Game.Image := mainImage;
  DrawSelect := False;
  Game.Prepare;


  {$IFDEF ANDROID}
    BorderStyle := TFmxFormBorderStyle.None;
  {$ENDIF}
// mainImage.Canvas.st
  vAlign := 0;
end;

procedure TmainForm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
var
  vSize: tPointF;
begin
  vSize := getDisplaySizeInPx;
//  Game.DrawFigures := not Game.DrawFigures;
  ShowMessage(FloatToStr(vSize.x) + ' -display_size-' + FloatToStr(vSize.y));
  ShowMessage(FloatToStr(Game.Image.Width) + ' -image.width-' + FloatToStr(Game.Image.Height));
  ShowMessage(FloatToStr(mainForm.Width) + ' -mainform.size-' + FloatToStr(mainForm.Height));
    ShowMessage(FloatToStr(mainForm.ClientWidth) + ' -mainform. client size-' + FloatToStr(mainForm.ClientHeight));
  ShowMessage(FloatToStr(Game.Image.Bitmap.Width) + ' -image.bitmap.size-' + FloatToStr(Game.Image.Bitmap.Height));
    ShowMessage(FloatToStr(Game.Image.Bitmap.Canvas.Width) + ' -image.bitmap.canvas.size-' + FloatToStr(Game.Image.Bitmap.Canvas.Height));
    ShowMessage(FloatToStr(Game.Image.Size.Width) + ' - image.SIZE.width -' + FloatToStr(Game.Image.Size.Height));
     ShowMessage(FloatToStr(Game.Image.Scale.X) + ' - image.scale -' + FloatToStr(Game.Image.Scale.Y));

  inc(vAlign);
  mainImage.Align := TAlignLayout(vAlign);

  ShowMessage(IntToStr(vAlign) );

  if ReturnPressed(Key) then
  begin
    case Game.GameStatus of
      gsMenu2: Game.GameStatus := gsMenu1;
      gsMenu3: Game.GameStatus := gsMenu2;
      gsStatistics: Game.GameStatus := gsMenu1;
      gsAbout: Game.GameStatus := gsMenu1;
      gsStoryMode, gsRelaxMode, gsSurvivalMode, gsGameOver, gsComix1, gsComix2, gsComix3, gsNextLevel: Game.GameStatus := gsMenu1;
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
//var
//  vSize: tPointF;
begin
 { vSize := getDisplaySizeInPx; }
//  mainImage.Position.X := 0;
 // mainImage.Position.Y := 0;
{  mainImage.Width := mainForm.ClientWidth;
  mainImage.Height:= mainForm.ClientHeight;   }
//  ShowMessage(FloatToStr(Game.Image.Width) + ' -image.size-' + FloatToStr(Game.Image.Height));
  Game.Resize;
end;


end.



