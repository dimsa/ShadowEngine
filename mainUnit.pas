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
  uNewFigure, uIntersectorClasses;

{$R *.fmx}

procedure TmainForm.FormCreate(Sender: TObject);
begin
  Game := TDemoGame.Create;
  Game.Image := mainImage;
  DrawSelect := False;
  Game.Prepare;
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
  mainImage.Width := Round(vSize.X + 0.4);
  mainImage.Height := Round(vSize.Y + 0.4);
  mainImage.Bitmap.Width := Round(vSize.X + 0.4);
  mainImage.Bitmap.Height := Round(vSize.Y + 0.4);

end;

end.


