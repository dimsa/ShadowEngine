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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  mainForm: TmainForm;
  Game: TDemoGame;

implementation

{$R *.fmx}

procedure TmainForm.FormCreate(Sender: TObject);
var
  size: tPointF;
begin
  size := getDisplaySizeInPx;
  mainImage.Position.X:=0;
  mainImage.Position.Y:=0;
  mainImage.Width:=round(size.X+0.4);
  mainImage.Height:=round(size.Y+0.4);
  mainImage.Bitmap.Width:=round(size.X+0.4);
  mainImage.Bitmap.Height:=round(size.Y+0.4);
  Game := TDemoGame.Create;
  Game.Image := mainImage;
  Game.Prepare;
end;

end.
