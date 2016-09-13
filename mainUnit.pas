unit mainUnit;

interface

uses
  System.Types, System.UITypes, FMX.Forms, FMX.Objects, FMX.Controls,
  System.Classes, FMX.Types, FMX.Dialogs, FMX.StdCtrls,
  FMX.Advertising,
  uEasyDevice, uDemoGame, uBannerPanel, FMX.Layouts, FMX.Controls.Presentation, FMX.Edit;

type
  TmainForm = class(TForm)
    mainImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
{    procedure mainImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);   }
  private
    Game: TDemoGame;
    {$IFDEF RELEASE}
    BannerPanel: TBannerPanel;
    {$ENDIF}
    { Private declarations }
  public
    { Public declarations }
  end;

var
  mainForm: TmainForm;

implementation

{$R *.fmx}

procedure TmainForm.FormCreate(Sender: TObject);
begin
  {$DEFINE DEBUG}

  Game := TDemoGame.Create;
  Game.Image := mainImage;

  {$IFDEF ANDROID}
  BorderStyle := TFmxFormBorderStyle.None;
  {$ENDIF}

  {$IFDEF RELEASE}
  BannerPanel := TBannerPanel.Create(mainForm);
  {$I private/admob.inc} // Remove for your project.
  BannerPanel.Prepare;
  Game.Banners := BannerPanel;
  {$ENDIF}
  Game.Prepare;
  Game.Start;
end;

procedure TmainForm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if KeyChar = #32 then
    Game.DrawFigures := not Game.DrawFigures;

  if ReturnPressed(Key) then
  begin
    {$IFDEF RELEASE}
    if BannerPanel.Visible then
    begin
      BannerPanel.Visible := False;
      Exit;
    end;
   {$ENDIF}

    case Game.GameStatus of
      gsMenu2: Game.GameStatus := gsMenu1;
      gsMenu3: Game.GameStatus := gsMenu2;
      gsStatistics: Game.GameStatus := gsMenu1;
      gsAbout: Game.GameStatus := gsMenu1;
      gsStoryMode, gsRelaxMode, gsSurvivalMode, gsGameOver, gsComix1, gsComix2, gsComix3, gsNextLevel, gsRetryLevel: Game.GameStatus := gsMenu1;
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
begin
  Game.Resize(Round(mainImage.Width), Round(mainImage.Height));
  {$IFDEF RELEASE}
  BannerPanel.Resize;
  {$ENDIF}
end;


end.


