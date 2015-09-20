unit mainUnit;

interface

uses
  System.Types, System.UITypes, FMX.Forms, FMX.Objects, FMX.Controls,
  System.Classes, FMX.Types, FMX.Dialogs, FMX.StdCtrls,
  FMX.Advertising, FMX.Controls.Presentation,
  uEasyDevice, uDemoGame, uBannerPanel, FMX.Layouts;

type
  TmainForm = class(TForm)
    mainImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure Banner1DidFail(Sender: TObject; const Error: string);
  private
    Game: TDemoGame;
    BannerPanel: TBannerPanel;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  mainForm: TmainForm;
implementation

{$R *.fmx}

procedure TmainForm.Banner1DidFail(Sender: TObject; const Error: string);
begin
  ShowMessage('Fail to load' + Error);
end;

procedure TmainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Game := TDemoGame.Create;
  Game.Image := mainImage;

  {$IFDEF ANDROID}
  BorderStyle := TFmxFormBorderStyle.None;
  {$ENDIF}

  BannerPanel := TBannerPanel.Create(mainForm);
  {$I private/admob.inc} // Remove for your project.
  BannerPanel.Prepare;
  {$IFDEF RELEASE}
  {$ENDIF}
  Game.Prepare;
end;

procedure TmainForm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if KeyChar = #32 then
    Game.DrawFigures := not Game.DrawFigures;

  if ReturnPressed(Key) then
  begin
    case Game.GameStatus of
      gsMenu2: Game.GameStatus := gsMenu1;
      gsMenu3: Game.GameStatus := gsMenu2;
      gsStatistics: Game.GameStatus := gsMenu1;
      gsAbout: Game.GameStatus := gsMenu1;
      gsStoryMode, gsRelaxMode, gsSurvivalMode, gsGameOver, gsComix1, gsComix2, gsComix3, gsNextLevel, gsRetryLevel: Game.GameStatus := gsMenu1;
     end;
  end;

  BannerPanel.Show;
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
end;

end.


