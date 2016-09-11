program AsteroidsDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {AsteroidsVsYou},
  uModel in 'Model\uModel.pas',
  uGame in 'Model\uGame.pas',
  uMapPainter in 'Other\uMapPainter.pas',
  uUnitCreator in 'Other\uUnitCreator.pas',
  uUtils in 'Common\uUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAsteroidsVsYou, AsteroidsVsYou);
  Application.Run;
end.
