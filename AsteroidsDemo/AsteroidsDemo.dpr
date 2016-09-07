program AsteroidsDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {AsteroidsVsYou},
  uModel in 'Model\uModel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAsteroidsVsYou, AsteroidsVsYou);
  Application.Run;
end.
