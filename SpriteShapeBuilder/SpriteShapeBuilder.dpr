program SpriteShapeBuilder;

uses
  System.StartUpCopy,
  FMX.Forms,
  SSBMainForm in 'SSBMainForm.pas' {SSBForm},
  uSpriteShapeBuilder in 'uSpriteShapeBuilder.pas',
  uSSBElement in 'uSSBElement.pas',
  uNamedList in 'Utils\uNamedList.pas',
  uClasses in 'Utils\uClasses.pas',
  uEasyDevice in 'Utils\uEasyDevice.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSSBForm, SSBForm);
  Application.Run;
end.
