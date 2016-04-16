program ShadowEngineDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  mainUnit in 'mainUnit.pas' {mainForm},
  uClasses in 'Utils\uClasses.pas',
  uNamedList in 'Utils\uNamedList.pas',
  uDemoGame in 'DemoCode\uDemoGame.pas',
  uDemoEngine in 'DemoCode\uDemoEngine.pas',
  uDemoGameLoader in 'DemoCode\uDemoGameLoader.pas',
  uDemoObjects in 'DemoCode\uDemoObjects.pas',
  uDemoMenu in 'DemoCode\uDemoMenu.pas',
  FMX.IniFile.Android in 'Utils\inifile\FMX.IniFile.Android.pas',
  FMX.IniFile.Apple in 'Utils\inifile\FMX.IniFile.Apple.pas',
  FMX.IniFile in 'Utils\inifile\FMX.IniFile.pas',
  uBannerPanel in 'Utils\uBannerPanel.pas' {$R *.res};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TmainForm, mainForm);
  Application.Run;
end.
