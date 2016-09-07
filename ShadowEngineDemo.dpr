program ShadowEngineDemo;

uses
  System.StartUpCopy,
  FMX.Types,
  FMX.Forms,
  mainUnit in 'mainUnit.pas' {mainForm},
  uClasses in 'Utils\uClasses.pas',
  uNamedList in 'Utils\uNamedList.pas',
  uDemoGame in 'DemoCode\uDemoGame.pas',
  uDemoEngine in 'DemoCode\uDemoEngine.pas',
  uDemoGameLoader in 'DemoCode\uDemoGameLoader.pas',
  uDemoObjects in 'DemoCode\uDemoObjects.pas',
  uDemoMenu in 'DemoCode\uDemoMenu.pas',
  FMX.IniFile.Android in 'DemoCode\Utils\inifile\FMX.IniFile.Android.pas',
  FMX.IniFile.Apple in 'DemoCode\Utils\inifile\FMX.IniFile.Apple.pas',
  FMX.IniFile in 'DemoCode\Utils\inifile\FMX.IniFile.pas',
  uBannerPanel in 'DemoCode\Utils\uBannerPanel.pas' {$R *.res};

{$R *.res}

begin
  // If GlobalUseGPUCanvas true, you've got more fps, but lower font rendering quality
  // and you can not use #13 for linebreak, use sLineBreak instead
//  GlobalUseGPUCanvas := True;
  Application.Initialize;
  Application.CreateForm(TmainForm, mainForm);
  Application.Run;
end.
