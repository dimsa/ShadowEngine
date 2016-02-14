program SpriteShapeBuilder;

uses
  System.StartUpCopy,
  FMX.Forms,
  SSBMainForm in 'SSBMainForm.pas' {SSBForm},
  uSpriteShapeBuilder in 'uSpriteShapeBuilder.pas',
  uSSBElement in 'uSSBElement.pas',
  uNamedList in 'Utils\uNamedList.pas',
  uClasses in 'Utils\uClasses.pas',
  uEasyDevice in 'Utils\uEasyDevice.pas',
  uIntersectorClasses in 'Intersector\uIntersectorClasses.pas',
  uIntersectorMethods in 'Intersector\uIntersectorMethods.pas',
  uNewFigure in 'Intersector\uNewFigure.pas',
  uSSBFigure in 'uSSBFigure.pas',
  SSBOptionsForm in 'SSBOptionsForm.pas' {OptionsForm},
  uSSBModels in 'Model\uSSBModels.pas',
  uSSBPresenters in 'Presenters\uSSBPresenters.pas',
  uView in 'View\uView.pas',
  uSSBTypes in 'uSSBTypes.pas',
  uIView in 'IView\uIView.pas',
  uIItemView in 'IView\uIItemView.pas',
  uItemView in 'View\uItemView.pas',
  uImagerPresenter in 'Presenters\uImagerPresenter.pas',
  uImagerItemPresenter in 'Presenters\uImagerItemPresenter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSSBForm, SSBForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.Run;
end.
