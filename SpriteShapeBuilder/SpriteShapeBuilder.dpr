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
  uMainPresenter__ in 'Presenters\uMainPresenter__.pas',
  uView in 'View\uView.pas',
  uSSBTypes in 'uSSBTypes.pas',
  uIView in 'IView\uIView.pas',
  uIItemView in 'IView\uIItemView.pas',
  uItemView in 'View\uItemView.pas',
  uMainPresenter in 'Presenters\uMainPresenter.pas',
  uImagerItemPresenter in 'Presenters\uImagerItemPresenter.pas',
  uMVPFrameWork in 'Utils\uMVPFrameWork.pas',
  uItemPresenterProxy in 'Presenters\uItemPresenterProxy.pas',
  uIItemPresenter in 'Presenters\uIItemPresenter.pas',
  uIPresenterEvent in 'Presenters\uIPresenterEvent.pas',
  uBaseItemPresenter in 'Presenters\uBaseItemPresenter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSSBForm, SSBForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.Run;
end.
