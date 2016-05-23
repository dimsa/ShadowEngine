program SpriteShapeBuilder;

uses
  System.StartUpCopy,
  FMX.Forms,
  SSBMainForm in 'SSBMainForm.pas' {SSBForm},
  uNamedList in 'Utils\uNamedList.pas',
  uClasses in 'Utils\uClasses.pas',
  uEasyDevice in 'Utils\uEasyDevice.pas',
  uIntersectorClasses in 'Intersector\uIntersectorClasses.pas',
  uIntersectorMethods in 'Intersector\uIntersectorMethods.pas',
  uNewFigure in 'Intersector\uNewFigure.pas',
  uNamedOptionsForm in 'uNamedOptionsForm.pas' {NamedOptionsForm},
  uSSBModels in 'Model\uSSBModels.pas',
  uSSBTypes in 'uSSBTypes.pas',
  uMVPFrameWork in 'Utils\uMVPFrameWork.pas',
  uIItemPresenter in 'Presenters\Item\uIItemPresenter.pas',
  uIItemPresenterEvent in 'Presenters\Item\uIItemPresenterEvent.pas',
  uItemBasePresenter in 'Presenters\Item\uItemBasePresenter.pas',
  uItemImagerPresenter in 'Presenters\Item\uItemImagerPresenter.pas',
  uItemObjecterPresenter in 'Presenters\Item\uItemObjecterPresenter.pas',
  uImagerPresenter in 'Presenters\Main\uImagerPresenter.pas',
  uObjecterPresenter in 'Presenters\Main\uObjecterPresenter.pas',
  uBasePresenterIncapsulator in 'Presenters\Main\uBasePresenterIncapsulator.pas',
  uItemShaperPresenter in 'Presenters\Item\uItemShaperPresenter.pas',
  uStreamUtil in 'Utils\uStreamUtil.pas',
  uMainModel in 'Model\uMainModel.pas',
  uIItemView in 'Views\IView\uIItemView.pas',
  uIWorkSpaceView in 'Views\IView\uIWorkSpaceView.pas',
  uItemView in 'Views\View\uItemView.pas',
  uWorkSpaceView in 'Views\View\uWorkSpaceView.pas',
  uITableView in 'Views\IView\uITableView.pas',
  uNamedTableView in 'Views\View\uNamedTableView.pas',
  uOptionsForm in 'uOptionsForm.pas' {OptionsForm},
  uMainPresenter in 'Presenters\uMainPresenter.pas',
  uIMainView in 'Views\IView\uIMainView.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSSBForm, SSBForm);
  Application.Run;
end.
