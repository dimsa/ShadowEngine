unit uITableView;

interface

uses
  System.Generics.Collections,
  uMVPFrameWork, uIItemPresenter;

type

ITableView = interface(IView)
  ['{70C20B55-5D1C-4342-AD0E-8A4DD03177A4}']
  procedure ShowParams(const AParams: TDictionary<string,string>);
  function GetPresenter: IItemPresenter;
  procedure SetPresenter(AValue: IItemPresenter);
  property Presenter: IItemPresenter read GetPresenter write SetPresenter;
end;

implementation

end.
