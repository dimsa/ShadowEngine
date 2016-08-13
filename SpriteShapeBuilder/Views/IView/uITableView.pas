unit uITableView;

interface

uses
  System.Generics.Collections, uClasses,
  uMVPFrameWork;

type

ITableView = interface(IView)
  ['{70C20B55-5D1C-4342-AD0E-8A4DD03177A4}']
  procedure ShowParams(const AParams: TDictionary<string,string>);
{  function TakeParams: TDictionary<string,string>;

  function GetPresenter: TItemBasePresenter;
  procedure SetPresenter(AValue: TItemBasePresenter);
  property Presenter: TItemBasePresenter read GetPresenter write SetPresenter; }
  procedure SetOnTakeParams(const Value: TEvent<TDictionary<string,string>>);
end;

implementation

end.
