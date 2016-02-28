unit uItemObjecterPresenter;

interface

uses
  uItemBasePresenter, uIItemView;

type
  TObjecterItemPresenter = class(TItemBasePresenter)
  public
    constructor Create(const AItemView: IItemView);
  end;

implementation

{ TObjecterItemPresenter }

constructor TObjecterItemPresenter.Create(const AItemView: IItemView);
begin

end;

end.
