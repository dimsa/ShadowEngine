unit uItemPresenterFacade;

interface

uses
  uIItemPresenter;

type

TItemPresenterProxy = class
private
  FImagerPresenter: IItemPresenter;
  FObjecterPresenter: IItemPresenter;
  FShaperPresenter: IItemPresenter;
  procedure SetPresenter(const Value: IItemPresenter);
public
  procedure Select;
  procedure StartDrag;
  procedure EndDrag;
  procedure Delete;
  destructor Destroy; override;
end;

implementation

{ TItemPresenterFacade }

procedure TItemPresenterProxy.Delete;
begin
  if Assigned(FPresenter) then
    FPresenter.Delete;
end;

destructor TItemPresenterProxy.Destroy;
begin
  FPresenter := nil;
  inherited;
end;

procedure TItemPresenterProxy.EndDrag;
begin
  if Assigned(FPresenter) then
    FPresenter.EndDrag;
end;

procedure TItemPresenterProxy.Select;
begin
  if Assigned(FPresenter) then
    FPresenter.Select;
end;

procedure TItemPresenterProxy.SetPresenter(const Value: IItemPresenter);
begin
  FPresenter := nil;
  FPresenter := Value;
end;

procedure TItemPresenterProxy.StartDrag;
begin
  if Assigned(FPresenter) then
    FPresenter.StartDrag;
end;

end.
