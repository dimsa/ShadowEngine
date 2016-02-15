unit uItemPresenterFacade;

interface

uses
  uIItemPresenter;

type

TItemPresenterFacade = class
private
  FPresenter: IItemPresenter;
  procedure SetPresenter(const Value: IItemPresenter);
public
  procedure Select;
  procedure StartDrag;
  procedure EndDrag;
  procedure Delete;
  property Presenter: IItemPresenter write SetPresenter;
  destructor Destroy; override;
end;

implementation

{ TItemPresenterFacade }

procedure TItemPresenterFacade.Delete;
begin
  if Assigned(FPresenter) then
    FPresenter.Delete;
end;

destructor TItemPresenterFacade.Destroy;
begin
  FPresenter := nil;
  inherited;
end;

procedure TItemPresenterFacade.EndDrag;
begin
  if Assigned(FPresenter) then
    FPresenter.EndDrag;
end;

procedure TItemPresenterFacade.Select;
begin
  if Assigned(FPresenter) then
    FPresenter.Select;
end;

procedure TItemPresenterFacade.SetPresenter(const Value: IItemPresenter);
begin
  FPresenter := nil;
  FPresenter := Value;
end;

procedure TItemPresenterFacade.StartDrag;
begin
  if Assigned(FPresenter) then
    FPresenter.StartDrag;
end;

end.
