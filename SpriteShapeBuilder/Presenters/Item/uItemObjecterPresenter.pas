unit uItemObjecterPresenter;

interface

uses
  uItemBasePresenter, uIItemView;

type
  TItemObjecterPresenter = class(TItemBasePresenter)
  public
    constructor Create(const AItemView: IItemView);
    procedure Select; override;
    procedure Capture; override;
    procedure UnCapture; override;
    procedure Hover; override;
    procedure StartDrag; override;
    procedure EndDrag; override;
    procedure Delete; override;
  end;

implementation

{ TObjecterItemPresenter }

procedure TItemObjecterPresenter.Capture;
begin
  inherited;

end;

constructor TItemObjecterPresenter.Create(const AItemView: IItemView);
begin

end;

procedure TItemObjecterPresenter.Delete;
begin
  inherited;

end;

procedure TItemObjecterPresenter.EndDrag;
begin
  inherited;

end;

procedure TItemObjecterPresenter.Hover;
begin
  inherited;

end;

procedure TItemObjecterPresenter.Select;
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

procedure TItemObjecterPresenter.StartDrag;
begin
  inherited;

end;

procedure TItemObjecterPresenter.UnCapture;
begin
  inherited;

end;

end.
