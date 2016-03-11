unit uItemObjecterPresenter;

interface

uses
  uItemBasePresenter, uIItemView;

type
  TItemObjecterPresenter = class(TItemBasePresenter)
  public
    constructor Create(const AItemView: IItemView);
{    procedure Select; override;
    procedure Capture; override;
    procedure UnCapture; override;
    procedure Hover; override;
    procedure StartDrag; override;
    procedure EndDrag; override;     }
    procedure MouseDown; override;
    procedure MouseUp; override;
    procedure MouseMove; override;
    procedure Delete; override;
  end;

implementation

{ TObjecterItemPresenter }

constructor TItemObjecterPresenter.Create(const AItemView: IItemView);
begin

end;

procedure TItemObjecterPresenter.Delete;
begin
  inherited;

end;

procedure TItemObjecterPresenter.MouseDown;
begin
  inherited;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self);
end;

procedure TItemObjecterPresenter.MouseMove;
begin
  inherited;
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self);
end;

procedure TItemObjecterPresenter.MouseUp;
begin
  inherited;
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self)
end;

end.
