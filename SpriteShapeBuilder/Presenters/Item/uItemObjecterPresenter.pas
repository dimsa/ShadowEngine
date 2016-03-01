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

end;

procedure TItemObjecterPresenter.MouseMove;
begin
  inherited;

end;

procedure TItemObjecterPresenter.MouseUp;
begin
  inherited;

end;

end.
