unit uItemObjecterPresenter;

interface

uses
  System.Types,
  uItemBasePresenter, uIItemView;

type
  TItemObjecterPresenter = class(TItemBasePresenter)
  private
    function GetHeight: Integer;
    function GetPosition: TPoint;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetPosition(const Value: TPoint);
    procedure SetWidth(const Value: Integer);
  public
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Position: TPoint read GetPosition write SetPosition;
    procedure MouseDown; override;
    procedure MouseUp; override;
    procedure MouseMove; override;
    procedure Delete; override;
  end;

implementation

{ TObjecterItemPresenter }

procedure TItemObjecterPresenter.Delete;
begin
  inherited;

end;

function TItemObjecterPresenter.GetHeight: Integer;
begin
  Result := FView.Height;
end;

function TItemObjecterPresenter.GetPosition: TPoint;
begin
  Result := Point(FView.Left, FView.Top);
end;

function TItemObjecterPresenter.GetWidth: Integer;
begin
  Result := FView.Width;
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

procedure TItemObjecterPresenter.SetHeight(const Value: Integer);
begin
  FView.Height := Value;
end;

procedure TItemObjecterPresenter.SetPosition(const Value: TPoint);
begin
  FView.Left := Value.X;
  FView.Top := Value.Y;
end;

procedure TItemObjecterPresenter.SetWidth(const Value: Integer);
begin
  FView.Width := Value;
end;

end.
