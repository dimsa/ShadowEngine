unit uItemShaperPresenter;

interface

uses
  System.Types,
  uItemBasePresenter, uIItemView;

type
  TItemShaperPresenter = class(TItemBasePresenter)
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
    function IsPointIn(const APoint: TPointF): Boolean;
    procedure MouseDown; override;
    procedure MouseUp; override;
    procedure MouseMove; override;
    procedure Delete; override;
  end;

implementation

{ TObjecterItemPresenter }

procedure TItemShaperPresenter.Delete;
begin
  inherited;

end;

function TItemShaperPresenter.GetHeight: Integer;
begin
  Result := FView.Height;
end;

function TItemShaperPresenter.GetPosition: TPoint;
begin
  Result := Point(FView.Left, FView.Top);
end;

function TItemShaperPresenter.GetWidth: Integer;
begin
  Result := FView.Width;
end;

function TItemShaperPresenter.IsPointIn(const APoint: TPointF): Boolean;
begin

end;

procedure TItemShaperPresenter.MouseDown;
begin
  inherited;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self);
end;

procedure TItemShaperPresenter.MouseMove;
begin
  inherited;
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self);
end;

procedure TItemShaperPresenter.MouseUp;
begin
  inherited;
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self)
end;

procedure TItemShaperPresenter.SetHeight(const Value: Integer);
begin
  FView.Height := Value;
end;

procedure TItemShaperPresenter.SetPosition(const Value: TPoint);
begin
  FView.Left := Value.X;
  FView.Top := Value.Y;
end;

procedure TItemShaperPresenter.SetWidth(const Value: Integer);
begin
  FView.Width := Value;
end;

end.

implementation

end.
