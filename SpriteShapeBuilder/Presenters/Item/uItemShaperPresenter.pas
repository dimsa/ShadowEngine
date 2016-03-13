unit uItemShaperPresenter;

interface

uses
  System.Types, FMX.Graphics, System.UITypes,
  uItemBasePresenter, uIItemView, uNewFigure;

type
  TItemShaperPresenter = class(TItemBasePresenter)
  private
    FShape: TNewFigure;
    function GetHeight: Integer;
    function GetPosition: TPoint;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetPosition(const Value: TPoint);
    procedure SetWidth(const Value: Integer);
  protected
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Position: TPoint read GetPosition write SetPosition;
    function IsPointIn(const APoint: TPointF): Boolean;
  public
    procedure MakeCircle;
    procedure MakePoly;
    procedure AddPoint;
    procedure Repaint;
    procedure MouseDown; override;
    procedure MouseUp; override;
    procedure MouseMove; override;
    procedure Delete; override;
    destructor Destroy; override;
  end;

implementation

{ TObjecterItemPresenter }

procedure TItemShaperPresenter.AddPoint;
begin
  if FShape = nil then
    Exit;

  if FShape.Kind = TNewFigure.cfPoly then
    FShape.AddPoint(TPoint.Zero);
end;

procedure TItemShaperPresenter.Delete;
begin
  inherited;

end;

destructor TItemShaperPresenter.Destroy;
begin
  if FShape <> nil then
    FShape.Free;

  inherited;
end;

function TItemShaperPresenter.GetHeight: Integer;
begin
//  Result := FShape.Boun
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

procedure TItemShaperPresenter.MakeCircle;
begin
  if FShape = nil then
    FShape := TNewFigure.CreateCircle;
end;

procedure TItemShaperPresenter.MakePoly;
begin
  if FShape = nil then
    FShape := TNewFigure.CreatePoly;
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

procedure TItemShaperPresenter.Repaint;
var
  vBmp: TBitmap;
begin
  vBmp := TBitmap.Create(Width, Height);

  FShape.Draw(vBmp.Canvas, TAlphaColorRec.Aqua);

  FView.AssignBitmap(vBmp);

  vBmp.Free;
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
