unit uItemShaperPresenter;

interface

uses
  System.Types, FMX.Graphics, System.UITypes,
  uItemBasePresenter, uIItemView, uNewFigure, uSSBModels;

type
  TItemShaperPresenter = class(TItemBasePresenter)
  private
//    FShape: TNewFigure;
    FItemShapeModel: TItemShapeModel;
    FColor: TColor;
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
    procedure Repaint(ABmp: TBitmap);
  public
    procedure AddPoint;
    procedure MouseDown; override;
    procedure MouseUp; override;
    procedure MouseMove; override;
    procedure Delete; override;
    constructor Create(const AItemView: IItemView; const AItemShapeModel: TItemShapeModel);
    destructor Destroy; override;
  end;

implementation

{ TObjecterItemPresenter }

procedure TItemShaperPresenter.AddPoint;
begin
//  if FShape = nil then
//    Exit;
//
//  if FShape.Kind = TNewFigure.cfPoly then
//    FShape.AddPoint(TPoint.Zero);
end;

constructor TItemShaperPresenter.Create(const AItemView: IItemView;
  const AItemShapeModel: TItemShapeModel);
begin
  inherited Create(AItemView);
  FItemShapeModel := AItemShapeModel;
  FColor := TAlphaColorRec.Aliceblue;
end;

procedure TItemShaperPresenter.Delete;
begin
  inherited;

end;

destructor TItemShaperPresenter.Destroy;
begin
  FItemShapeModel := nil;
//  if FShape <> nil then
//    FShape.Free;

  inherited;
end;

function TItemShaperPresenter.GetHeight: Integer;
begin
  Result := FItemShapeModel.MaxRadius;
end;

function TItemShaperPresenter.GetPosition: TPoint;
begin
  Result := Point(FView.Left, FView.Top);
end;

function TItemShaperPresenter.GetWidth: Integer;
begin
  Result := FItemShapeModel.MaxRadius;
end;

function TItemShaperPresenter.IsPointIn(const APoint: TPointF): Boolean;
begin
  Result := FItemShapeModel.Figure.BelongPointLocal(APoint);
end;

procedure TItemShaperPresenter.MouseDown;
begin
  inherited;
  FColor := Random(MaxLongInt);
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

procedure TItemShaperPresenter.Repaint(ABmp: TBitmap);
begin
  ABmp.Canvas.BeginScene();
  FItemShapeModel.Figure.TempTranslate(PointF(Abmp.Width / 2, ABmp.Height / 2));
  FItemShapeModel.Figure.Draw(ABmp.Canvas, FColor{TAlphaColorRec.Aqua});
  FItemShapeModel.Figure.Reset;
  ABmp.Canvas.EndScene;
end;

procedure TItemShaperPresenter.SetHeight(const Value: Integer);
begin

end;

procedure TItemShaperPresenter.SetPosition(const Value: TPoint);
begin

end;

procedure TItemShaperPresenter.SetWidth(const Value: Integer);
begin

end;

end.
