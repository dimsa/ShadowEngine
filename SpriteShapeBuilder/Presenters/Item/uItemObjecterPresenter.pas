unit uItemObjecterPresenter;

interface

uses
  System.Types, System.Generics.Collections, uIntersectorClasses, FMX.Graphics,
  System.UITypes, FMX.Types, {$IFDEF VER290} System.Math.Vectors,
  {$ENDIF} System.Math,
  uItemBasePresenter, uItemShaperPresenter, uIItemView, uSSBModels;

type
  TItemShaperPresenterFriend = class(TItemShaperPresenter);

  TItemObjecterPresenter = class(TItemBasePresenter)
  private
    FBmp: TBitmap; // Picture of object with Shapes
    FShapes: TList<TItemShaperPresenterFriend>;
    FItemObjectModel: TItemObjectModel;
    FIsShapeVisible: Boolean;
    FCapturedShape: TItemShaperPresenterFriend;
    FSelectedShaper: TItemShaperPresenterFriend;
    function GetHeight: Integer;
    function GetPosition: TPoint;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetPosition(const Value: TPoint);
    procedure SetWidth(const Value: Integer);
    procedure OnModelUpdate(ASender: TObject);
    function Bitmap: TBitmap;
  public
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Position: TPoint read GetPosition write SetPosition;
    procedure ShowShapes;
    procedure HideShapes;
    procedure AddPoly;
    procedure AddCircle;
    procedure MouseDown; override;
    procedure MouseUp; override;
    procedure MouseMove; override;
    procedure Delete; override;
    constructor Create(const AItemView: IItemView; const AItemObjectModel: TItemObjectModel);
    destructor Destroy; override;
  end;

implementation

{ TObjecterItemPresenter }

procedure TItemObjecterPresenter.AddCircle;
var
  vShape: TItemShaperPresenterFriend;
  vShapeModel: TItemShapeModel;
  vCircle: TCircle;
begin
  vShapeModel := TItemShapeModel.CreateCircle(OnModelUpdate);

  vCircle.X := 0;
  vCircle.Y := 0;
  vCircle.Radius := FItemObjectModel.Width / 4;
  vShapeModel.SetData(vCircle);

  vShape := TItemShaperPresenterFriend.Create(FView, vShapeModel);
  FItemObjectModel.AddShape(vShapeModel);
  FShapes.Add(vShape);
  FBmp := Bitmap;
  vShape.Repaint(FBmp);
  FView.AssignBitmap(FBmp);
end;

procedure TItemObjecterPresenter.AddPoly;
var
  vShape: TItemShaperPresenterFriend;
  vShapeModel: TItemShapeModel;
  vPoly: TPolygon;
begin
  vShapeModel := TItemShapeModel.CreatePoly(OnModelUpdate);

  SetLength(vPoly, 3);
  vPoly[0] := PointF(0, -FItemObjectModel.Height / 4);
  vPoly[1] := PointF(-FItemObjectModel.Width / 4, FItemObjectModel.Height / 4);
  vPoly[2] := PointF(FItemObjectModel.Width / 4, FItemObjectModel.Height / 4);

  vShapeModel.SetData(vPoly);

  vShape := TItemShaperPresenterFriend.Create(FView, vShapeModel);
  FItemObjectModel.AddShape(vShapeModel);
  FShapes.Add(vShape);
  FBmp := Bitmap;
  vShape.Repaint(FBmp);
  FView.AssignBitmap(FBmp);
end;

function TItemObjecterPresenter.Bitmap: TBitmap;
begin
  FBmp.Width := Width;
  FBmp.Height := Height;

    FBmp.Canvas.BeginScene();
    FBmp.Canvas.StrokeThickness := 5;
    FBmp.Canvas.Stroke.Color := TAlphaColorRec.Red;
    FBmp.Canvas.Fill.Color := TAlphaColorRec.Blue;
    FBmp.Canvas.FillRect(
    RectF(0, 0, Width, Height), 0, 0, [], 1, FMX.Types.TCornerType.ctBevel);
    FBmp.Canvas.EndScene;

  Result := FBmp;
end;

constructor TItemObjecterPresenter.Create(const AItemView: IItemView; const AItemObjectModel: TItemObjectModel);
begin
  inherited Create(AItemView);

  FBmp := TBitmap.Create;
  FItemObjectModel := AItemObjectModel;
  FItemObjectModel.UpdateHander := OnModelUpdate;
  FShapes := TList<TItemShaperPresenterFriend>.Create;
end;

procedure TItemObjecterPresenter.Delete;
begin
  inherited;

end;

destructor TItemObjecterPresenter.Destroy;
var
  i: Integer;
begin
  for i := 0 to FShapes.Count - 1 do
    FShapes[i].Free;
  FShapes.Free;
  inherited;
end;

function TItemObjecterPresenter.GetHeight: Integer;
begin
  Result := FItemObjectModel.Height;
end;

function TItemObjecterPresenter.GetPosition: TPoint;
begin
  Result := FItemObjectModel.Position;
end;

function TItemObjecterPresenter.GetWidth: Integer;
begin
  Result := FItemObjectModel.Width;
end;

procedure TItemObjecterPresenter.HideShapes;
begin
  FIsShapeVisible := False;
end;

procedure TItemObjecterPresenter.MouseDown;
var
  i: Integer;
begin
  inherited;

  if FIsShapeVisible then
    for i := 0 to FShapes.Count - 1 do
      if FShapes[i].IsPointIn(FView.MousePos) then
        FShapes[i].MouseDown;

  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self);
end;

procedure TItemObjecterPresenter.MouseMove;
var
  i: Integer;
begin
  inherited;

  if FIsShapeVisible then
    for i := 0 to FShapes.Count - 1 do
      if FShapes[i].IsPointIn(FView.MousePos) then
        FShapes[i].MouseMove;

  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self);
end;

procedure TItemObjecterPresenter.MouseUp;
var
  i: Integer;
begin
  inherited;

  if FIsShapeVisible then
    for i := 0 to FShapes.Count - 1 do
      if FShapes[i].IsPointIn(FView.MousePos) then
        FShapes[i].MouseUp;

  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self)
end;

procedure TItemObjecterPresenter.OnModelUpdate(ASender: TObject);
begin
  FView.Width := FItemObjectModel.Width;
  FView.Height:= FItemObjectModel.Height;
  FView.Left:= FItemObjectModel.Position.X;
  FView.Top := FItemObjectModel.Position.Y;
end;

procedure TItemObjecterPresenter.SetHeight(const Value: Integer);
begin
  FItemObjectModel.Height := Value;
end;

procedure TItemObjecterPresenter.SetPosition(const Value: TPoint);
begin
  FItemObjectModel.Position := Value;
end;

procedure TItemObjecterPresenter.SetWidth(const Value: Integer);
begin
  FItemObjectModel.Width:= Value;
end;

procedure TItemObjecterPresenter.ShowShapes;
var
  i: Integer;
begin
  FIsShapeVisible := True;

  for i := 0 to FShapes.Count - 1 do
    FShapes[i].Repaint(FBmp);
end;

end.
