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
    FStartShapePos: TPointF;
    FStartKeyPoint: TPointF;
    FSelectedShape: TItemShaperPresenterFriend;
    function GetHeight: Integer;
    function GetPosition: TPoint;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetPosition(const Value: TPoint);
    procedure SetWidth(const Value: Integer);
    procedure OnModelUpdate(ASender: TObject);
    function Bitmap: TBitmap;
    procedure RepaintShapes;
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
  RepaintShapes;
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
  RepaintShapes;
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
  vPoint: TPointF;
  vKeyPoint: TPointF;
begin
  inherited;

  vPoint := FView.MousePos - PointF(FView.Width / 2, FView.Height / 2);
  if FIsShapeVisible then
    for i := 0 to FShapes.Count - 1 do
      if FShapes[i].IsPointIn(vPoint) then
      begin
        FSelectedShape := FShapes[i];
        FShapes[i].MouseDown;

        if FShapes[i].KeyPointLocal(vPoint, vKeyPoint, 5, True) then
        begin
          FStartShapePos := vPoint;
          FStartKeyPoint := vKeyPoint;
          FCapturedShape := FShapes[i];
        end;

        RepaintShapes;
        Exit;
      end;

  FCapturedShape := nil;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self);
end;

procedure TItemObjecterPresenter.MouseMove;
var
  i: Integer;
  vPoint, vKeyPoint: TPointF;
  vNeedRepaint: Boolean;
begin
  inherited;

  vNeedRepaint := False;
  vPoint := FView.MousePos - PointF(FView.Width / 2, FView.Height / 2);

  if FIsShapeVisible then
  begin
    if FCapturedShape = nil then
      for i := 0 to FShapes.Count - 1 do
        if FShapes[i].IsPointIn(vPoint) then
        begin
          vNeedRepaint := True;
         // FSelectedShape := FShapes[i];
          FShapes[i].KeyPointLocal(vPoint, vKeyPoint, 5, True);

        //  vKeyPoint := vKeyPoint + PointF(FView.Width / 2, FView.Height / 2);
          FShapes[i].MouseMove;
        end;

     if FCapturedShape <> nil then
     begin
       FCapturedShape.ChangeLockedPoint(vPoint - FStartShapePos + FStartKeyPoint);
       vNeedRepaint := True;
     end;
  end;

  if vNeedRepaint then
     RepaintShapes;

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

  FCapturedShape := nil;
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

procedure TItemObjecterPresenter.RepaintShapes;
var
  i: Integer;
begin
  FBmp := Bitmap;
  for i := 0 to FShapes.Count - 1 do
    FShapes[i].Repaint(FBmp);

  FView.AssignBitmap(FBmp);
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
  RepaintShapes;

end;

end.
