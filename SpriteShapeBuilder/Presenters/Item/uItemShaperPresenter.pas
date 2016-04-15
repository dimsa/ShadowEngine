unit uItemShaperPresenter;

interface

uses
  System.Types, FMX.Graphics, System.UITypes,  {$I 'Utils\DelphiCompatability.inc'}
  System.Math, uItemBasePresenter, uIntersectorClasses, uIntersectorMethods,
  uIItemView, uNewFigure, uSSBModels;

type
  TItemShaperPresenter = class(TItemBasePresenter)
  private
    FItemShapeModel: TItemShapeModel;
    FLockedIndex: Integer;
    FLockedPoint: TPointF;
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
    function KeyPointLocal(const ATestPosition: TPointF; out AKeyPoint: TPointF;
      const ADistance: Double; const ALock: Boolean): Boolean;
    procedure ChangeLockedPoint(const ANewPoint: TPointF);
    procedure TranslateFigure(const ATranslate: TPointF);
    procedure Repaint(ABmp: TBitmap; const AColor: TColor = TAlphaColorRec.Aliceblue);
  public
    property Model: TItemShapeModel read FItemShapeModel;
    procedure AddPoint;
    procedure DelPoint;
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
var
  vPoly: TPolygon;
  i: Integer;
begin
  if FItemShapeModel.Figure.Kind = TNewFigure.cfPoly then
  begin
    vPoly := FItemShapeModel.Figure.AsPoly;
    SetLength(vPoly, Length(vPoly) + 1);
    vPoly[High(vPoly)].X := 0;
    vPoly[High(vPoly)].Y := 0;
    FItemShapeModel.SetData(vPoly);
    FItemShapeModel.RaiseUpdateEvent;
  end;
end;

procedure TItemShaperPresenter.ChangeLockedPoint(const ANewPoint: TPointF);
var
  vD: single;
  vFigure: TNewFigure;
  vPoly: TPolygon;
  vCircle: TCircle;
begin
  if FLockedIndex <> -1 then
  begin
    vFigure := FItemShapeModel.Figure;
    if vFigure.Kind = TNewFigure.cfPoly then
    begin
      vPoly := vFigure.AsPoly;
      vPoly[FLockedIndex] := ANewPoint;
      vFigure.SetData(vPoly);
    end;
    if vFigure.Kind = TNewFigure.cfCircle then
    begin
      vCircle := vFigure.AsCircle;
      if FLockedIndex = 0 then
      begin
        vCircle.X := ANewPoint.X;
        vCircle.Y := ANewPoint.Y;
      end;

      if FLockedIndex = 1 then
      begin
        vD := Distance(vCircle.Center, ANewPoint); //Distance(FData[0], FLockedPoint) - Distance(FData[0], ANewPoint);
        vCircle.Radius := vD;
      end;

      vFigure.SetData(vCircle);
    end;
  end;
end;

constructor TItemShaperPresenter.Create(const AItemView: IItemView;
  const AItemShapeModel: TItemShapeModel);
begin
  inherited Create(AItemView);
  FItemShapeModel := AItemShapeModel;
  //FColor := TAlphaColorRec.Aliceblue;
end;

procedure TItemShaperPresenter.Delete;
begin
  inherited;

end;

procedure TItemShaperPresenter.DelPoint;
var
  vPoly: TPolygon;
  i: Integer;
begin
  if FItemShapeModel.Figure.Kind = TNewFigure.cfPoly then
  begin
    vPoly := FItemShapeModel.Figure.AsPoly;
    if (FLockedIndex >= 0) and (Length(vPoly) > 3) then
    begin
      for i := FLockedIndex to High(vPoly) - 1 do
        vPoly[i] := vPoly[i + 1];
      SetLength(vPoly, Length(vPoly) - 1);
      FItemShapeModel.SetData(vPoly);
      FLockedIndex := -1;
      FItemShapeModel.RaiseUpdateEvent;
    end;

  end;
end;

destructor TItemShaperPresenter.Destroy;
begin
  FItemShapeModel := nil;
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

function TItemShaperPresenter.KeyPointLocal(const ATestPosition: TPointF;
  out AKeyPoint: TPointF; const ADistance: Double;
  const ALock: Boolean): Boolean;
var
  vCenterToPoint, vCenterToRadius: Double;
  vArcTan: Double;
  vPoly: TPolygon;
  i: Integer;
  vFigure: TNewFigure;
  vCircle: TCircle;
begin
   vFigure := FItemShapeModel.Figure;
   case vFigure.Kind of
    TNewFigure.cfCircle:
      begin
        vCircle := vFigure.AsCircle;
        vCenterToPoint := Distance(ATestPosition, vCircle.Center);
        vCenterToRadius := vCircle.Radius;// FData[1].X;//Distance(PointF(0,0), FData[1]);
        if (vCircle.Radius - vCenterToPoint) < vCenterToPoint then
        begin
          if (vCenterToPoint <= vCircle.Radius + (ADistance)) and
           (vCenterToPoint >= vCircle.Radius - (ADistance))
          then
          begin
            vArcTan := ArcTan2(ATestPosition.Y - vCircle.Y, ATestPosition.X - vCircle.X );
            AKeyPoint := PointF(vCircle.X + vCenterToRadius * Cos(vArcTan), vCenterToRadius * Sin(vArcTan) + vCircle.Y);

            if ALock then
            begin
              FLockedIndex := 1;
              FLockedPoint := AKeyPoint - vCircle.Center;
            end;

            Exit(True);
          end;
        end else
        begin
          if vCenterToPoint <= (ADistance) then
          begin
            AKeyPoint := vCircle.Center;
            if ALock then
            begin
              FLockedIndex := 0;
              FLockedPoint := AKeyPoint - vCircle.Center;
            end;

            Exit(True);
          end;
        end;
      end;
    TNewFigure.cfPoly:
      begin
        vPoly := vFigure.AsPoly;
        for i := 0 to Length(vPoly) - 1 do
        begin
          if Distance(vPoly[i], ATestPosition) <= ADistance then
          begin
            AKeyPoint := vPoly[i];
            if ALock then
            begin
              FLockedIndex := i;
              FLockedPoint := AKeyPoint
            end;

            Exit(True);
          end;
        end;

      end;
  end;
  Result := False;
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

procedure TItemShaperPresenter.Repaint(ABmp: TBitmap; const AColor: TColor = TAlphaColorRec.Aliceblue);
begin
  ABmp.Canvas.BeginScene();
  FItemShapeModel.Figure.TempTranslate(PointF(ABmp.Width / 2, ABmp.Height / 2));
  FItemShapeModel.Figure.Draw(ABmp.Canvas, AColor{TAlphaColorRec.Aqua});

  if FLockedIndex >= 0 then
    FItemShapeModel.Figure.DrawPoint(ABmp.Canvas, FLockedPoint, TAlphaColorRec.Red);
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

procedure TItemShaperPresenter.TranslateFigure(const ATranslate: TPointF);
var
  vFigure: TNewFigure;
  vCircle: TCircle;
  vPoly: TPolygon;
begin
   vFigure := FItemShapeModel.Figure;
   case vFigure.Kind of
     TNewFigure.cfCircle:
     begin
       vCircle := vFigure.AsCircle;
       Translate(vCircle, ATranslate);
       vFigure.SetData(vCircle);
     end;
     TNewFigure.cfPoly:
     begin
       vPoly := vFigure.AsPoly;
       Translate(vPoly, ATranslate);
       vFigure.SetData(vPoly);
     end;
   end;
end;

end.
