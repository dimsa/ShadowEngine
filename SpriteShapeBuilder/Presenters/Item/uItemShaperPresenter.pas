unit uItemShaperPresenter;

interface

uses
  System.Types, FMX.Graphics, System.UITypes,  {$I 'Utils\DelphiCompatability.inc'}
  System.Math, uItemBasePresenter, uIntersectorClasses, uIntersectorMethods,
  System.Generics.Collections, System.SysUtils, System.Classes,  uClasses,
  uIItemView, uSSBModels, uITableView;

type
  TItemShaperPresenter = class(TItemBasePresenter)
  private
    FItemShapeModel: TItemShapeModel;
    FOnCreateShapeModel: TNotifyEvent;
    FParams: TDictionary<string, string>;
    FLockedIndex: Integer;
    FLockedPoint: TPointF;
    FOnFigureUpdated: TNotifyEvent;
    function GetHeight: Integer;
    function GetPosition: TPoint;
    function GetWidth: Integer;
    procedure SetPosition(const Value: TPoint);
    function GetParams: TDictionary<string,string>;
    procedure SetParams(const AValue: TDictionary<string, string>); override;
    procedure OnModelUpdate(ASender: TObject);
    procedure RaiseOnCreateShapeModel;
  protected
    property Position: TPoint read GetPosition write SetPosition;
    function IsPointIn(const APoint: TPointF): Boolean;
    function KeyPointLocal(const ATestPosition: TPointF; out AKeyPoint: TPointF;
      const ADistance: Double; const ALock: Boolean): Boolean;
    procedure ChangeLockedPoint(const ANewPoint: TPointF);
    procedure TranslateFigure(const ATranslate: TPointF);
    procedure Repaint(ABmp: TBitmap; const AColor: TColor = TAlphaColorRec.Aliceblue);
    property Model: TItemShapeModel read FItemShapeModel;
    property OnFigureUpdated: TNotifyEvent read FOnFigureUpdated write FOnFigureUpdated;
  public
    procedure CreateCircle;
    procedure CreatePoly;
    procedure AddPoint;
    procedure DelPoint;
    procedure MouseDown; override;
    procedure MouseUp; override;
    procedure MouseMove; override;
    procedure ShowOptions; override;

    constructor Create(const AItemView: IItemView; OnCreateShapeModel: TNotifyEvent); overload;
    constructor Create(const AItemView: IItemView; AShapeModel: TItemShapeModel); overload;
    destructor Destroy; override;
  end;

implementation

{ TObjecterItemPresenter }

procedure TItemShaperPresenter.AddPoint;
var
  vPoly: TPolygon;
  i: Integer;
begin
  if FItemShapeModel.FigureKind = fkPoly then
  begin
    vPoly := FItemShapeModel.AsPoly;

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
 // vFigure: TNewFigure;
  vPoly: TPolygon;
  vCircle: TCircle;
begin
  if FLockedIndex <> -1 then
  begin
   // vFigure := FItemShapeModel.Figure;
    if FItemShapeModel.FigureKind = fkPoly then
    begin
      vPoly := FItemShapeModel.AsPoly;
      vPoly[FLockedIndex] := ANewPoint;
      FItemShapeModel.SetData(vPoly);
    end;
    if FItemShapeModel.FigureKind = fkCircle then
    begin
      vCircle := FItemShapeModel.AsCircle;
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

      FItemShapeModel.SetData(vCircle);
    end;
  end;
end;

constructor TItemShaperPresenter.Create(const AItemView: IItemView; OnCreateShapeModel: TNotifyEvent);
begin
  inherited Create(AItemView);
  FParams := TDictionary<string, string>.Create;
  FOnCreateShapeModel := OnCreateShapeModel;
end;

constructor TItemShaperPresenter.Create(const AItemView: IItemView; AShapeModel: TItemShapeModel);
begin
  inherited Create(AItemView);
  FParams := TDictionary<string, string>.Create;
  FItemShapeModel := AShapeModel;
  FItemShapeModel.UpdateHander := OnModelUpdate;
end;

procedure TItemShaperPresenter.CreateCircle;
var
  vShapeModel: TItemShapeModel;
  vCircle: TCircle;
begin
 // Creating Model
  vShapeModel := TItemShapeModel.CreateCircle(OnModelUpdate);
  FItemShapeModel := vShapeModel;

  RaiseOnCreateShapeModel;
//  нужно сюда передать метод, который на вход принимает ShapeModel
//  FItemObjectModel.AddShape(vShapeModel);
end;

procedure TItemShaperPresenter.CreatePoly;
var
  vShapeModel: TItemShapeModel;
  vPoly: TPolygon;
begin
 // Creating Model
  vShapeModel := TItemShapeModel.CreatePoly(OnModelUpdate);
  FItemShapeModel := vShapeModel;

  RaiseOnCreateShapeModel;
end;

procedure TItemShaperPresenter.DelPoint;
var
  vPoly: TPolygon;
  i: Integer;
begin
  if FItemShapeModel.FigureKind = fkPoly then
  begin
    vPoly := FItemShapeModel.AsPoly;
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
  FTableView := nil;
  FParams.Free;
  inherited;
end;

procedure TItemShaperPresenter.RaiseOnCreateShapeModel;
begin
  if Assigned(FOnCreateShapeModel) then
    FOnCreateShapeModel(FItemShapeModel);
end;

function TItemShaperPresenter.GetHeight: Integer;
begin
  Result := FItemShapeModel.MaxRadius;
end;

function TItemShaperPresenter.GetParams: TDictionary<string, string>;
var
  i: integer;
  vPoly: TPolygon;
  vCircle: TCircle;
begin
  FParams.Clear;

  case Model.FigureKind of
    fkCircle:
    begin
      vCircle := Model.AsCircle;
      FParams.Add('Type', 'Circle');
      FParams.Add('X', FloatToStr(Round(vCircle.X)));
      FParams.Add('Y', FloatToStr(Round(vCircle.Y)));
      FParams.Add('Radius', FloatToStr(Round(vCircle.Radius)));
    end;

    fkPoly:
    begin
      FParams.Add('Type', 'Poly');
      vPoly := Model.AsPoly;
      for i := 0 to High(vPoly) do
      begin
        FParams.Add('X' + IntToStr(i), FloatToStr(Round(vPoly[i].X)));
        FParams.Add('Y' + IntToStr(i), FloatToStr(Round(vPoly[i].Y)));
      end;
    end;
  end;

  Result := FParams;
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
  Result := FItemShapeModel.BelongPointLocal(APoint);
end;

function TItemShaperPresenter.KeyPointLocal(const ATestPosition: TPointF;
  out AKeyPoint: TPointF; const ADistance: Double;
  const ALock: Boolean): Boolean;
var
  vCenterToPoint, vCenterToRadius: Double;
  vArcTan: Double;
  vPoly: TPolygon;
  i: Integer;
 // vFigure: TNewFigure;
  vCircle: TCircle;
begin
  // vFigure := FItemShapeModel.Figure;
   case FItemShapeModel.FigureKind of
    fkCircle:
      begin
        vCircle := FItemShapeModel.AsCircle;
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
    fkPoly:
      begin
        vPoly := FItemShapeModel.AsPoly;
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

procedure TItemShaperPresenter.OnModelUpdate(ASender: TObject);
begin
  if Assigned(FOnFigureUpdated) then
    FOnFigureUpdated(Self);
end;

procedure TItemShaperPresenter.Repaint(ABmp: TBitmap; const AColor: TColor = TAlphaColorRec.Aliceblue);
begin
  FItemShapeModel.Repaint(ABmp, FLockedPoint, FLockedIndex, AColor);
end;

procedure TItemShaperPresenter.SetParams(const AValue: TDictionary<string, string>);
var
  vErr, vA: Integer;
  i: integer;
  vPoly: TPolygon;
  vCircle: TCircle;
begin
  case Model.FigureKind of
    fkCircle:
    begin
      vCircle.X := ToFloat(AValue['X']);
      vCircle.Y := ToFloat(AValue['Y']);
      vCircle.Radius := ToFloat(AValue['Radius']);
      Model.SetData(vCircle);
    end;

    fkPoly:
    begin
      vPoly := Model.AsPoly;
      for i := 0 to High(vPoly) do
      begin
        vPoly[i].X := ToFloat(AValue['X' + IntToStr(i)]);
        vPoly[i].Y := ToFloat(AValue['Y' + IntToStr(i)]);
      end;
      Model.SetData(vPoly);
    end;
  end;
end;

procedure TItemShaperPresenter.SetPosition(const Value: TPoint);
begin

end;

procedure TItemShaperPresenter.ShowOptions;
begin
  inherited;
  if Assigned(FOnOptionsShow) then
    FOnOptionsShow(Self);

  if Assigned(FTableView) then
    FTableView.ShowParams(GetParams);
end;

procedure TItemShaperPresenter.TranslateFigure(const ATranslate: TPointF);
var
  vCircle: TCircle;
  vPoly: TPolygon;
begin
   case FItemShapeModel.FigureKind of
     fkCircle:
     begin
       vCircle := FItemShapeModel.AsCircle;
       Translate(vCircle, ATranslate);
       FItemShapeModel.SetData(vCircle);
     end;
     fkPoly:
     begin
       vPoly := FItemShapeModel.AsPoly;
       Translate(vPoly, ATranslate);
       FItemShapeModel.SetData(vPoly);
     end;
   end;
end;

end.
