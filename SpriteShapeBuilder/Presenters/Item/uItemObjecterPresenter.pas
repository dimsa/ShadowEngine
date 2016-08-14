unit uItemObjecterPresenter;

interface

uses
  System.Types, System.Generics.Collections, uIntersectorClasses, FMX.Graphics,
  System.UITypes, FMX.Types, System.SysUtils, {$I 'Utils\DelphiCompatability.inc'}
  System.Math, uItemBasePresenter, uItemShaperPresenter, uIItemView, uSSBModels,
  uITableView, uIItemPresenter;

type
  // To access protected Fields
  TItemShpPresenter = class(TItemShaperPresenter);

  TCaptureType = (ctNone, ctFigure, ctKeyPoint);

  TItemObjecterPresenter = class(TItemBasePresenter)
  private
    FItemObjectModel: TResourceModel;
    FParams: TDictionary<string, string>;
    FBmp: TBitmap; // Picture of object with Shapes
    FShapes: TList<TItemShpPresenter>;
    FIsShapeVisible: Boolean;
    FCaptureType: TCaptureType;
    FStartCapturePoint: TPointF;
    FLastTranslate: TPointF;
    FCapturedShape: TItemShpPresenter;
    FSelectedShape: TItemShpPresenter;
    function GetHeight: Integer;
    function GetPosition: TPoint;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetPosition(const Value: TPoint);
    procedure SetWidth(const Value: Integer);
    procedure OnModelUpdate(ASender: TObject);
    function Bitmap: TBitmap;
    procedure RepaintShapes;
    function GetParams: TDictionary<string,string>;
    procedure SetParams(const AValue: TDictionary<string, string>); override;
    function GetRect: TRectF; override;
    procedure SetRect(const Value: TRectF); override;
    procedure DoOptionsShow(ASender: TObject);
    procedure DoOptionsSave(ASender: TObject);
    procedure OnShapeAdded(ASender: TObject);
    procedure OnShapeRemoved(ASender: TObject);
    procedure OnFigureUpdated(ASender: TObject);
  protected
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Position: TPoint read GetPosition write SetPosition;
    property Rect: TRectF read GetRect write SetRect;
    property Model: TResourceModel read FItemObjectModel;
  public
    procedure ShowShapes;
    procedure HideShapes;
    procedure AddPoly;
    procedure AddCircle;
    procedure AddPoint;
    procedure DelPoint;
    procedure Repaint;
    procedure MouseDown; override;
    procedure MouseUp; override;
    procedure MouseMove; override;
    procedure DelShape;
    procedure ShowOptions; override;

    constructor Create(const AItemView: IItemView; const AItemObjectModel: TResourceModel);
    destructor Destroy; override;
  end;

implementation

uses
   uClasses;

{ TObjecterItemPresenter }

procedure TItemObjecterPresenter.AddCircle;
var
  vShape: TItemShpPresenter;
begin
  vShape := TItemShpPresenter.Create(FView, FItemObjectModel.OnAddShapeHandler);
  vShape.CreateCircle;
  RepaintShapes;
end;

procedure TItemObjecterPresenter.AddPoint;
begin
  if FSelectedShape <> nil then
  begin
    FSelectedShape.AddPoint;
    FSelectedShape := nil;
    RepaintShapes;
  end;
end;

procedure TItemObjecterPresenter.AddPoly;
var
  vShape: TItemShpPresenter;
begin
  vShape := TItemShpPresenter.Create(FView, FItemObjectModel.OnAddShapeHandler);
  vShape.CreatePoly;
  RepaintShapes;
end;

function TItemObjecterPresenter.Bitmap: TBitmap;
begin
  FBmp.Width := Width;
  FBmp.Height := Height;
  FBmp.ClearRect(RectF(0, 0, Width, Height));
  FBmp.Canvas.BeginScene;
  FBmp.Canvas.StrokeThickness := 1;
  FBmp.Canvas.Stroke.Color := TAlphaColorRec.Red;
  FBmp.Canvas.Fill.Color := $339999ff;
  FBmp.Canvas.FillRect(
      RectF(0, 0, Width, Height), 0, 0, [], 1, FMX.Types.TCornerType.ctBevel);
  FBmp.Canvas.DrawRect(
      RectF(0, 0, Width, Height), 0, 0, [], 1, FMX.Types.TCornerType.ctBevel);
  FBmp.Canvas.EndScene;

  Result := FBmp;
end;

constructor TItemObjecterPresenter.Create(const AItemView: IItemView; const AItemObjectModel: TResourceModel);
begin
  inherited Create(AItemView);

  FIsShapeVisible := True;
  FCaptureType := ctNone;
  FBmp := TBitmap.Create;
  FParams := TDictionary<string, string>.Create;
  FItemObjectModel := AItemObjectModel;
  FItemObjectModel.UpdateHander := OnModelUpdate;
  FItemObjectModel.ShapeAdded.Add(OnShapeAdded);
  FItemObjectModel.ShapeRemoved.Add(OnShapeRemoved);
  FShapes := TList<TItemShpPresenter>.Create;
end;

procedure TItemObjecterPresenter.DelPoint;
begin
  if FSelectedShape <> nil then
  begin
    FSelectedShape.DelPoint;
    FSelectedShape := nil;
    RepaintShapes;
  end;

end;

procedure TItemObjecterPresenter.DelShape;
begin
  inherited;

  if FSelectedShape <> nil then
  begin
    Model.RemoveShape(FSelectedShape.Model);
    FSelectedShape := nil;
  end;
end;

destructor TItemObjecterPresenter.Destroy;
var
  i: Integer;
begin
  for i := 0 to FShapes.Count - 1 do
    FShapes[i] := nil;

  FShapes.Free;
  FParams.Free;
  inherited;
end;

procedure TItemObjecterPresenter.DoOptionsSave(ASender: TObject);
begin
  FTableView := nil;
  OnModelUpdate(nil);
end;

procedure TItemObjecterPresenter.DoOptionsShow(ASender: TObject);
  var
  vItem: TItemShpPresenter;
  vTableView: ITableView;
begin
  vItem := TItemShpPresenter(ASender);
  vTableView := FTableView;

//  vTableView.Presenter := vItem;
  vTableView.SetOnTakeParams(vItem.OnOptionsSave);
  vItem.TableView := vTableView;
end;

function TItemObjecterPresenter.GetHeight: Integer;
begin
  Result := FItemObjectModel.Height;
end;

function TItemObjecterPresenter.GetParams: TDictionary<string,string>;
begin
  with FParams do
  begin
    Clear;
    Add('Name', Model.Name);
    Add('X', IntToStr(Model.Position.X));
    Add('Y', IntToStr(Model.Position.Y));
    Add('Width', IntToStr(Model.Width));
    Add('Height', IntToStr(Model.Height));
  end;
  Result := FParams;
end;

function TItemObjecterPresenter.GetPosition: TPoint;
begin
  Result := FItemObjectModel.Position;
end;

function TItemObjecterPresenter.GetRect: TRectF;
begin
  Result := System.Types.RectF(
    FItemObjectModel.Position.X,
    FItemObjectModel.Position.Y,
    FItemObjectModel.Position.X + FItemObjectModel.Width,
    FItemObjectModel.Position.Y + FItemObjectModel.Height);
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
  begin
    // Test on capturing KeyPoints of figures
    for i := 0 to FShapes.Count - 1 do
      if FShapes[i].KeyPointLocal(vPoint, vKeyPoint, 5, True) then
      begin
        FSelectedShape := TItemShpPresenter(FShapes[i]);
        FShapes[i].MouseDown;
        FCapturedShape := TItemShpPresenter(FShapes[i]);
        FCaptureType := ctKeyPoint;
        FStartCapturePoint := vPoint;

        RepaintShapes;
        Exit;
      end;

    // Test on capturing Figure
    for i := 0 to FShapes.Count - 1 do
      if TItemShpPresenter(FShapes[i]).IsPointIn(vPoint) then
      begin
        FSelectedShape := TItemShpPresenter(FShapes[i]);
        FShapes[i].MouseDown;
        FCapturedShape := TItemShpPresenter(FShapes[i]);
        FCaptureType := ctFigure;
        FStartCapturePoint := vPoint;

        RepaintShapes;
        Exit;
      end;

      FLastTranslate := TPointF.Zero;
      FStartCapturePoint := TPointF.Zero;
  end;

  FCapturedShape := nil;
  FSelectedShape := nil;
  FCaptureType := ctNone;
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

  case FCaptureType of
    ctNone:
    begin
      for i := 0 to FShapes.Count - 1 do
        if TItemShpPresenter(FShapes[i]).KeyPointLocal(vPoint, vKeyPoint, 5, True) then
        begin
          vNeedRepaint := True;
          FShapes[i].MouseMove;
        end;
    end;
    ctFigure:
    begin
      FCapturedShape.TranslateFigure(- FLastTranslate + vPoint - FStartCapturePoint );
      FLastTranslate := vPoint - FStartCapturePoint;
      vNeedRepaint := True;
    end;
    ctKeyPoint:
    begin
      FCapturedShape.ChangeLockedPoint(vPoint);
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
      if TItemShpPresenter(FShapes[i]).IsPointIn(FView.MousePos) then
        FShapes[i].MouseUp;

  FCapturedShape := nil;
  FCaptureType := ctNone;
  FLastTranslate := TPointF.Zero;

  RepaintShapes;

  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self)
end;

procedure TItemObjecterPresenter.OnFigureUpdated(ASender: TObject);
begin
  RepaintShapes;
end;

procedure TItemObjecterPresenter.OnModelUpdate(ASender: TObject);
var
  vShape: TItemShpPresenter;
  vITableView:ITableView;
  i: Integer;
  vi: IItemPresenter;
begin
  FView.Width := FItemObjectModel.Width;
  FView.Height:= FItemObjectModel.Height;
  FView.Left := FItemObjectModel.Position.X;
  FView.Top := FItemObjectModel.Position.Y;

  if ASender = nil then
    Exit;

  RepaintShapes;
end;

procedure TItemObjecterPresenter.OnShapeAdded(ASender: TObject);
var
  vShapePresenter: TItemShpPresenter;
begin
  vShapePresenter := TItemShpPresenter.Create(FView, TItemShapeModel(ASender));
  vShapePresenter.OnOptionsShow := OnOptionsShow;
  vShapePresenter.OnFigureUpdated := OnFigureUpdated;
  FShapes.Add(vShapePresenter);
  RepaintShapes;
end;

procedure TItemObjecterPresenter.OnShapeRemoved(ASender: TObject);
var
  i: Integer;
begin
  for i := 0 to FShapes.Count - 1 do
    if FShapes[i].Model = TItemShapeModel(ASender) then
      FShapes.Remove(FShapes[i]);
  RepaintShapes;
end;

procedure TItemObjecterPresenter.Repaint;
begin
  OnModelUpdate(nil);
  RepaintShapes;
end;

procedure TItemObjecterPresenter.RepaintShapes;
var
  i: Integer;
begin
  FBmp := Bitmap;
  for i := 0 to FShapes.Count - 1 do
  if TItemShpPresenter(FShapes[i]) = FSelectedShape then
    TItemShpPresenter(FShapes[i]).Repaint(FBmp, TAlphaColorRec.Aqua)
  else
    TItemShpPresenter(FShapes[i]).Repaint(FBmp);

  FView.AssignBitmap(FBmp);
end;

procedure TItemObjecterPresenter.SetHeight(const Value: Integer);
begin
  FItemObjectModel.Height := Value;
end;

procedure TItemObjecterPresenter.SetParams(
  const AValue: TDictionary<string, string>);
var
  vErr, vA: Integer;
begin
  with Model do
  begin
    Name := AValue['Name'];
    Position:= Point(ToInt(AValue['X']), ToInt(AValue['Y']));
    Width := ToInt(AValue['Width']);
    Height := ToInt(AValue['Height']);
  end;
end;

procedure TItemObjecterPresenter.SetPosition(const Value: TPoint);
begin
  FItemObjectModel.Position := Value;
end;

procedure TItemObjecterPresenter.SetRect(const Value: TRectF);
begin
  Position := Value.TopLeft.Round;
  Width := Round(Value.Width);
  Height := Round(Value.Height);
end;

procedure TItemObjecterPresenter.SetWidth(const Value: Integer);
begin
  FItemObjectModel.Width:= Value;
end;

procedure TItemObjecterPresenter.ShowOptions;
var
  vPoint: TPointF;
begin
  // For Objecter Mode
  if not FIsShapeVisible then
    if Assigned(FOnOptionsShow) then
      FOnOptionsShow(Self);

  // For Shaper Mode
  if FIsShapeVisible then
  begin
    // Test on capturing Figure
    vPoint := FView.MousePos - PointF(FView.Width / 2, FView.Height / 2);
    if Assigned(FSelectedShape) then
      if FSelectedShape.IsPointIn(vPoint) then
      begin
        FSelectedShape.ShowOptions;
        Exit;
      end;
  end;

  if Assigned(FTableView) then
    FTableView.ShowParams(GetParams);
end;

procedure TItemObjecterPresenter.ShowShapes;
begin
  FIsShapeVisible := True;
  RepaintShapes;
end;

end.
