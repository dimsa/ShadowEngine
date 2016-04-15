unit uObjecterPresenter;

interface

uses
  System.Generics.Collections, FMX.Objects, System.Types, uClasses,
  uIView, uMVPFrameWork, uSSBModels, uSSBTypes,
  uIItemView, uItemObjecterPresenter,
  uBasePresenterIncapsulator;

type
  TObjecterPresenterIncapsulator = class(TBasePresenterIncapsulator)
  strict private
    FCaptured: TItemObjecterPresenter;
    procedure SetCaptured(const Value: TItemObjecterPresenter);
    procedure SetElementStart(const ARect: TRect); override;
  protected
    property Captured: TItemObjecterPresenter read FCaptured write SetCaptured;
  const
    CPrec = 3;
  end;

  TObjecterPresenter = class(TObjecterPresenterIncapsulator)
  private
    FSelected: TItemObjecterPresenter;
    FCaptureMode: TCaptureMode;
    FResizeType: TResizeType;
    FIsShapeVisible: Boolean;
    procedure JustifyPoints(AItem: TItemObjecterPresenter);
    procedure JustifyAnchors(AItem: TItemObjecterPresenter);
    function ResizeType(const AItem: TItemObjecterPresenter): TResizeType;
  protected
    FModel: TSSBModel;
    FItems: TDictionary<TItemObjecterPresenter, IItemView>;
    function GetView: IMainView;
    property View: IMainView read GetView;

    // Методы на клик
    procedure DoMouseUp(ASender: TObject);
    procedure DoMouseDown(ASender: TObject);
    procedure DoMouseMove(ASender: TObject);
  public
    constructor Create(AView: IView; AModel: TSSBModel);
    procedure ShowShapes;
    procedure HideShapes;
    procedure AddPoly;
    procedure AddCircle;
    procedure AddObj; overload;
    procedure AddObj(const AObject: TItemObjectModel); overload; // Need to move to protected
    procedure DelObj;
    procedure AddPoint;
    procedure DelPoint;
    procedure DelShape;
    procedure MouseMove;
    procedure MouseUp;
    procedure MouseDown;
  end;

implementation

uses
  FMX.Types, System.UITypes;

{ TObjecterPresenter }

procedure TObjecterPresenter.AddObj;
var
  vViewItem: IItemView;
  vItemPresenter: TItemObjecterPresenter;
  vModel: TItemObjectModel;
begin
    // Creating View
    vViewItem := View.AddElement;

    // Creating Model
    vModel := Model.AddElement;

    // Creating Presenter
    vItemPresenter := TItemObjecterPresenter.Create(vViewItem, vModel);

    vModel.Width := 50;
    vModel.Height:= 50;

    vViewItem.Presenter := vItemPresenter;
    vItemPresenter.OnMouseDown := DoMouseDown;
    vItemPresenter.OnMouseUp := DoMouseUp;
    vItemPresenter.OnMouseMove := DoMouseMove;

    FItems.Add(TItemObjecterPresenter(vItemPresenter), vViewItem);
    try
      vItemPresenter.Repaint;
    except
      View.RemoveElement(vViewItem);
    end;
end;

procedure TObjecterPresenter.AddObj(const AObject: TItemObjectModel);
var
  vViewItem: IItemView;
  vItemPresenter: TItemObjecterPresenter;
begin
    // Creating View
    vViewItem := View.AddElement;

    // Creating Presenter
    vItemPresenter := TItemObjecterPresenter.Create(vViewItem, AObject);

    vViewItem.Presenter := vItemPresenter;
    vItemPresenter.OnMouseDown := DoMouseDown;
    vItemPresenter.OnMouseUp := DoMouseUp;
    vItemPresenter.OnMouseMove := DoMouseMove;

    FItems.Add(TItemObjecterPresenter(vItemPresenter), vViewItem);
    try
      vItemPresenter.Repaint;
    except
      View.RemoveElement(vViewItem);
    end;
end;

procedure TObjecterPresenter.AddPoint;
begin
  if FSelected <> nil then
    FSelected.AddPoint;
end;

procedure TObjecterPresenter.AddPoly;
begin
  if FSelected <> nil then
    FSelected.AddPoly;
end;

procedure TObjecterPresenter.AddCircle;
begin
  if FSelected <> nil then
    FSelected.AddCircle;
end;

constructor TObjecterPresenter.Create(AView: IView; AModel: TSSBModel);
begin
  inherited Create(AView, AModel);
  FItems := TDictionary<TItemObjecterPresenter, IItemView>.Create;
  FView := AView;
end;

procedure TObjecterPresenter.DelObj;
var
  vView: IItemView;
begin
  if FSelected <> nil then
  begin
    Model.DelElement(FSelected.Model);
    vView := FItems[FSelected];
    View.RemoveElement(vView);
    FItems.Remove(FSelected);
    FSelected := nil;
  end;
end;

procedure TObjecterPresenter.DelPoint;
begin
  if FSelected <> nil then
    FSelected.DelPoint;
end;

procedure TObjecterPresenter.DelShape;
begin
  if FSelected <> nil then
    FSelected.Delete;
end;

procedure TObjecterPresenter.DoMouseDown(ASender: TObject);
begin
  if (ASender is TItemObjecterPresenter) then
  begin
    FSelected := TItemObjecterPresenter(ASender);
    View.SelectElement(FItems[FSelected]);
    MouseDown;
  end;
end;

procedure TObjecterPresenter.DoMouseMove(ASender: TObject);
begin
  MouseMove;
end;

procedure TObjecterPresenter.DoMouseUp(ASender: TObject);
begin
  MouseUp;
end;

function TObjecterPresenter.GetView: IMainView;
begin
  Result := IMainView(FView);
end;

procedure TObjecterPresenter.HideShapes;
var
  vItem: TItemObjecterPresenter;
begin
  FIsShapeVisible := False;

  for vItem in FItems.Keys do
    vItem.HideShapes;
end;

procedure TObjecterPresenter.JustifyAnchors(AItem: TItemObjecterPresenter);
var
  vX: Integer;
  vY: Integer;
  vItem: TItemObjecterPresenter;
  vRect: TRectF;
begin
  vRect := RectF(AItem.Rect.TopLeft.X, AItem.Rect.TopLeft.Y, AItem.Rect.BottomRight.X, AItem.Rect.BottomRight.Y);
  for vItem in FItems.Keys do
  begin
    if vItem <> AItem then
    begin
      with vRect do
      begin
        for vX := 0 to 3 do
          for vY := 0 to 3 do
          begin
            if (Points[vX].X <= vItem.Rect.Points[vY].X + CPrec) and (Points[vX].X >= vItem.Rect.Points[vY].X - CPrec) then
              Anchors[vX] := PointF(vItem.Rect.Points[vY].X, Points[vX].Y);
            if (Points[vX].Y <= vItem.Rect.Points[vY].Y + CPrec) and (Points[vX].Y >= vItem.Rect.Points[vY].Y - CPrec) then
              Anchors[vX] := PointF(Points[vX].X, vItem.Rect.Points[vY].Y);
          end;
      end;
      AItem.Rect := vRect;
    end;
  end;
end;

procedure TObjecterPresenter.JustifyPoints(AItem: TItemObjecterPresenter);
begin

end;

procedure TObjecterPresenter.MouseDown;
begin
  IsMouseDowned := True;
  if FSelected <> nil then
  begin
    if ResizeType(FSelected) = TResizeType.rtNone then
    begin
      Captured := nil;
      FSelected := nil;
      Exit;
    end;

    Captured := FSelected;
    if ResizeType(FSelected) = TResizeType.rtCenter then
    begin
      FCaptureMode := TCaptureMode.cmMove;
      FResizeType := TResizeType.rtCenter;
    end
    else begin
      FCaptureMode := TCaptureMode.cmResize;
      FResizeType := ResizeType(Captured);
    end;
  end;
end;

procedure TObjecterPresenter.MouseMove;
begin
  if (FSelected <> nil) then
      ResizeType(FSelected);

  if IsMouseDowned then
    if Captured <> nil then
    begin
      if FCaptureMode = TCaptureMode.cmMove then
      begin
        Captured.Position := ElementStart.TopLeft - MouseStart + View.GetMousePos;
        JustifyAnchors(Captured);
      end;
      if FCaptureMode = TCaptureMode.cmResize then
      begin
        case FResizeType of
          TResizeType.rtEW: begin
            Captured.Width := ElementStart.Width - MouseStart.X + View.GetMousePos.X;
          end;
          TResizeType.rtWE: begin
            Captured.Position := Point(ElementStart.Left - MouseStart.X + View.GetMousePos.X, Captured.Position.Y);
            Captured.Width := ElementStart.Width + MouseStart.X - View.GetMousePos.X;
          end;
          TResizeType.rtSN: begin
            Captured.Height:= ElementStart.Height - MouseStart.Y + View.GetMousePos.Y;
          end;
          TResizeType.rtNS: begin
            Captured.Position := Point(Captured.Position.X, ElementStart.Top - MouseStart.Y + View.GetMousePos.Y);
            Captured.Height := ElementStart.Height + MouseStart.Y - View.GetMousePos.Y;
          end;
        end;
        JustifyPoints(Captured);
      end;
    end;
end;

procedure TObjecterPresenter.MouseUp;
begin
  Captured := nil;
  FCaptureMode := cmNone;
  FResizeType := rtNone;
  IsMouseDowned := False;
end;

function TObjecterPresenter.ResizeType(
  const AItem: TItemObjecterPresenter): TResizeType;
var
  vPoint: TPoint;
  vD: Integer;
begin
  vPoint := View.GetMousePos;

  vD := 5;
  if (AItem.Position.X - vD <= vPoint.X) and
     (AItem.Position.X + vD >= vPoint.X) and
     (AItem.Position.Y < vPoint.Y) and (AItem.Position.Y + AItem.Height > vPoint.Y) then
     begin
         View.ChangeCursor(crSizeWE);
       Exit(TResizeType.rtWE);
     end;

  if (AItem.Position.X + AItem.Width - vD <= vPoint.X) and
     (AItem.Position.X + AItem.Width + vD >= vPoint.X) and
     (AItem.Position.Y < vPoint.Y) and (AItem.Position.Y + AItem.Height > vPoint.Y) then
     begin
         View.ChangeCursor(crSizeWE);
       Exit(TResizeType.rtEW);
     end;

  if (AItem.Position.Y - vD <= vPoint.Y) and
     (AItem.Position.Y + vD >= vPoint.Y) and
     (AItem.Position.X < vPoint.X) and (AItem.Position.X + AItem.Width > vPoint.X) then
     begin
         View.ChangeCursor(crSizeNS);
       Exit(TResizeType.rtNS);
     end;

  if (AItem.Position.Y + AItem.Height - vD <= vPoint.Y) and
     (AItem.Position.Y + AItem.Height + vD >= vPoint.Y) and
     (AItem.Position.X < vPoint.X) and (AItem.Position.X + AItem.Width > vPoint.X) then
     begin
         View.ChangeCursor(crSizeNS);
       Exit(TResizeType.rtSN);
     end;


   View.ChangeCursor(crArrow);
   if (AItem.Position.X <= vPoint.X) and (AItem.Position.Y <= vPoint.Y) and
      (AItem.Position.X + AItem.Width >= vPoint.X) and (AItem.Position.Y + AItem.Height >= vPoint.Y) then
        Exit(TResizeType.rtCenter);

   Exit(TResizeType.rtNone)
end;

procedure TObjecterPresenter.ShowShapes;
var
  vItem: TItemObjecterPresenter;
begin
  FIsShapeVisible := True;

  for vItem in FItems.Keys do
    vItem.ShowShapes;
end;

{ TObjecterPresenterIncapsulator }

procedure TObjecterPresenterIncapsulator.SetCaptured(
  const Value: TItemObjecterPresenter);
var
  vRect: TRect;
begin
  FCaptured := Value;
  if Value <> nil then
  begin
    vRect.TopLeft := Value.Position;
    vRect.Width := Value.Width;
    vRect.Height := Value.Height;
  end else
    vRect := TRect.Empty;

  SetElementStart(vRect);
end;

procedure TObjecterPresenterIncapsulator.SetElementStart(const ARect: TRect);
begin
  inherited;

end;

end.
