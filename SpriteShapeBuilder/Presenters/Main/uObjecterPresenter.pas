unit uObjecterPresenter;

interface

uses
  System.Generics.Collections, FMX.Objects, System.Types, uClasses,
  uIWorkSpaceView, uMVPFrameWork, uSSBModels, uMainModel, uSSBTypes,
  uIItemView, uItemObjecterPresenter, uNamedTableView,
  uBasePresenterIncapsulator;

type
  // To access protected Fields
  TItemObjPresenter = class(TItemObjecterPresenter);

  TObjecterPresenterIncapsulator = class(TBasePresenterIncapsulator)
  strict private
    FCaptured: TItemObjPresenter;
    procedure SetCaptured(const Value: TItemObjPresenter);
    procedure SetElementStart(const ARect: TRect); override;
  protected
    property Captured: TItemObjPresenter read FCaptured write SetCaptured;
  const
    CPrec = 3;
  end;

  TObjecterPresenter = class(TObjecterPresenterIncapsulator)
  private
    FSelected: TItemObjPresenter;
    FCaptureMode: TCaptureMode;
    FResizeType: TResizeType;
    FIsShapeVisible: Boolean;
    procedure JustifyPoints(AItem: TItemObjPresenter);
    procedure JustifyAnchors(AItem: TItemObjPresenter);
    function ResizeType(const AItem: TItemObjPresenter): TResizeType;
  protected
    FModel: TSSBModel;
    FItems: TDictionary<TItemObjPresenter, IItemView>;
    function GetView: IWorkSpaceView;
    property View: IWorkSpaceView read GetView;

    // Методы на клик
    procedure DoMouseUp(ASender: TObject);
    procedure DoMouseDown(ASender: TObject);
    procedure DoMouseMove(ASender: TObject);
    procedure DoOptionsShow(ASender: TObject);
  public
    constructor Create(AView: IWorkSpaceView; AModel: TSSBModel);
    procedure ShowShapes;
    procedure HideShapes;
    procedure AddPoly;
    procedure AddCircle;
    procedure AddObj; overload;
    procedure AddObj(const AObject: TResourceModel); overload; // Need to move to protected
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
  vItemPresenter: TItemObjPresenter;
  vTableView: TTableView;
  vModel: TResourceModel;
begin
    // Creating View
    vViewItem := View.AddElement;
    vTableView := TTableView.Create;

    // Creating Model
    vModel := Model.AddResource;

    // Creating Presenter
    vItemPresenter := TItemObjPresenter.Create(vViewItem, vTableView, vModel);

    vModel.Width := 50;
    vModel.Height:= 50;

    vViewItem.Presenter := vItemPresenter;
    vTableView.Presenter := vItemPresenter;

    vItemPresenter.OnMouseDown := DoMouseDown;
    vItemPresenter.OnMouseUp := DoMouseUp;
    vItemPresenter.OnMouseMove := DoMouseMove;
    vItemPresenter.OnOptionsShow := DoOptionsShow;

    FItems.Add(TItemObjPresenter(vItemPresenter), vViewItem);
    try
      vItemPresenter.Repaint;
    except
      View.RemoveElement(vViewItem);
    end;
end;

procedure TObjecterPresenter.AddObj(const AObject: TResourceModel);
var
  vViewItem: IItemView;
  vItemPresenter: TItemObjPresenter;
  vTableView: TTableView;
begin
    // Creating View
    vViewItem := View.AddElement;
    vTableView := TTableView.Create;

    // Creating Presenter
    vItemPresenter := TItemObjPresenter.Create(vViewItem, vTableView, AObject);

    vViewItem.Presenter := vItemPresenter;
    vTableView.Presenter := vItemPresenter;

    vItemPresenter.OnMouseDown := DoMouseDown;
    vItemPresenter.OnMouseUp := DoMouseUp;
    vItemPresenter.OnMouseMove := DoMouseMove;
    vItemPresenter.OnOptionsShow := DoOptionsShow;

    FItems.Add(TItemObjPresenter(vItemPresenter), vViewItem);
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

constructor TObjecterPresenter.Create(AView: IWorkSpaceView; AModel: TSSBModel);
begin
  inherited Create(AView, AModel);
  FItems := TDictionary<TItemObjPresenter, IItemView>.Create;
  FView := AView;
end;

procedure TObjecterPresenter.DelObj;
var
  vView: IItemView;
begin
  if FSelected <> nil then
  begin
    Model.RemoveResource(FSelected.Model);
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
  if (ASender is TItemObjPresenter) then
  begin
    FSelected := TItemObjPresenter(ASender);
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

procedure TObjecterPresenter.DoOptionsShow(ASender: TObject);
var
  vItem: TItemObjPresenter;
begin
  vItem := TItemObjPresenter(ASender);
//  View.ShowParams(vItem.Params);
end;

function TObjecterPresenter.GetView: IWorkSpaceView;
begin
  Result := IWorkSpaceView(FView);
end;

procedure TObjecterPresenter.HideShapes;
var
  vItem: TItemObjPresenter;
begin
  FIsShapeVisible := False;

  for vItem in FItems.Keys do
    vItem.HideShapes;
end;

procedure TObjecterPresenter.JustifyAnchors(AItem: TItemObjPresenter);
var
  vX: Integer;
  vY: Integer;
  vItem: TItemObjPresenter;
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

procedure TObjecterPresenter.JustifyPoints(AItem: TItemObjPresenter);
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
  const AItem: TItemObjPresenter): TResizeType;
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
  vItem: TItemObjPresenter;
begin
  FIsShapeVisible := True;

  for vItem in FItems.Keys do
    vItem.ShowShapes;
end;

{ TObjecterPresenterIncapsulator }

procedure TObjecterPresenterIncapsulator.SetCaptured(
  const Value: TItemObjPresenter);
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
