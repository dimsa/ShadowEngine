unit uObjecterPresenter;

interface

uses
  System.Generics.Collections, FMX.Objects, System.Types,
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
  end;

  TObjecterPresenter = class(TObjecterPresenterIncapsulator)
  private
    FSelected: TItemObjecterPresenter;
    FCaptured: TItemObjecterPresenter;
    FCaptureMode: TCaptureMode;
    FIsShapeVisible: Boolean;
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
    procedure AddObj;
    procedure DelObj;
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
  vImg: TImage;
  vViewItem: IItemView;
  vItemPresenter: TItemObjecterPresenter;
  vModel: TItemObjectModel;
//  vItemPresenter: TItemPresenterProxy;
begin
    vImg := TImage.Create(nil);
    vImg.Width := 50;
    vImg.Height := 50;
    vImg.Bitmap.Width := 50;
    vImg.Bitmap.Height := 50;
    vImg.Bitmap.Canvas.BeginScene();
    vImg.Bitmap.Canvas.StrokeThickness := 5;
    vImg.Bitmap.Canvas.Stroke.Color := TAlphaColorRec.Red;
    vImg.Bitmap.Canvas.Fill.Color := TAlphaColorRec.Blue;
    vImg.Bitmap.Canvas.FillRect(
    RectF(0, 0, vImg.Width, vImg.Height), 0, 0, [], 1, FMX.Types.TCornerType.ctBevel);
    vImg.Bitmap.Canvas.EndScene;

    // Creating View
    vViewItem := View.AddElement;
    vViewItem.AssignBitmap(vImg.Bitmap);

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
      vViewItem.AssignBitmap(vImg.Bitmap);
    except
      vImg.Free;
      View.RemoveElement(vViewItem);
    end;
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
begin

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
      FCaptureMode := TCaptureMode.cmMove
    else
      FCaptureMode := TCaptureMode.cmResize
  end;
end;

procedure TObjecterPresenter.MouseMove;
var
  vItem: TItemObjecterPresenter;
  vPoint: TPoint;
  vD: Integer;
  vTmp: Integer;
begin
  if (FSelected <> nil) then
      ResizeType(FSelected);

  if IsMouseDowned then
    if Captured <> nil then
    begin
      if FCaptureMode = TCaptureMode.cmMove then
        Captured.Position := ElementStart.TopLeft - MouseStart + View.GetMousePos;
      if FCaptureMode = TCaptureMode.cmResize then
      begin
        case ResizeType(Captured) of
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

      end;
    end;
end;

procedure TObjecterPresenter.MouseUp;
begin
  Captured := nil;
  FCaptureMode := cmNone;
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
