unit uImagerPresenter;

interface

uses
  System.Types, System.SysUtils, System.Generics.Collections, FMX.Objects,
  uBasePresenterIncapsulator,
  uIWorkSpaceView, uSSBTypes, uItemBasePresenter, uClasses, uIItemPresenter,
  uIItemView, uItemImagerPresenter, uMainModel, uSSBModels, uMVPFrameWork,
  uITableView, uEasyDevice;


type
  // To access protected Fields
  TItemImgPresenter = class(TItemImagerPresenter);

  TImagerPresenterIncapsulator = class(TBasePresenterIncapsulator)
  strict private
    FCaptured: TItemImgPresenter;
    procedure SetCaptured(const Value: TItemImgPresenter);
    procedure SetElementStart(const ARect: TRect); override;
  protected
    property Captured: TItemImgPresenter read FCaptured write SetCaptured;
  const
    CPrec = 3;
  end;

  TImagerPresenter = class(TImagerPresenterIncapsulator)
  private
    FSelected: TItemImgPresenter;
    FCaptureMode: TCaptureMode; 
    FResizeType: TResizeType;
    FItems: TDictionary<TItemImgPresenter, IItemView>;
    // Методы на клик
    procedure DoMouseDown(ASender: TObject);
    procedure DoMouseUp(ASender: TObject);
    procedure DoMouseMove(ASender: TObject);
    procedure DoOptionsShow(ASender: TObject);

    function ResizeType(const AItem: TItemImgPresenter): TResizeType;
    function GetView: IWorkSpaceView;
    procedure JustifyPoints(AItem: TItemImgPresenter);
    procedure JustifyAnchors(AItem: TItemImgPresenter);
  public
    procedure AddImg; overload;
    procedure AddImg(const AModel: TItemImageModel); overload;
    procedure DelImg;
    procedure CloneImg;
    procedure MouseMove;
    procedure MouseDown;
    procedure MouseUp;
    procedure DisableItems;
    procedure EnableItems;
    constructor Create(AView: IWorkSpaceView; AModel: TSSBModel; AStatus: TDelegate<TSSBStatus>); override;
    destructor Destroy; override;
  end;

implementation

uses
  FMX.Platform, FMX.Platform.Win, FMX.Types, System.UITypes;

procedure TImagerPresenter.AddImg;
var
  vFileName: string;
  vModel: TItemImageModel;
  vImg: TImage;
begin
  if View.FilenameFromDlg(vFileName) then
  begin
    vImg := TImage.Create(nil);
    vImg.Bitmap.LoadFromFile(vFileName);

    // Creating Model
    vModel := Model.AddImageElement;
    AddImg(vModel);
    vModel.OriginalImage := vImg;
    vModel.Rect := Rect(0, 0, Round(vImg.Bitmap.Width), Round(vImg.Bitmap.Height)) ;
  end;
end;

procedure TImagerPresenter.AddImg(const AModel: TItemImageModel);
var
  vViewItem: IItemView;
  vItemPresenter: TItemImgPresenter;
begin
// Creating View
  vViewItem := View.AddElement;

  // Creating Presenter
  vItemPresenter := TItemImgPresenter.Create(vViewItem, AModel);//TItemPresenterProxy.Create(vViewItem, sPicture);
  vViewItem.Presenter := vItemPresenter;

  vItemPresenter.OnMouseDown := DoMouseDown;
  vItemPresenter.OnMouseUp := DoMouseUp;
  vItemPresenter.OnMouseMove := DoMouseMove;
  vItemPresenter.OnOptionsShow := DoOptionsShow;

  FItems.Add(vItemPresenter, vViewItem);
end;

procedure TImagerPresenter.CloneImg;
var
  vFileName: string;
  vModel, vOldModel: TItemImageModel;
  vImg: TImage;
begin
  if FSelected <> nil then
  begin
    vOldModel := FSelected.Model;
    // Creating Model
    vModel := Model.AddImageElement;
    vModel.Rect := vOldModel.Rect;
    vModel.Width := vOldModel.Width;
    vModel.Height := vOldModel.Height;

    AddImg(vModel);
    vModel.OriginalImage := TImage.Create(nil);
    vModel.OriginalImage.Bitmap.Assign(vOldModel.OriginalImage.Bitmap);
  end;
end;

constructor TImagerPresenter.Create(AView: IWorkSpaceView; AModel: TSSBModel; AStatus: TDelegate<TSSBStatus>);
begin
  inherited Create(AView, AModel, AStatus);
  FItems := TDictionary<TItemImgPresenter, IItemView>.Create;
  FCaptureMode := cmNone;
end;

procedure TImagerPresenter.DelImg;
var
  vView: IItemView;
begin
  if FSelected <> nil then
  begin
    Model.RemoveImage(FSelected.Model);
    vView := FItems[FSelected];
    View.RemoveElement(vView);
    FItems.Remove(FSelected);
    FSelected := nil;
  end;
end;

destructor TImagerPresenter.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TImagerPresenter.DisableItems;
var
  i: TItemImgPresenter;
begin
  for i in FItems.Keys do
    i.Disable;
end;

procedure TImagerPresenter.JustifyAnchors(AItem: TItemImgPresenter);
var
  vX: Integer;
  vY: Integer;
  vItem: TItemImgPresenter;
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

procedure TImagerPresenter.JustifyPoints(AItem: TItemImgPresenter);
var
  vX: Integer;
  vY: Integer;
  vItem: TItemImgPresenter;
//  vIItem: IInterface;
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
              Points[vX] := PointF(vItem.Rect.Points[vY].X, Points[vX].Y);
            if (Points[vX].Y <= vItem.Rect.Points[vY].Y + CPrec) and (Points[vX].Y >= vItem.Rect.Points[vY].Y - CPrec) then
              Points[vX] := PointF(Points[vX].X, vItem.Rect.Points[vY].Y);
          end;
      end;
      AItem.Rect := vRect;
    end;
  end;
end;

procedure TImagerPresenter.DoMouseDown(ASender: TObject);
begin
  if (ASender is TItemImgPresenter) then
  begin
    FSelected := TItemImgPresenter(ASender);
    View.SelectElement(FItems[FSelected]);
    MouseDown;
  end;
end;

procedure TImagerPresenter.DoMouseMove(ASender: TObject);
begin
  MouseMove;
end;

procedure TImagerPresenter.DoMouseUp(ASender: TObject);
begin
  MouseUp;
end;

procedure TImagerPresenter.DoOptionsShow(ASender: TObject);
var
  vItem: TItemImgPresenter;
  vTableView: ITableView;
begin
  if Status <> sPicture then
    Exit;

  vItem := TItemImgPresenter(ASender);
  vTableView := View.AddTableView;

  vTableView.SetOnTakeParams(vItem.OnOptionsSave);
  vItem.TableView := vTableView;
end;

procedure TImagerPresenter.EnableItems;
var
  i: TItemImgPresenter;
begin
  for i in FItems.Keys do
    i.Enable;
end;

function TImagerPresenter.GetView: IWorkSpaceView;
begin

end;

procedure TImagerPresenter.MouseDown;
begin
  if Status <> sPicture then
    Exit;

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
      FResizeType := rtCenter;
    end
    else begin
      FCaptureMode := TCaptureMode.cmResize ;
      FResizeType := ResizeType(Captured);
    end;
  end;
end;

procedure TImagerPresenter.MouseMove;
var
  vItem: TItemImgPresenter;
  vRect: TRectF;
  vW, vH: Single;
begin
  if Status <> sPicture then
    Exit;

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

procedure TImagerPresenter.MouseUp;
begin
  if Status <> sPicture then
    Exit;

  Captured := nil;
  FCaptureMode := cmNone;
  FResizeType := rtNone;
  IsMouseDowned := False;
end;

function TImagerPresenter.ResizeType(
  const AItem: TItemImgPresenter): TResizeType;
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

procedure TImagerPresenterIncapsulator.SetCaptured(
  const Value: TItemImgPresenter);
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

procedure TImagerPresenterIncapsulator.SetElementStart(const ARect: TRect);
begin
  inherited;

end;

end.


