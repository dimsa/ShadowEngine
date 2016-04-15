unit uImagerPresenter;

interface

uses
  System.Types, System.SysUtils, System.Generics.Collections, FMX.Objects,
  uBasePresenterIncapsulator,
  uIView, uSSBTypes, uItemBasePresenter, uClasses, uIItemPresenter,
  uIItemView, uItemImagerPresenter, uSSBModels, uMVPFrameWork,
  uEasyDevice;


type
  TImagerPresenterIncapsulator = class(TBasePresenterIncapsulator)
  strict private
    FCaptured: TItemImagerPresenter;
    procedure SetCaptured(const Value: TItemImagerPresenter);
    procedure SetElementStart(const ARect: TRect); override;
  protected
    property Captured: TItemImagerPresenter read FCaptured write SetCaptured;
  const
    CPrec = 3;
  end;

  TImagerPresenter = class(TImagerPresenterIncapsulator)
  private
    FSelected: TItemImagerPresenter;
    FCaptureMode: TCaptureMode; 
    FResizeType: TResizeType;
    FItems: TDictionary<TItemImagerPresenter, IItemView>;
    // Методы на клик
    procedure DoMouseDown(ASender: TObject);
    procedure DoMouseUp(ASender: TObject);
    procedure DoMouseMove(ASender: TObject);
    function ResizeType(const AItem: TItemImagerPresenter): TResizeType;
    function GetView: IMainView;
//    procedure JustifyPoints(vItem: TItemImagerPresenter; var vRect: TRectF);
    procedure JustifyPoints(AItem: TItemImagerPresenter);
    procedure JustifyAnchors(AItem: TItemImagerPresenter);

  public
    procedure AddImg; overload;
    procedure AddImg(const AModel: TItemImageModel); overload;
    procedure DelImg;
    procedure MouseMove;
    procedure MouseDown;
    procedure MouseUp;
    procedure Init;                        
    constructor Create(AView: IView; AModel: TSSBModel); override;
    destructor Destroy; override;
  end;

implementation

uses
  FMX.Platform, FMX.Platform.Win, FMX.Types, System.UITypes;

procedure TImagerPresenter.AddImg;
var
  vFileName: string;
  vViewItem: IItemView;
  vItemPresenter: TItemImagerPresenter;
  vModel: TItemImageModel;
  vImg: TImage;
begin
  vFileName := View.FilenameFromDlg;
  if vFileName <> '' then
  begin
    vImg := TImage.Create(nil);
    vImg.Bitmap.LoadFromFile(vFileName);

    // Creating View
    vViewItem := View.AddElement;

    // Creating Model
    vModel := Model.AddImageElement;

    // Creating Presenter
    vItemPresenter := TItemImagerPresenter.Create(vViewItem, vModel);//TItemPresenterProxy.Create(vViewItem, sPicture);
    vViewItem.Presenter := vItemPresenter;

    vModel.OriginalImage := vImg;
    vModel.Rect := Rect(0, 0, Round(vImg.Bitmap.Width), Round(vImg.Bitmap.Height)) ;

    vItemPresenter.OnMouseDown := DoMouseDown;
    vItemPresenter.OnMouseUp := DoMouseUp;
    vItemPresenter.OnMouseMove := DoMouseMove;

    FItems.Add(TItemImagerPresenter(vItemPresenter), vViewItem);
  end;
end;

procedure TImagerPresenter.AddImg(const AModel: TItemImageModel);
var
  vViewItem: IItemView;
  vItemPresenter: TItemImagerPresenter;
begin
//     Creating View
    vViewItem := View.AddElement;

    // Creating Presenter
    vItemPresenter := TItemImagerPresenter.Create(vViewItem, AModel);//TItemPresenterProxy.Create(vViewItem, sPicture);
    vViewItem.Presenter := vItemPresenter;

    AModel.OriginalImage := AModel.OriginalImage;

    vItemPresenter.OnMouseDown := DoMouseDown;
    vItemPresenter.OnMouseUp := DoMouseUp;
    vItemPresenter.OnMouseMove := DoMouseMove;



    FItems.Add(vItemPresenter, vViewItem);
end;

constructor TImagerPresenter.Create(AView: IView; AModel: TSSBModel);
begin
  inherited Create(AView, AModel);
  FItems := TDictionary<TItemImagerPresenter, IItemView>.Create;
//  FItems := TDictionary<IItemPresenter, IItemView>.Create;
  FCaptureMode := cmNone;
end;

procedure TImagerPresenter.DelImg;
begin

end;

destructor TImagerPresenter.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TImagerPresenter.JustifyAnchors(AItem: TItemImagerPresenter);
var
  vX: Integer;
  vY: Integer;
  vItem: TItemImagerPresenter;
//  vIItem: IInterface;
  vRect: TRectF;
begin
  vRect := RectF(AItem.Rect.TopLeft.X, AItem.Rect.TopLeft.Y, AItem.Rect.BottomRight.X, AItem.Rect.BottomRight.Y);
  for vItem in FItems.Keys do
  begin
//    vItem := TItemImagerPresenter(vIItem);
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

procedure TImagerPresenter.JustifyPoints(AItem: TItemImagerPresenter);
var
  vX: Integer;
  vY: Integer;
  vItem: TItemImagerPresenter;
//  vIItem: IInterface;
  vRect: TRectF;
begin
  vRect := RectF(AItem.Rect.TopLeft.X, AItem.Rect.TopLeft.Y, AItem.Rect.BottomRight.X, AItem.Rect.BottomRight.Y);
  for vItem in FItems.Keys do
  begin
   // vItem := TItemImagerPresenter(vIItem);
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
  if (ASender is TItemImagerPresenter) then
  begin
    FSelected := TItemImagerPresenter(ASender);
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

function TImagerPresenter.GetView: IMainView;
begin

end;

procedure TImagerPresenter.Init;
begin
  inherited;

end;

procedure TImagerPresenter.MouseDown;
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
  vItem: TItemImagerPresenter;
  vRect: TRectF;
  vW, vH: Single;
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

procedure TImagerPresenter.MouseUp;
begin
  Captured := nil;
  FCaptureMode := cmNone;
  FResizeType := rtNone;
  IsMouseDowned := False;
end;

function TImagerPresenter.ResizeType(
  const AItem: TItemImagerPresenter): TResizeType;
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
  const Value: TItemImagerPresenter);
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


