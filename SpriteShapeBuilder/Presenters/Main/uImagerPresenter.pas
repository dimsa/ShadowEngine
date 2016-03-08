unit uImagerPresenter;

interface

uses
  System.Types, System.SysUtils, System.Generics.Collections, FMX.Objects,
  uIView, uItemPresenterProxy, uSSBTypes, uItemBasePresenter,
  uIItemView, uItemImagerPresenter, uSSBModels, uMVPFrameWork,
  uEasyDevice;


type
  TImagerPresenterIncapsulator = class(TPresenter)
  strict private
    FCaptured: TImagerItemPresenter;
    FModel: TSSBModel;
    FElementStart: TRect;
    FMouseStartPoint: TPoint;
    FIsMouseDowned: Boolean;
    function GetView: IMainView;    
    procedure SetCaptured(const Value: TImagerItemPresenter);
    procedure SetIsMouseDowned(const Value: Boolean);    
  protected
    property View: IMainView read GetView;  
    property ElementStart: TRect read FElementStart;
    property MouseStart: TPoint read FMouseStartPoint;
    property IsMouseDowned: Boolean read FIsMouseDowned write SetIsMouseDowned;
    property Captured: TImagerItemPresenter read FCaptured write SetCaptured;

    constructor Create(AView: IView; AModel: TSSBModel); virtual;    
    destructor Destroy; override;
  end;

  TImagerPresenter = class(TImagerPresenterIncapsulator)
  private
    FSelected: TImagerItemPresenter;
    FCaptureMode: TCaptureMode; 

    FItems: TDictionary<TImagerItemPresenter, IItemView>;  
    // Методы на клик
    procedure DoMouseDown(ASender: TObject);
    procedure DoMouseUp(ASender: TObject);
    procedure DoMouseMove(ASender: TObject);
    function ResizeType(const AItem: TImagerItemPresenter): TResizeType;
    function GetView: IMainView;
  public
    procedure AddImg;
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
  vItemPresenter: TItemPresenterProxy;
  vImg: TImage;
begin
  vFileName := View.FilenameFromDlg;
  if vFileName <> '' then
  begin
    vImg := TImage.Create(nil);
    vImg.Bitmap.LoadFromFile(vFileName);

    // Creating View
    vViewItem := View.AddElement;
    vViewItem.Left := 0;
    vViewItem.Top := 0;
    vViewItem.Width := Round(vImg.Width);
    vViewItem.Height:= Round(vImg.Height);

    // Creating Presenter
    vItemPresenter := TItemPresenterProxy.Create(vViewItem, sPicture);
    vViewItem.Presenter := vItemPresenter;

    vItemPresenter.OnMouseDown := DoMouseDown;
    vItemPresenter.OnMouseUp := DoMouseUp;
    vItemPresenter.OnMouseMove := DoMouseMove;

    FItems.Add(TImagerItemPresenter(vItemPresenter.Instance), vViewItem);
    try
      vViewItem.AssignBitmap(vImg.Bitmap);
    except
      vImg.Free;
      View.RemoveElement(vViewItem);
    end;
  end;
end;

constructor TImagerPresenter.Create(AView: IView; AModel: TSSBModel);
begin
  inherited;
  FItems := TDictionary<TImagerItemPresenter, IItemView>.Create;
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

procedure TImagerPresenter.DoMouseDown(ASender: TObject);
begin
  if (ASender is TImagerItemPresenter) then
  begin
    FSelected := TImagerItemPresenter(ASender);
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
      FCaptureMode := TCaptureMode.cmMove
    else
      FCaptureMode := TCaptureMode.cmResize
  end;
end;

procedure TImagerPresenter.MouseMove;
var
  vItem: TImagerItemPresenter;
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

procedure TImagerPresenter.MouseUp;
begin
  Captured := nil;
  FCaptureMode := cmNone;
  IsMouseDowned := False;
end;

function TImagerPresenter.ResizeType(
  const AItem: TImagerItemPresenter): TResizeType;
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

{ TImagerPresenterIncapsulator }

constructor TImagerPresenterIncapsulator.Create(AView: IView;
  AModel: TSSBModel);
begin
  FView := AView;
  FModel := AModel;
end;

destructor TImagerPresenterIncapsulator.Destroy;
begin
  FModel.Free;
  inherited;
end;

function TImagerPresenterIncapsulator.GetView: IMainView;
begin
  Result := IMainView(FView);
end;

procedure TImagerPresenterIncapsulator.SetCaptured(
  const Value: TImagerItemPresenter);
begin
  FCaptured := Value;
  if Value <> nil then
  begin
    FElementStart.TopLeft := Value.Position;
    FElementStart.Width := Value.Width;
    FElementStart.Height := Value.Height;
  end else
    FElementStart := TRect.Empty;
end;

procedure TImagerPresenterIncapsulator.SetIsMouseDowned(const Value: Boolean);
begin
  FIsMouseDowned := Value;
  FMouseStartPoint := View.GetMousePos;
end;

end.


