unit uImagerPresenter;

interface

uses
  System.Types, System.SysUtils, System.Generics.Collections, FMX.Objects,
  uIView, uItemPresenterProxy, uSSBTypes, uItemBasePresenter,
  uIItemView, uItemImagerPresenter, uSSBModels, uMVPFrameWork,
  uEasyDevice;


type
  TImagerPresenter = class(TPresenter)
  private
    FModel: TSSBModel;
    FSelected: TImagerItemPresenter;
    FCaptured: TImagerItemPresenter;
    FIsMouseDown: Boolean;
    FMouseStartPoint: TPointF;
    FMouseElementPoint: TPointF;
    FElementStartPosition: TPointF;
    FItems: TDictionary<TImagerItemPresenter, IItemView>;
    function GetView: IMainView;
    property View: IMainView read GetView;
    // Методы на клик
    procedure DoSelectItem(ASender: TObject);
    procedure DoCaptureItem(ASender: TObject);
    procedure DoUncaptureItem(ASender: TObject);
    procedure DoDeleteItem(ASender: TObject);
    procedure DoDragCapturedItem(ASender: TObject);
    procedure DoHoverItem(ASender: TObject);
  public
    procedure AddImg;
    procedure DelImg;
    procedure DragImg;
    procedure StartDragImg;
    procedure FinishDragImg;
    procedure MouseMove;
    procedure Init;
    constructor Create(AView: IView; AModel: TSSBModel);
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
    vItemPresenter.OnSelect := DoSelectItem;
    vItemPresenter.OnCapture:= DoCaptureItem;
    vItemPresenter.OnUnCapture:= DoUnCaptureItem;
    vItemPresenter.OnHover := DoHoverItem;

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
  FItems := TDictionary<TImagerItemPresenter, IItemView>.Create;
  FView := AView;
  FModel := AModel;
end;

procedure TImagerPresenter.DelImg;
begin

end;

destructor TImagerPresenter.Destroy;
begin
  FItems.Free;
  FModel.Free;
  inherited;
end;

procedure TImagerPresenter.DoCaptureItem(ASender: TObject);
begin
  if (ASender is TItemPresenterProxy) then
  begin
    FMouseStartPoint := uEasyDevice.MousePos;
    FCaptured := TImagerItemPresenter(ASender);
  end;
end;

procedure TImagerPresenter.DoDeleteItem(ASender: TObject);
begin

end;

procedure TImagerPresenter.DoDragCapturedItem(ASender: TObject);
begin
  if (ASender is TItemPresenterProxy) then
  begin
    FMouseStartPoint := uEasyDevice.MousePos;
    FCaptured := TImagerItemPresenter(ASender);
  end;
end;

procedure TImagerPresenter.DoHoverItem(ASender: TObject);
begin
  MouseMove;
end;

procedure TImagerPresenter.DoSelectItem(ASender: TObject);
begin
  if (ASender is TImagerItemPresenter) then
  begin
    FSelected := TImagerItemPresenter(ASender);
    View.SelectElement(FItems[FSelected]);
  end;
end;

procedure TImagerPresenter.DoUncaptureItem(ASender: TObject);
begin
  FCaptured := nil;
end;

procedure TImagerPresenter.DragImg;
begin

end;

procedure TImagerPresenter.FinishDragImg;
begin
  FCaptured := Nil;
end;

function TImagerPresenter.GetView: IMainView;
begin
  Result := IMainView(FView);
end;

procedure TImagerPresenter.Init;
begin
  inherited;

end;

procedure SetCursor(ACursor: TCursor);
var
  CS: IFMXCursorService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
  begin
    CS := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService;
  end;
  if Assigned(CS) then
  begin
    CS.SetCursor(ACursor);
    CS.SetCursor(ACursor);
  end;
end;

procedure TImagerPresenter.MouseMove;
var
  vItem: TImagerItemPresenter;
  vPoint: TPoint;
  vD: Integer;
begin
  vPoint := View.GetMousePos;

  if not Assigned(FSelected) then
    Exit;

  vD := 5;
  if (FSelected.Position.X - vD <= vPoint.X) and
     (FSelected.Position.X + vD >= vPoint.X) and
     (FSelected.Position.Y < vPoint.Y) and (FSelected.Position.Y + FSelected.Height > vPoint.Y) then
     begin
       View.ChangeCursor(crSizeWE);
       Exit;
     end;

  if (FSelected.Position.X + FSelected.Width - vD <= vPoint.X) and
     (FSelected.Position.X + FSelected.Width + vD >= vPoint.X) and
     (FSelected.Position.Y < vPoint.Y) and (FSelected.Position.Y + FSelected.Height > vPoint.Y) then
     begin
       View.ChangeCursor(crSizeWE);
       Exit;
     end;

  if (FSelected.Position.Y - vD <= vPoint.Y) and
     (FSelected.Position.Y + vD >= vPoint.Y) and
     (FSelected.Position.X < vPoint.X) and (FSelected.Position.X + FSelected.Width > vPoint.X) then
     begin
       View.ChangeCursor(crSizeNS);
       Exit;
     end;

  if (FSelected.Position.Y + FSelected.Height - vD <= vPoint.Y) and
     (FSelected.Position.Y + FSelected.Height + vD >= vPoint.Y) and
     (FSelected.Position.X < vPoint.X) and (FSelected.Position.X + FSelected.Width > vPoint.X) then
     begin
       View.ChangeCursor(crSizeNS);
       Exit;
     end;

   View.ChangeCursor(crArrow);
end;

{procedure TSSBImagerPresenter.SelImg;
var
  i: Integer;
begin
  for i := 0 to FModel.ImageCount - 1 do
    if FModel.Images[i].PointInObject(FView.MousePos.X, FView.MousePos.Y) then
    begin
      FSelected := FModel.Images[i];
      FView.SelectElement(FView.ElementUnderMouse);
      Exit;
    end;
    FSelected := nil;
end;  }

procedure TImagerPresenter.StartDragImg;
begin

end;

end.
