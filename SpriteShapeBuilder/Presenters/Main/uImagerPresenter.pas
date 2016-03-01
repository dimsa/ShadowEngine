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
    FResizing: TImagerItemPresenter;
    FIsMouseDown: Boolean;
    FMouseStartPoint: TPoint;
    FElementStartPoint: TPoint;    
    FMouseDowned: Boolean;

    FItems: TDictionary<TImagerItemPresenter, IItemView>;
    function GetView: IMainView;
    property View: IMainView read GetView;
    // Методы на клик
    procedure DoMouseDown(ASender: TObject);
    procedure DoMouseUp(ASender: TObject);
    procedure DoMouseMove(ASender: TObject);
  public
    procedure AddImg;
    procedure DelImg;
    procedure MouseMove;
    procedure MouseDown;
    procedure MouseUp;
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

procedure TImagerPresenter.DoMouseDown(ASender: TObject);
begin
  if (ASender is TImagerItemPresenter) then
  begin
    FSelected := TImagerItemPresenter(ASender);
    FCaptured := TImagerItemPresenter(ASender);
    FMouseDowned := True;
    FMouseStartPoint := View.GetMousePos;
    FElementStartPoint := FSelected.Position;
    View.SelectElement(FItems[FSelected]);
  end;
end;

procedure TImagerPresenter.DoMouseMove(ASender: TObject);
begin
  if FMouseDowned then
    if FCaptured <> nil then
      FCaptured.Position := FElementStartPoint - FMouseStartPoint + View.GetMousePos;
end;

procedure TImagerPresenter.DoMouseUp(ASender: TObject);
begin
  if FMouseDowned then
    if FCaptured <> nil then
      FCaptured.Position := FElementStartPoint - FMouseStartPoint + View.GetMousePos;
  FCaptured := nil;
  
  FMouseDowned := False;
end;

function TImagerPresenter.GetView: IMainView;
begin
  Result := IMainView(FView);
end;

procedure TImagerPresenter.Init;
begin
  inherited;

end;

procedure TImagerPresenter.MouseDown;
begin

end;

procedure TImagerPresenter.MouseMove;
var
  vItem: TImagerItemPresenter;
  vPoint: TPoint;
  vD: Integer;
begin
  if FMouseDowned then
    if FCaptured <> nil then
      FCaptured.Position := FElementStartPoint - FMouseStartPoint + View.GetMousePos;

  vPoint := View.GetMousePos;

  if not Assigned(FSelected) then
    Exit;

  vD := 5;
  if (FSelected.Position.X - vD <= vPoint.X) and
     (FSelected.Position.X + vD >= vPoint.X) and
     (FSelected.Position.Y < vPoint.Y) and (FSelected.Position.Y + FSelected.Height > vPoint.Y) then
     begin
       View.ChangeCursor(crSizeWE);
       FResizing := FSelected;
       Exit;
     end;

  if (FSelected.Position.X + FSelected.Width - vD <= vPoint.X) and
     (FSelected.Position.X + FSelected.Width + vD >= vPoint.X) and
     (FSelected.Position.Y < vPoint.Y) and (FSelected.Position.Y + FSelected.Height > vPoint.Y) then
     begin
       View.ChangeCursor(crSizeWE);
       FResizing := FSelected;
       Exit;
     end;

  if (FSelected.Position.Y - vD <= vPoint.Y) and
     (FSelected.Position.Y + vD >= vPoint.Y) and
     (FSelected.Position.X < vPoint.X) and (FSelected.Position.X + FSelected.Width > vPoint.X) then
     begin
       View.ChangeCursor(crSizeNS);
       FResizing := FSelected;
       Exit;
     end;

  if (FSelected.Position.Y + FSelected.Height - vD <= vPoint.Y) and
     (FSelected.Position.Y + FSelected.Height + vD >= vPoint.Y) and
     (FSelected.Position.X < vPoint.X) and (FSelected.Position.X + FSelected.Width > vPoint.X) then
     begin
       View.ChangeCursor(crSizeNS);
       FResizing := FSelected;
       Exit;
     end;

   View.ChangeCursor(crArrow);
end;

procedure TImagerPresenter.MouseUp;
begin
  FCaptured := nil;
  FMouseDowned := False;
end;

end.


