unit uMainPresenter;

interface

uses
  System.Types, System.Generics.Collections, FMX.Objects,
  uIView, uItemPresenterProxy,
  uIItemView, uImagerItemPresenter, uSSBModels, uMVPFrameWork;


type
  TMainPresenter = class(TPresenter)
  private
    FSelected: TItemPresenterProxy;
    FIsMouseDown: Boolean;
    FMouseStartPoint: TPointF;
    FMouseElementPoint: TPointF;
    FElementStartPosition: TPointF;
    FItems: TDictionary<TItemPresenterProxy, IItemView>;
    function GetView: IMainView;
    // Методы на клик
    procedure DoSelectItem(ASender: TObject);
    procedure DoDelImage(ASender: TObject);
  protected
    FModel: TSSBModel;
    property View: IMainView read GetView;
  public
    procedure AddImg;
    //procedure SelImg;
    procedure DelImg;
    procedure DragImg;
    procedure StartDragImg;
    procedure FinishDragImg;
    procedure Init;
    constructor Create(AView: IView; AModel: TSSBModel);
    destructor Destroy; override;
  end;

implementation

procedure TMainPresenter.AddImg;
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
    vItemPresenter := TItemPresenterProxy.Create(vViewItem);
    vViewItem.Presenter := vItemPresenter;
    vViewItem.Presenter.OnSelect := DoSelectItem;

    FItems.Add(vItemPresenter, vViewItem);
    try
      vViewItem.AssignBitmap(vImg.Bitmap);
    except
      vImg.Free;
      View.RemoveElement(vViewItem);
    end;
  end;
end;

constructor TMainPresenter.Create(AView: IView; AModel: TSSBModel);
begin
  FItems := TDictionary<TItemPresenterProxy, IItemView>.Create;
  FView := AView;
  FModel := AModel;
end;

procedure TMainPresenter.DelImg;
begin

end;

destructor TMainPresenter.Destroy;
begin
  FItems.Free;
  FModel.Free;
  inherited;
end;

procedure TMainPresenter.DoDelImage(ASender: TObject);
begin

end;

procedure TMainPresenter.DoSelectItem(ASender: TObject);
begin
  if (ASender is TItemPresenterProxy) then
  begin
    FSelected := TItemPresenterProxy(ASender);
    View.SelectElement(FItems[FSelected]);
  end;
end;

procedure TMainPresenter.DragImg;
begin

end;

procedure TMainPresenter.FinishDragImg;
begin

end;

function TMainPresenter.GetView: IMainView;
begin
  Result := IMainView(FView);
end;

procedure TMainPresenter.Init;
begin
  inherited;

{  FView.ChangeImageMouseDownHandler(DoMouseDown);
  FView.ChangeImageMouseUpHandler(DoMouseUp);
  FView.ChangeImageMouseMoveHandler(DoMouseMove);}
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

procedure TMainPresenter.StartDragImg;
begin

end;

end.
