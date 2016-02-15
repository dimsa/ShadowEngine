unit uImagerPresenter;

interface

uses
  System.Types, System.Generics.Collections, FMX.Objects,
  uIView,
  uIItemView, uImagerItemPresenter, uSSBModels, uMainPresenter, uMVPFrameWork;


type
  TImagerPresenter = class(TMainPresenter)
  private
    FSelected: TImagerItemPresenter;
    FIsMouseDown: Boolean;
    FMouseStartPoint: TPointF;
    FMouseElementPoint: TPointF;
    FElementStartPosition: TPointF;
    FItems: TDictionary<TImagerItemPresenter, IItemView>;

    // Методы на клик
    procedure DoSelectItem(ASender: TObject);
    procedure DoDelImage(ASender: TObject);
  public
    procedure AddImg;
    //procedure SelImg;
    procedure DelImg;
    procedure DragImg;
    procedure StartDragImg;
    procedure FinishDragImg;
    procedure Init; override;
    constructor Create(AView: IView; AModel: TSSBModel); override;
    destructor Destroy; override;
  end;

implementation

procedure TImagerPresenter.AddImg;
var
  vFileName: string;
  vViewItem: IItemView;
  vItemPresenter: TImagerItemPresenter;
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
    vItemPresenter := TImagerItemPresenter.Create(vViewItem);
    vItemPresenter.OnSelect := DoSelectItem;

    FItems.Add(vItemPresenter, vViewItem);
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

procedure TImagerPresenter.DoDelImage(ASender: TObject);
begin

end;

procedure TImagerPresenter.DoSelectItem(ASender: TObject);
begin
  if (ASender is TImagerItemPresenter) then
  begin
    FSelected := TImagerItemPresenter(ASender);
    View.SelectElement(FItems[FSelected]);
  end;
end;

procedure TImagerPresenter.DragImg;
begin

end;

procedure TImagerPresenter.FinishDragImg;
begin

end;

procedure TImagerPresenter.Init;
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

procedure TImagerPresenter.StartDragImg;
begin

end;

end.
