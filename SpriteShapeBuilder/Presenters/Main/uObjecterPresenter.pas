unit uObjecterPresenter;

interface

uses
  System.Generics.Collections, FMX.Objects,
  uIView, uItemPresenterProxy, uMVPFrameWork, uSSBModels, uSSBTypes,
  uIItemView, uItemObjecterPresenter;

type
  TObjecterPresenter = class(TPresenter)
  private
    FSelected: TItemObjecterPresenter;
    FCaptured: TItemObjecterPresenter;
  protected
    FModel: TSSBModel;
    FItems: TDictionary<TItemObjecterPresenter, IItemView>;
    function GetView: IMainView;
    property View: IMainView read GetView;
    // Методы на клик
    procedure DoSelectItem(ASender: TObject);
    procedure DoCaptureItem(ASender: TObject);
    procedure DoUncaptureItem(ASender: TObject);
    procedure DoDeleteItem(ASender: TObject);
    procedure DoDragCapturedItem(ASender: TObject);
  public
    constructor Create(AView: IView; AModel: TSSBModel);
    procedure AddObj;
  end;

implementation

uses
  System.Types, FMX.Types, System.UITypes;

{ TObjecterPresenter }

procedure TObjecterPresenter.AddObj;
var
  vImg: TImage;
  vViewItem: IItemView;
  vItemPresenter: TItemPresenterProxy;
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
    vViewItem.Left := 0;
    vViewItem.Top := 0;
    vViewItem.Width := 50;//Round(vImg.Width);
    vViewItem.Height:= 50; //Round(vImg.Height);
    vViewItem.AssignBitmap(vImg.Bitmap);

    // Creating Presenter
    vItemPresenter := TItemPresenterProxy.Create(vViewItem, sObject);
    vViewItem.Presenter := vItemPresenter;
    vItemPresenter.OnSelect := DoSelectItem;
    vItemPresenter.OnCapture:= DoCaptureItem;
    vItemPresenter.OnUnCapture:= DoUnCaptureItem;

    FItems.Add(TItemObjecterPresenter(vItemPresenter.Instance), vViewItem);
    try
      vViewItem.AssignBitmap(vImg.Bitmap);
    except
      vImg.Free;
      View.RemoveElement(vViewItem);
    end;
end;

constructor TObjecterPresenter.Create(AView: IView; AModel: TSSBModel);
begin
  FItems := TDictionary<TItemObjecterPresenter, IItemView>.Create;
  FView := AView;
  FModel := AModel;
end;

procedure TObjecterPresenter.DoCaptureItem(ASender: TObject);
begin

end;

procedure TObjecterPresenter.DoDeleteItem(ASender: TObject);
begin

end;

procedure TObjecterPresenter.DoDragCapturedItem(ASender: TObject);
begin

end;

procedure TObjecterPresenter.DoSelectItem(ASender: TObject);
begin
  if (ASender is TItemObjecterPresenter) then
  begin
    FSelected := TItemObjecterPresenter(ASender);
    View.SelectElement(FItems[FSelected]);
  end;
end;

procedure TObjecterPresenter.DoUncaptureItem(ASender: TObject);
begin

end;

function TObjecterPresenter.GetView: IMainView;
begin
  Result := IMainView(FView);
end;

end.
