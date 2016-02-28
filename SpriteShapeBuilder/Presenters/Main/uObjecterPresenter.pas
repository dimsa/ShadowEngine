unit uObjecterPresenter;

interface

uses
  System.Generics.Collections, FMX.Objects,
  uIView, uItemPresenterProxy, uMVPFrameWork, uSSBModels, uSSBTypes,
  uIItemView;

type
  TObjecterPresenter = class(TPresenter)
  protected
    FModel: TSSBModel;
    FItems: TDictionary<TItemPresenterProxy, IItemView>;
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

{ TObjecterPresenter }

procedure TObjecterPresenter.AddObj;
var
  vImg: TImage;
  vViewItem: IItemView;
  vItemPresenter: TItemPresenterProxy;
begin

    vImg := TImage.Create(nil);
    {vImg.Bitmap.LoadFromFile(vFileName);  }

    // Creating View
    vViewItem := View.AddElement;
    vViewItem.Left := 0;
    vViewItem.Top := 0;
    vViewItem.Width := 100;//Round(vImg.Width);
    vViewItem.Height:= 100; //Round(vImg.Height);

    // Creating Presenter
    vItemPresenter := TItemPresenterProxy.Create(vViewItem, sObject);
    vViewItem.Presenter := vItemPresenter;
    vItemPresenter.OnSelect := DoSelectItem;
    vItemPresenter.OnCapture:= DoCaptureItem;
    vItemPresenter.OnUnCapture:= DoUnCaptureItem;

    FItems.Add(vItemPresenter, vViewItem);
    try
      vViewItem.AssignBitmap(vImg.Bitmap);
    except
      vImg.Free;
      View.RemoveElement(vViewItem);
    end;
end;

constructor TObjecterPresenter.Create(AView: IView; AModel: TSSBModel);
begin
  FItems := TDictionary<TItemPresenterProxy, IItemView>.Create;
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

end;

procedure TObjecterPresenter.DoUncaptureItem(ASender: TObject);
begin

end;

function TObjecterPresenter.GetView: IMainView;
begin
  Result := IMainView(FView);
end;

end.
