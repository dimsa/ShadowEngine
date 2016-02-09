unit uSSBView;

interface

uses
  System.Generics.Collections, System.SysUtils, System.Types, FMX.Graphics,
  FMX.Controls, FMX.Layouts,  FMX.Objects, FMX.StdCtrls, FMX.Forms, FMX.Dialogs,
  FMX.Types, System.Classes, System.UITypes, uEasyDevice,
  uSSBTypes, uSSBPresenters, uIView;

type
  {TLinkedImage = class
  private
    FLink: TObject;
    FImage: TImage;
  public
    property Link: TObject read FLink write FLink;
    property Image: TImage read FImage write FImage;
    procedure AssignImage(const AObject: TObject); virtual;
  end;    }

  TElement = class(TInterfacedObject, ISSBViewElement)
  private
    FImage: TImage;
    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);
    function GetHeight: Integer;
    procedure SetHeight(AValue: Integer);
    function GetTop: Integer;
    procedure SetTop(AValue: Integer);
    function GetLeft: Integer;
    procedure SetLeft(AValue: Integer);
    function GetOnMouseDown: TMouseEvent;
    function GetOnMouseMove: TMouseMoveEvent;
    function GetOnMouseUp: TMouseEvent;
    procedure SetOnMouseDown(const Value: TMouseEvent);
    procedure SetOnMouseMove(const Value: TMouseMoveEvent);
    procedure SetOnMouseUp(const Value: TMouseEvent);
  public
    procedure AssignBitmap(ABmp: TBitmap);
    property OnMouseMove: TMouseMoveEvent read GetOnMouseMove write SetOnMouseMove;
    property OnMouseDown: TMouseEvent read GetOnMouseDown write SetOnMouseDown;
    property OnMouseUp: TMouseEvent read GetOnMouseUp write SetOnMouseUp;
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
  end;

  TSSBView = class(TInterfacedObject, ISSBView)
  private
    FChangeblePanel: TLayout;
    FElements: TList<TElement>;
    FPanel: TPanel;
    FFormPosition: TPositionFunc;
    FBackground: TImage;
    FSelected: TImage;
    FOpenDialog: TOpenDialog;
    FMouseMoveHandler: TMouseMoveEvent;
    FMouseDownHandler: TMouseEvent;
    FMouseUpHandler: TMouseEvent;
    procedure CopyEvents(const AFromControl: TControl; AToControl: TControl);
    function ElementByInterface(AElem: ISSBViewElement): TElement;
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  public

   { property Selected: TImage read FSelected;
    property Elements: TList<TLinkedImage> read FElements;
    property ChangeblePanel: TLayout read FChangeblePanel;
    property Background: TImage read FBackground;
    property Model: TSSBModel read FModel write SetModel;
    procedure Update(Sender: TObject);
    procedure Init(const AProgForm: TForm); // Инициализируем где будут размещатсья элементы   }
    constructor Create(APanel: TPanel; ABackground, ASelected: TImage; AOpenDialog: TOpenDialog; AFormPosition: TPositionFunc);
    destructor Destroy; override;
    procedure ClearAndFreeImg;
    function GetMousePos: TPoint;
    function ElementUnderMouse: ISSBViewElement;
    function AddElement: ISSBViewElement;
    procedure RemoveElement(const AElement: ISSBViewElement);
    procedure SelectElement(const AElement: ISSBViewElement);
    procedure SetBackground(const AImg: TImage);
    function FilenameFromDlg: string;
    procedure ChangeImageMouseDownHandler(const AHandler: TMouseEvent);
    procedure ChangeImageMouseUpHandler(const AHandler: TMouseEvent);
    procedure ChangeImageMouseMoveHandler(const AHandler: TMouseMoveEvent);
  end;

implementation

{ TSSBView }

function TSSBView.AddElement: ISSBViewElement;
var
  vImg: TElement;
  vi: TImage;
begin
  vImg := TElement.Create(FPanel);

//  vImg.Parent := FPanel;

  vImg.OnMouseDown := FMouseDownHandler;
  vImg.OnMouseUp := FMouseUpHandler;
  vImg.OnMouseMove := FMouseMoveHandler;
  FElements.Add(vImg);
  Result := vImg;
end;

procedure TSSBView.ChangeImageMouseDownHandler(const AHandler: TMouseEvent);
begin
  FMouseDownHandler := AHandler;
end;

procedure TSSBView.ChangeImageMouseMoveHandler(const AHandler: TMouseMoveEvent);
begin
  FMouseMoveHandler := AHandler;
end;

procedure TSSBView.ChangeImageMouseUpHandler(const AHandler: TMouseEvent);
begin
  FMouseUpHandler := AHandler;
end;

procedure TSSBView.ClearAndFreeImg;
begin

end;

procedure TSSBView.CopyEvents(const AFromControl: TControl;
  AToControl: TControl);
begin
  AToControl.OnDragEnter := AFromControl.OnDragEnter;
  AToControl.OnDragLeave := AFromControl.OnDragLeave;
  AToControl.OnDragOver := AFromControl.OnDragOver;
  AToControl.OnDragDrop := AFromControl.OnDragDrop;
  AToControl.OnDragEnd := AFromControl.OnDragEnd;
  AToControl.OnKeyDown := AFromControl.OnKeyDown;
  AToControl.OnKeyUp := AFromControl.OnKeyUp;
  AToControl.OnClick := AFromControl.OnClick;
  AToControl.OnDblClick := AFromControl.OnDblClick;
  AToControl.OnCanFocus := AFromControl.OnCanFocus;
  AToControl.OnEnter := AFromControl.OnEnter;
  AToControl.OnExit := AFromControl.OnExit;
  AToControl.OnMouseDown := AFromControl.OnMouseDown;
  AToControl.OnMouseMove := AFromControl.OnMouseMove;
  AToControl.OnMouseUp := AFromControl.OnMouseUp;
  AToControl.OnMouseWheel := AFromControl.OnMouseWheel;
  AToControl.OnMouseEnter := AFromControl.OnMouseEnter;
  AToControl.OnMouseLeave := AFromControl.OnMouseLeave;
  AToControl.OnPainting := AFromControl.OnPainting;
  AToControl.OnPaint := AFromControl.OnPaint;
  AToControl.OnResize := AFromControl.OnResize;
  AToControl.OnActivate := AFromControl.OnActivate;
  AToControl.OnDeactivate := AFromControl.OnDeactivate;
  AToControl.OnApplyStyleLookup := AFromControl.OnApplyStyleLookup;
  AToControl.OnGesture := AFromControl.OnGesture;
  AToControl.OnTap := AFromControl.OnTap;
end;

constructor TSSBView.Create(APanel: TPanel; ABackground, ASelected: TImage;
  AOpenDialog: TOpenDialog; AFormPosition: TPositionFunc);
begin
  FElements := TList<TElement>.Create;
  FPanel := APanel;
  FBackground := ABackground;
  FSelected := ASelected;
  FOpenDialog := AOpenDialog;
  FFormPosition := AFormPosition;
end;

destructor TSSBView.Destroy;
var
  i: Integer;
begin
  for i := 0 to FElements.Count - 1 do
    FElements[i].Free;
  FElements.Clear;
  FElements.Free;

  FPanel := nil;
  FBackground := nil;
  FSelected := nil;
  FOpenDialog := nil;

  FMouseMoveHandler := nil;
  FMouseDownHandler := nil;
  FMouseUpHandler := nil;

  inherited;
end;

function TSSBView.ElementUnderMouse: ISSBViewElement;
begin

end;

function TSSBView.ElementByInterface(AElem: ISSBViewElement): TElement;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FElements.Count - 1 do
    if ISSBViewElement(FElements[i]) = AElem then
      Exit(FElements[i]);
end;

function TSSBView.FilenameFromDlg: string;
begin
  Result := '';
  if FOpenDialog.Execute then
    Result := FOpenDialog.FileName;
end;

function TSSBView.GetMousePos: TPoint;
var
  vPoint: TPointF;
begin
  vPoint := uEasyDevice.MousePos;
  Result := (FFormPosition(vPoint) - FPanel.Position.Point).Round;
end;

procedure TSSBView.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin

end;

procedure TSSBView.RemoveElement(const AElement: ISSBViewElement);
var
  i: Integer;
  vElem: TElement;
begin
  vElem := ElementByInterface(AElement);
  FElements.Remove(vElem);
  vElem.Free;
  {for i := 0 to FElements.Count - 1 do
    if ISSBViewElement(FElements[i]) = AElement then
    begin
      vElem := FElements[i];
      FElements.Remove(vElem);
      vElem.Free;
    end;}
 end;

{procedure TSSBView.Init(const AProgForm: TForm);
begin
  FSelected := TImage(AProgForm.FindComponent('Selected'));
  FBackground := TImage(AProgForm.FindComponent('Background'));
  FElements := TList<TLinkedImage>.Create;
  FPanel := TPanel(AProgForm.FindComponent('MainPanel'));
end;  }

procedure TSSBView.SelectElement(const AElement: ISSBViewElement);
begin
//  FSelected.Assign(ElementByInterface(AElement).Bitmap);
end;

procedure TSSBView.SetBackground(const AImg: TImage);
begin

end;

{procedure TSSBView.Update;
var
  vObj: TControl;
  vLinkCtrl: TLinkedImage;
  vIsFound: Boolean;
  i: Integer;
begin
  // Обновляем фон и выделение
  if FModel.SelectedBitmap <> nil then
    FSelected.Assign(FModel.SelectedBitmap);

  if FModel.Background <> nil then
    FBackground.Bitmap.Assign(FModel.Background);

  // Добавляем и изменяем все объекты
  for vObj in FModel.Elements do
  begin
    vIsFound := False;
    for vLinkCtrl in FElements do
      if vLinkCtrl.Link = vObj then
      begin
        vIsFound := True;
         vLinkCtrl.AssignImage(TControl(vObj));
      end;
    if not vIsFound then
    begin
      // Здесь необходимо как бы указать шаблон нового компонента
      FElements.Add(TLinkedImage.Create);
      FElements.Last.Link := vObj;
      FElements.Last.Image := TImage.Create(FPanel);
      FElements.Last.Image.Parent := FPanel;

      FElements.Last.AssignImage(vObj);


//      TImage(FElements.Last.Control).Bitmap.Assign(TControl(vObj).);
    end;
  end;

  // Удаляем лишние объекты
  for i := FElements.Count - 1 downto 0 do
  begin
    vLinkCtrl := FElements[i];

    vIsFound := False;
    for vObj in FModel.Elements do
      if vLinkCtrl.Link = vObj then
        vIsFound := True;
    if not vIsFound then
      FElements.Delete(i);
    vLinkCtrl.Free;
  end;

end;   }

{ TLinkedControl }

{procedure TLinkedImage.AssignImage(const AObject: TObject);
begin
  if AObject is TImage then
  begin
    with TImage(AObject) do
    begin
      Image.Width := Width;
      Image.Height := Height;
      Image.Position := Position;
      Image.Bitmap.Assign(Bitmap);
    end;
  end;

end;  }

{ TElement }

procedure TElement.AssignBitmap(ABmp: TBitmap);
begin
  FImage.Width := ABmp.Width;
  FImage.Height:= ABmp.Height;
  FImage.Bitmap.Assign(ABmp);
end;

constructor TElement.Create(AOwner: TControl);
begin
  FImage := TImage.Create(AOwner);
  FImage.Parent := AOwner;
end;

destructor TElement.Destroy;
begin
  FImage.Free;
end;

function TElement.GetHeight: Integer;
begin
  Result := Round(FImage.Height);
end;

function TElement.GetLeft: Integer;
begin
  Result := Round(FImage.Position.X);
end;

function TElement.GetOnMouseDown: TMouseEvent;
begin
  Result := FImage.OnMouseDown;
end;

function TElement.GetOnMouseMove: TMouseMoveEvent;
begin
  Result := FImage.OnMouseMove;
end;

function TElement.GetOnMouseUp: TMouseEvent;
begin
  Result := FImage.OnMouseUp;
end;

function TElement.GetTop: Integer;
begin
  Result := Round(FImage.Position.Y);
end;

function TElement.GetWidth: Integer;
begin
  Result := Round(FImage.Width);
end;

procedure TElement.SetHeight(AValue: Integer);
begin
  FImage.Height := AValue;
end;

procedure TElement.SetLeft(AValue: Integer);
begin
  FImage.Position.X := AValue;
end;

procedure TElement.SetOnMouseDown(const Value: TMouseEvent);
begin
  FImage.OnMouseDown := Value;
end;

procedure TElement.SetOnMouseMove(const Value: TMouseMoveEvent);
begin
  FImage.OnMouseMove := Value;
end;

procedure TElement.SetOnMouseUp(const Value: TMouseEvent);
begin
  FImage.OnMouseUp := Value;
end;

procedure TElement.SetTop(AValue: Integer);
begin
  FImage.Position.Y := AValue;
end;

procedure TElement.SetWidth(AValue: Integer);
begin
  FImage.Width := AValue;
end;

end.
