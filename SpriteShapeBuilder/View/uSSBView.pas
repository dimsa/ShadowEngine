unit uSSBView;

interface

uses
  System.Generics.Collections, System.SysUtils, System.Types, FMX.Graphics,
  FMX.Controls, FMX.Layouts,  FMX.Objects, FMX.StdCtrls, FMX.Forms, FMX.Dialogs,
  uSSBTypes, uSSBModels, uIView;

type
  TLinkedImage = class
  private
    FLink: TObject;
    FImage: TImage;
  public
    property Link: TObject read FLink write FLink;
    property Image: TImage read FImage write FImage;
    procedure AssignImage(const AObject: TObject); virtual;
  end;

  TSSBView = class(TInterfacedObject, ISSBView)
  private
    FModel: TSSBModel;
    FChangeblePanel: TLayout;
    FElements: TList<TImage>;
    FPanel: TPanel;
    FBackground: TImage;
    FSelected: TImage;
    FOpenDialog: TOpenDialog;
    procedure SetModel(const Value: TSSBModel);
    procedure CopyEvents(const AFromControl: TControl; AToControl: TControl);
  public

   { property Selected: TImage read FSelected;
    property Elements: TList<TLinkedImage> read FElements;
    property ChangeblePanel: TLayout read FChangeblePanel;
    property Background: TImage read FBackground;
    property Model: TSSBModel read FModel write SetModel;
    procedure Update(Sender: TObject);
    procedure Init(const AProgForm: TForm); // Инициализируем где будут размещатсья элементы   }
    constructor Create(APanel: TPanel; ABackground, ASelected: TImage; AOpenDialog: TOpenDialog);
    destructor Destroy; override;
    function AddImage(const AImg: TBitmap): TControl;
    procedure ClearAndFreeImg;
    function GetMousePos: TPoint;
    procedure RemoveImage;
    procedure SelectImage;
    procedure SetBackground(const AImg: TImage);
    function FilenameFromDlg: string;
  end;

implementation

{ TSSBView }

function TSSBView.AddImage(const AImg: TBitmap): TControl;
var
  vImg: TImage;
begin
  vImg := TImage.Create(FPanel);
  vImg.Parent := FPanel;
  vImg.Width := AImg.Width;
  vImg.Height:= AImg.Height;
  vImg.Bitmap.Assign(AImg);
  FElements.Add(vImg);
  Result := vImg;
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
  AOpenDialog: TOpenDialog);
begin
  FElements := TList<TImage>.Create;
  FPanel := APanel;
  FBackground := ABackground;
  FSelected := ASelected;
  FOpenDialog := AOpenDialog;
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
  inherited;
end;

function TSSBView.FilenameFromDlg: string;
begin
  Result := '';
  if FOpenDialog.Execute then
    Result := FOpenDialog.FileName;
end;

function TSSBView.GetMousePos: TPoint;
begin

end;

procedure TSSBView.RemoveImage;
begin

end;

{procedure TSSBView.Init(const AProgForm: TForm);
begin
  FSelected := TImage(AProgForm.FindComponent('Selected'));
  FBackground := TImage(AProgForm.FindComponent('Background'));
  FElements := TList<TLinkedImage>.Create;
  FPanel := TPanel(AProgForm.FindComponent('MainPanel'));
end;  }

procedure TSSBView.SelectImage;
begin

end;

procedure TSSBView.SetBackground(const AImg: TImage);
begin

end;

procedure TSSBView.SetModel(const Value: TSSBModel);
begin
  FModel := Value;
  //Update(Self);
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

procedure TLinkedImage.AssignImage(const AObject: TObject);
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

end;

end.
