unit uSSBView;

interface

uses
  System.Generics.Collections, System.SysUtils, System.Types,
  FMX.Controls, FMX.Layouts,  FMX.Objects, FMX.StdCtrls, FMX.Forms,
  uSSBTypes, uSSBModels;

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

  TSSBView = class
  private
    FModel: TSSBModel;
    FChangeblePanel: TLayout;
    FElements: TList<TLinkedImage>;
    FPanel: TPanel;
    FBackground: TImage;
    FSelected: TImage;
    procedure SetModel(const Value: TSSBModel);
  public
    property Selected: TImage read FSelected;
    property Elements: TList<TLinkedImage> read FElements;
    property ChangeblePanel: TLayout read FChangeblePanel;
    property Background: TImage read FBackground;
    property Model: TSSBModel read FModel write SetModel;
    procedure Update(Sender: TObject);
    procedure Init(const AProgForm: TForm); // Инициализируем где будут размещатсья элементы
    constructor Create;
  end;

implementation

{ TSSBView }

constructor TSSBView.Create;
begin

end;

procedure TSSBView.Init(const AProgForm: TForm);
begin
  FSelected := TImage(AProgForm.FindComponent('Selected'));
  FBackground := TImage(AProgForm.FindComponent('Background'));
  FElements := TList<TLinkedImage>.Create;
  FPanel := TPanel(AProgForm.FindComponent('MainPanel'));
end;

procedure TSSBView.SetModel(const Value: TSSBModel);
begin
  FModel := Value;
  Update(Self);
end;

procedure TSSBView.Update;
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

     { FElements.Last.Image := TImage.Create(FPanel);
      FElements.Last.Image.Parent := FPanel;
      FElements.Last.AssignImage(TControl(vObj)); }
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

end;

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
