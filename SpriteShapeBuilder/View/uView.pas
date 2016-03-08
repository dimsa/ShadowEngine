unit uView;

interface

uses
  System.Generics.Collections, System.SysUtils, System.Types, FMX.Graphics,
  FMX.Controls, FMX.Layouts,  FMX.Objects, FMX.StdCtrls, FMX.Forms, FMX.Dialogs,
  FMX.Types, System.Classes, System.UITypes, uEasyDevice,
  uSSBTypes, uIView, uIItemView, uItemView, uMVPFrameWork, FMX.Effects;

type
  TView = class(TInterfacedObject, IMainView, IView)
  private
    FChangeblePanel: TLayout;
    FElements: TDictionary<IItemView, TItemView>;
    FEffect: TGlowEffect;
    FPanel: TPanel;
    FFormPosition: TPositionFunc;
    FBackground: TImage;
    FSelected: TImage;
    FOpenDialog: TOpenDialog;
    procedure CopyEvents(const AFromControl: TControl; AToControl: TControl);
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
    function AddElement: IItemView;
    procedure RemoveElement(const AElement: IItemView);
    procedure SelectElement(const AElement: IItemView);
    procedure SetBackground(const AImg: TImage);
    function FilenameFromDlg: string;
    procedure ChangeCursor(const ACursor: TCursor);
  end;

implementation

{ TSSBView }

function TView.AddElement: IItemView;
var
  vImg: TItemView;
  vi: TImage;
begin
  vImg := TItemView.Create(FPanel);
  FElements.Add(vImg, vImg);
  vImg.Image.WrapMode := TImageWrapMode.Stretch;
  Result := vImg;
end;

procedure TView.ChangeCursor(const ACursor: TCursor);
begin
  if ACursor = FPanel.Cursor then
    Exit;
  FPanel.Cursor := ACursor;
end;

procedure TView.ClearAndFreeImg;
begin

end;

procedure TView.CopyEvents(const AFromControl: TControl;
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

constructor TView.Create(APanel: TPanel; ABackground, ASelected: TImage;
  AOpenDialog: TOpenDialog; AFormPosition: TPositionFunc);
begin
  FElements := TDictionary<IItemView, TItemView>.Create;
  FPanel := APanel;
  FBackground := ABackground;
  FSelected := ASelected;
  FOpenDialog := AOpenDialog;
  FFormPosition := AFormPosition;
  FEffect := TGlowEffect.Create(nil);
end;

destructor TView.Destroy;
var
  i: Integer;
  vItem: TPair<IItemView, TItemView>;
begin
  FEffect.Free;
  for vItem in FElements do
    FElements.Remove(vItem.Key);

 {for i := 0 to FElements.Count - 1 do
    FElements[i].Free;}
  FElements.Clear;
  FElements.Free;

  FPanel := nil;
  FBackground := nil;
  FSelected := nil;
  FOpenDialog := nil;


  inherited;
end;

{unction TView.ElementByInterface(AElem: IItemView): TItemView;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FElements.Count - 1 do
    if IItemView(FElements[i]) = AElem then
      Exit(FElements[i]);
end; }

function TView.FilenameFromDlg: string;
begin
  Result := '';
  if FOpenDialog.Execute then
    Result := FOpenDialog.FileName;
end;

function TView.GetMousePos: TPoint;
var
  vPoint: TPointF;
begin
  vPoint := uEasyDevice.MousePos;
  Result := (FFormPosition(vPoint) - FPanel.Position.Point).Round;
end;

procedure TView.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
//  Sender.
end;

procedure TView.RemoveElement(const AElement: IItemView);
var
  i: Integer;
  vElem: TItemView;
begin
//  vElem := ElementByInterface(AElement);
  FElements.Remove(vElem);
  vElem.Free;
  {for i := 0 to FElements.Count - 1 do
    if IItemView(FElements[i]) = AElement then
    begin
      vElem := FElements[i];
      FElements.Remove(vElem);
      vElem.Free;
    end;}
 end;

procedure TView.SelectElement(const AElement: IItemView);
begin
  FSelected.Bitmap.Assign(FElements[AElement].Image.Bitmap);
  FEffect.Parent := FElements[AElement].Image;
end;

procedure TView.SetBackground(const AImg: TImage);
begin

end;

end.
