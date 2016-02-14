unit uView;

interface

uses
  System.Generics.Collections, System.SysUtils, System.Types, FMX.Graphics,
  FMX.Controls, FMX.Layouts,  FMX.Objects, FMX.StdCtrls, FMX.Forms, FMX.Dialogs,
  FMX.Types, System.Classes, System.UITypes, uEasyDevice,
  uSSBTypes, uSSBPresenters, uIView, uIItemView, uItemView;

type
  TView = class(TInterfacedObject, IMainView)
  private
    FChangeblePanel: TLayout;
    FElements: TList<TItemView>;
    FPanel: TPanel;
    FFormPosition: TPositionFunc;
    FBackground: TImage;
    FSelected: TImage;
    FOpenDialog: TOpenDialog;
    FMouseMoveHandler: TMouseMoveEvent;
    FMouseDownHandler: TMouseEvent;
    FMouseUpHandler: TMouseEvent;
    procedure CopyEvents(const AFromControl: TControl; AToControl: TControl);
    function ElementByInterface(AElem: IItemView): TItemView;
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
//    function ElementUnderMouse: ISSBViewElement;
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
    procedure ChangeImageMouseDownHandler(const AHandler: TMouseEvent);
    procedure ChangeImageMouseUpHandler(const AHandler: TMouseEvent);
    procedure ChangeImageMouseMoveHandler(const AHandler: TMouseMoveEvent);
  end;

implementation

{ TSSBView }

function TView.AddElement: IItemView;
var
  vImg: TItemView;
  vi: TImage;
begin
  vImg := TItemView.Create(FPanel);
  vImg.OnMouseDown := MouseDown;
  vImg.OnMouseUp := FMouseUpHandler;
  vImg.OnMouseMove := FMouseMoveHandler;
  FElements.Add(vImg);
  Result := vImg;
end;

procedure TView.ChangeImageMouseDownHandler(const AHandler: TMouseEvent);
begin
  FMouseDownHandler := AHandler;
end;

procedure TView.ChangeImageMouseMoveHandler(const AHandler: TMouseMoveEvent);
begin
  FMouseMoveHandler := AHandler;
end;

procedure TView.ChangeImageMouseUpHandler(const AHandler: TMouseEvent);
begin
  FMouseUpHandler := AHandler;
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
  FElements := TList<TItemView>.Create;
  FPanel := APanel;
  FBackground := ABackground;
  FSelected := ASelected;
  FOpenDialog := AOpenDialog;
  FFormPosition := AFormPosition;
end;

destructor TView.Destroy;
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

function TView.ElementByInterface(AElem: IItemView): TItemView;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FElements.Count - 1 do
    if IItemView(FElements[i]) = AElem then
      Exit(FElements[i]);
end;

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
  vElem := ElementByInterface(AElement);
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

end;

procedure TView.SetBackground(const AImg: TImage);
begin

end;

end.
