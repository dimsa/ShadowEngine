unit uView;

interface

uses
  System.Generics.Collections, System.SysUtils, System.Types, FMX.Graphics,
  FMX.Controls, FMX.Layouts,  FMX.Objects, FMX.StdCtrls, FMX.Forms, FMX.Dialogs,
  FMX.Types, System.Classes, System.UITypes, uEasyDevice, uNamedOptionsForm,
  uSSBTypes, uIView, uIItemView, uItemView, uMVPFrameWork, FMX.Effects;

type
  TWorkSpaceView = class(TInterfacedObject, IWorkSpaceView, IView)
  private
    FElements: TDictionary<IItemView, TItemView>;
    FOptionsFrom: TNamedOptionsForm;
    FEffect: TGlowEffect;
    FParentTopLeft: TPointFunction;
    FPanel: TPanel;
    FBackground: TImage;
    FSelected: TImage;
    FOpenDialog: TOpenDialog;
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    function PanelTopLeft: TPointF;
  public
    constructor Create(APanel: TPanel; ABackground, ASelected: TImage; AOpenDialog: TOpenDialog; AParentTopLeft: TPointFunction);
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

function TWorkSpaceView.PanelTopLeft: TPointF;
begin
  Result := (FPanel.Position.Point + FParentTopLeft);
//  FPanel. (FPanel.Position.Point - FParentTopLeft);
end;

{function TView.ParentScreenToClient(const APoint: TPointF): TPointF;
begin
  Result := (FFormPosition(APoint) - FPanel.Position.Point);
end;  }

function TWorkSpaceView.AddElement: IItemView;
var
  vImg: TItemView;
begin
  vImg := TItemView.Create(FPanel, PanelTopLeft);
  FElements.Add(vImg, vImg);
  vImg.Image.WrapMode := TImageWrapMode.Stretch;
  Result := vImg;
end;

procedure TWorkSpaceView.ChangeCursor(const ACursor: TCursor);
begin
  if ACursor = FPanel.Cursor then
    Exit;
  FPanel.Cursor := ACursor;
end;

procedure TWorkSpaceView.ClearAndFreeImg;
begin

end;

{procedure TView.CopyEvents(const AFromControl: TControl;
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
end;}

constructor TWorkSpaceView.Create(APanel: TPanel; ABackground, ASelected: TImage;
  AOpenDialog: TOpenDialog; AParentTopLeft: TPointFunction);
begin
  FElements := TDictionary<IItemView, TItemView>.Create;
  FPanel := APanel;
  FBackground := ABackground;
  FSelected := ASelected;
  FOpenDialog := AOpenDialog;
  FParentTopLeft := AParentTopLeft;
  FOptionsFrom := TNamedOptionsForm.Create(nil);
//  FFormPosition := AFormPosition;
  FEffect := TGlowEffect.Create(nil);
end;

destructor TWorkSpaceView.Destroy;
var
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

function TWorkSpaceView.FilenameFromDlg: string;
begin
  Result := '';
  if FOpenDialog.Execute then
    Result := FOpenDialog.FileName;
end;

function TWorkSpaceView.GetMousePos: TPoint;
begin
  Result := (uEasyDevice.MousePos - FPanel.Position.Point - FParentTopLeft).Round;
end;

procedure TWorkSpaceView.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin

end;

procedure TWorkSpaceView.RemoveElement(const AElement: IItemView);
var
  vItem: TItemView;
begin
  FEffect.Parent := nil;
  FElements.Remove(AElement);
  vItem := TItemView(AElement);
  vItem.Image.Free;
end;

procedure TWorkSpaceView.SelectElement(const AElement: IItemView);
begin
  FSelected.Bitmap.Assign(FElements[AElement].Image.Bitmap);
  FEffect.Parent := FElements[AElement].Image;
end;

procedure TWorkSpaceView.SetBackground(const AImg: TImage);
begin

end;

end.
