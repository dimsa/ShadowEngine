unit uWorkSpaceView;

interface

uses
  System.Generics.Collections, System.SysUtils, System.Types, FMX.Graphics,
  FMX.Controls, FMX.Layouts,  FMX.Objects, FMX.StdCtrls, FMX.Forms, FMX.Dialogs,
  FMX.Types, System.Classes, System.UITypes, uEasyDevice, uNamedOptionsForm,
  uSSBTypes, uIWorkSpaceView, uIItemView, uItemView, uMVPFrameWork, FMX.Effects,
  uImagerPresenter, uObjecterPresenter, uITableView, uNamedTableView, uMainPanelFrame;

type
  TGraphicItemWorkspace = class(TInterfacedObject, IWorkSpaceView, IView)
  private
    FElements: TDictionary<IItemView, TItemView>;
    FOptionsForm: TNamedOptionsForm;
    FEffect: TGlowEffect;
    FParentTopLeft: TPointFunction;
    FFrame: TMainPanelFrame;
    FSelected: TImage;
    FOpenDialog: TOpenDialog;
    FObjecter: IInterface;
    FImager: IInterface;
    function PanelTopLeft: TPointF;
    function GetImager: TImagerPresenter;
    function GetObjecter: TObjecterPresenter;
    procedure SetImager(const Value: TImagerPresenter);
    procedure SetObjecter(const Value: TObjecterPresenter);
    procedure OnWorkspaceMouseDown(ASender: TObject);
    procedure OnWorkspaceMouseUp(ASender: TObject);
    procedure OnWorkspaceMouseMove(ASender: TObject);
  public
    constructor Create(AFrame: TMainPanelFrame;{APanel: TPanel; ABackground,} ASelected: TImage;
      AOpenDialog: TOpenDialog; AParentTopLeft: TPointFunction);
    destructor Destroy; override;
    procedure ClearAndFreeImg;
    function GetMousePos: TPoint;
    function AddElement: IItemView;
    function GetScale: Single;
    procedure RemoveElement(const AElement: IItemView);
    procedure SelectElement(const AElement: IItemView);
    procedure SetBackground(const AImg: TImage);
    function FilenameFromDlg(out AFileName: string): boolean;
    procedure ChangeCursor(const ACursor: TCursor);
    function AddTableView: ITableView;
    property Imager: TImagerPresenter read GetImager write SetImager;
    property Objecter: TObjecterPresenter read GetObjecter write SetObjecter;
  end;

implementation

{ TSSBView }

function TGraphicItemWorkspace.PanelTopLeft: TPointF;
begin
  Result := (FFrame.Position.Point + FParentTopLeft);
end;

function TGraphicItemWorkspace.AddElement: IItemView;
var
  vImg: TItemView;
begin
  vImg := TItemView.Create(FFrame.Panel, PanelTopLeft);
  FElements.Add(vImg, vImg);
  vImg.Image.WrapMode := TImageWrapMode.Stretch;
  Result := vImg;
end;

function TGraphicItemWorkspace.AddTableView: ITableView;
begin
  Result := TTableView.Create;
end;

procedure TGraphicItemWorkspace.ChangeCursor(const ACursor: TCursor);
begin
  if ACursor = FFrame.MainImg.Cursor then
    Exit;
  FFrame.MainImg.Cursor := ACursor;
end;

procedure TGraphicItemWorkspace.ClearAndFreeImg;
begin

end;

constructor TGraphicItemWorkspace.Create(AFrame: TMainPanelFrame; ASelected: TImage;
  AOpenDialog: TOpenDialog; AParentTopLeft: TPointFunction);
begin
  FElements := TDictionary<IItemView, TItemView>.Create;
  FSelected := ASelected;
  FOpenDialog := AOpenDialog;
  FParentTopLeft := AParentTopLeft;
  FOptionsForm := TNamedOptionsForm.Create(nil);
  FEffect := TGlowEffect.Create(nil);

  FFrame := AFrame;
  with FFrame do begin
    MouseDowned := OnWorkspaceMouseDown;
    MouseUpped := OnWorkspaceMouseUp;
    MouseMoved := OnWorkspaceMouseMove;
  end;
end;

destructor TGraphicItemWorkspace.Destroy;
var
  vItem: TPair<IItemView, TItemView>;
begin
  FImager := nil;
  FObjecter := nil;

  FEffect.Free;
  for vItem in FElements do
    FElements.Remove(vItem.Key);

  FElements.Clear;
  FElements.Free;

  FFrame := nil;
  FSelected := nil;
  FOpenDialog := nil;

  inherited;
end;

function TGraphicItemWorkspace.FilenameFromDlg(out AFileName: string): Boolean;
begin
  AFileName := '';
  Result := FOpenDialog.Execute;
  if Result then
    AFileName := FOpenDialog.FileName;
end;

function TGraphicItemWorkspace.GetImager: TImagerPresenter;
begin
  Result := TImagerPresenter(FImager);
end;

function TGraphicItemWorkspace.GetMousePos: TPoint;
begin
  Result := ((uEasyDevice.MousePos - FFrame.Position.Point - FParentTopLeft) / FFrame.Panel.Scale.X).Round;
end;

function TGraphicItemWorkspace.GetObjecter: TObjecterPresenter;
begin
  Result := TObjecterPresenter(FObjecter);
end;

function TGraphicItemWorkspace.GetScale: Single;
begin
  Result := FFrame.Panel.Scale.X;
end;

procedure TGraphicItemWorkspace.OnWorkspaceMouseDown(ASender: TObject);
begin
  Objecter.MouseDown;
  Imager.MouseDown;
end;

procedure TGraphicItemWorkspace.OnWorkspaceMouseMove(ASender: TObject);
begin
  Objecter.MouseMove;
  Imager.MouseMove;
end;

procedure TGraphicItemWorkspace.OnWorkspaceMouseUp(ASender: TObject);
begin
  Objecter.MouseUp;
  Imager.MouseUp;
end;

procedure TGraphicItemWorkspace.RemoveElement(const AElement: IItemView);
var
  vItem: TItemView;
begin
  FEffect.Parent := nil;
  FElements.Remove(AElement);
  vItem := TItemView(AElement);
  vItem.Image.Free;
end;

procedure TGraphicItemWorkspace.SelectElement(const AElement: IItemView);
begin
  FSelected.Bitmap.Assign(FElements[AElement].Image.Bitmap);
  FEffect.Parent := FElements[AElement].Image;
end;

procedure TGraphicItemWorkspace.SetBackground(const AImg: TImage);
begin

end;

procedure TGraphicItemWorkspace.SetImager(const Value: TImagerPresenter);
begin
  FImager := Value;
end;

procedure TGraphicItemWorkspace.SetObjecter(const Value: TObjecterPresenter);
begin
  FObjecter := Value;
end;

end.
