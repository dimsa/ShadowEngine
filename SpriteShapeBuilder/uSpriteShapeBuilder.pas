unit uSpriteShapeBuilder;

interface

uses
  System.Generics.Collections, FMX.Objects, FMX.StdCtrls, System.Classes, FMX.Forms,
  FMX.Dialogs, System.SysUtils, System.UITypes, FMX.Types, System.Types,
  System.JSON, FMX.Controls, FMX.Layouts,
  uSSBElement, uNamedList, uEasyDevice, uSSBFigure, uClasses,
  uSSBModels, uView, uSSBTypes, uImagerPresenter, uObjecterPresenter;

type
  TSpriteShapeBuilder = class(TInterfacedObject, ISerializable)
  private
    FStatus: TSSBStatus;
    FPanel: TPanel;
    FForm: TForm;

    // Контролы для переключения статуса
    FPanels: array[TSSBStatus] of TLayout;
    FTabsRect: array[TSSBStatus] of TRectangle;
    FTabsImg: array[TSSBStatus] of TImage;

    FView: TView;
    FModel: TSSBModel;
    FControllers: array[TSSBStatus] of TImagerPresenter;
    FSelectedElement: TSSBElement;
    FSelectedImage: TImage;
    FLockPoint: Boolean;
    FIsMouseDown: Boolean;
//    FShaper: TSSBShaperPresenter;
    FObjecter: TObjecterPresenter;
    FImager: TImagerPresenter;

    procedure DoChangeStatus(ASender: TObject);

    procedure DoSaveProject(ASender: TObject);
    procedure DoLoadProject(ASender: TObject);

    function Serialize: TJSONObject;
    procedure Deserialize(const AJson: TJSONObject);
    procedure SetStatus(const Value: TSSBStatus);
    function GetController: TImagerPresenter;
    function FormTopLeft: TPointF;
    procedure OnModelUpdate(ASender: TObject);
  public
    property Status: TSSBStatus read FStatus write SetStatus;
    property IsMouseDown: Boolean read FIsMouseDown write FIsMouseDown;
    property Controller: TImagerPresenter read GetController;
    property Imager: TImagerPresenter read FImager;
    property Objecter: TObjecterPresenter read FObjecter;
//    property Shaper: TSSBShaperPresenter read FShaper;
//    property Elements: TNamedList<TSSBElement> read FElements write FElements;
//    property Elements[Index: Integer]: read GetElements write SetElements;
//    property SelectedElement: TSSBElement read FSelectedElement write SetSelectedElement;
{    procedure AddElement(const AFileName: string); overload;
    procedure AddElement(const AElement: TSSBElement); overload;    }
    procedure LoadProject(const AFileName: string);
    procedure SaveProject(const AFileName: string);
    procedure SaveForEngine(const AFileName: string);
    constructor Create(AForm: TForm; APanel: TPanel; ABackground, ASelected: TImage;
  AOpenDialog: TOpenDialog);
    procedure Init(const AProgForm: TForm);
    destructor Destroy; override;
  const
    CPrec = 5;
  end;


implementation

uses
  SSBMainForm;

{ TSpriteShapeBuilder }

constructor TSpriteShapeBuilder.Create(AForm: TForm; APanel: TPanel; ABackground,
  ASelected: TImage; AOpenDialog: TOpenDialog);
begin
  FForm := AForm;
  FView := TView.Create(APanel, ABackground, ASelected, AOpenDialog, FormTopLeft);
  FModel := TSSBModel.Create(OnModelUpdate);
  FImager := TImagerPresenter.Create(FView, FModel);
  FObjecter := TObjecterPresenter.Create(FView, FModel);
end;

procedure TSpriteShapeBuilder.Deserialize(const AJson: TJSONObject);
begin

end;

destructor TSpriteShapeBuilder.Destroy;
var
  vSSBElement: TSSBElement;
  vImg: TImage;
begin
  FView.Free;
  FImager.Free;

  inherited;
end;

procedure TSpriteShapeBuilder.DoChangeStatus(ASender: TObject);
var
  vName: String;
begin

  vName := LowerCase(TControl(ASender).Name);
  if vName.Contains('picture') then
    Status := sPicture;

  if vName.Contains('object') then
    Status := sObject;

  if vName.Contains('shape') then
    Status := sShape;
end;

procedure TSpriteShapeBuilder.DoLoadProject(ASender: TObject);
begin
  LoadProject('JSONoutput.txt');
end;

procedure TSpriteShapeBuilder.DoSaveProject(ASender: TObject);
begin
  SaveProject('JSONoutput.txt');
end;

function TSpriteShapeBuilder.FormTopLeft: TPointF;
begin
  Result := FForm.ClientToScreen(TPoint.Zero);
end;

function TSpriteShapeBuilder.GetController: TImagerPresenter;
begin
  Result := FControllers[FStatus];
end;

procedure TSpriteShapeBuilder.Init(const AProgForm: TForm);
var
  vDelBtn, vAddCircle, vAddPoly, vSavePrjBtn, vLoadPrjBtn: TCornerButton;
  i: Integer;
  iStatus: TSSBStatus;
begin
  FPanel := TPanel(AProgForm.FindComponent('MainPanel'));
  with FPanel do
  begin
    try
      Canvas.BeginScene;
      Canvas.Fill.Color := TAlphaColorRec.Blanchedalmond;
      Canvas.FillRect(FPanel.BoundsRect, 0, 0, [], 1, FMX.Types.TCornerType.Round);
    finally
      Canvas.EndScene;
    end;

  end;

  FPanels[sPicture] := TLayout(AProgForm.FindComponent('Picture_Inst'));
  FPanels[sObject] := TLayout(AProgForm.FindComponent('Object_Inst'));
  FPanels[sShape] := TLayout(AProgForm.FindComponent('Shape_Inst'));

  Status := sPicture;

  FTabsRect[sPicture] := TRectangle(AProgForm.FindComponent('Picture_Rect'));
  FTabsRect[sObject] := TRectangle(AProgForm.FindComponent('Object_Rect'));
  FTabsRect[sShape] := TRectangle(AProgForm.FindComponent('Shape_Rect'));

  FTabsImg[sPicture] := TImage(AProgForm.FindComponent('Picture_Img'));
  FTabsImg[sObject] := TImage(AProgForm.FindComponent('Object_Img'));
  FTabsImg[sShape] := TImage(AProgForm.FindComponent('Shape_Img'));

  Status := TSSBStatus.sPicture;
  Imager.Init;
end;

procedure TSpriteShapeBuilder.LoadProject(const AFileName: string);
var
  vList: TStringList;
  vItem: TJSONValue;
  vObj: TJSONObject;
  vArr: TJSONArray;
  vSSB: TSSBElement;
begin
  vList := TStringList.Create;
  vList.LoadFromFile(AFileName);

  vObj := TJSONObject(TJSONObject.ParseJSONValue(vList.Text));

  vArr := TJSONArray(vObj.GetValue('Elements'));

  for vItem in vArr do
  begin
    vSSB := TSSBElement.Create(FPanel);
    vSSB.Deserialize(TJSONObject(vItem));
    //AddElement(vSSB);
  end;

  vList.Free;
end;

procedure TSpriteShapeBuilder.OnModelUpdate(ASender: TObject);
begin
//  FImager.OnModelUpdate(ASender);
end;

procedure TSpriteShapeBuilder.SaveForEngine(const AFileName: string);
begin

end;

procedure TSpriteShapeBuilder.SaveProject(const AFileName: string);
var
  vList: TStringList;
begin
  vList := TStringList.Create;
  vList.Add(Self.Serialize.ToJSON);
  vList.SaveToFile(AFileName);
  vList.Free;
end;

function TSpriteShapeBuilder.Serialize: TJSONObject;
var
  vObj, vElem: TJSONObject;
  vArr: TJSONArray;
  i: Integer;
begin
  vObj := TJSONObject.Create;
  vArr := TJSONArray.Create;

{  for i := 0 to FElements.Count - 1 do
  begin
    vElem := FElements[i].Serialize;
    vArr.AddElement(vElem);
  end;  }
  vObj.AddPair('Elements', vArr);

  Result := vObj;
end;

procedure TSpriteShapeBuilder.SetStatus(const Value: TSSBStatus);
begin
  FPanels[FStatus].Visible := False;
  FStatus := Value;
  FPanels[FStatus].Visible := True;

  if Value = TSSBStatus.sShape then
    FObjecter.ShowShapes
  else
    FObjecter.HideShapes;
//  FPanels[Value].Visible := True;
end;

end.


