unit uSpriteShapeBuilder;

interface

uses
  System.Generics.Collections, FMX.Objects, FMX.StdCtrls, System.Classes, FMX.Forms,
  FMX.Dialogs, System.SysUtils, System.UITypes, FMX.Types, System.Types,
  System.JSON, FMX.Controls, FMX.Layouts,
  uSSBElement, uNamedList, uEasyDevice, uSSBFigure, uClasses, uSSBPresenters,
  uSSBModels, uSSBView, uSSBTypes;

type
  TSpriteShapeBuilder = class(TInterfacedObject, ISerializable)
  private
    FStatus: TSSBStatus;
    FPanel: TPanel;

    // Контролы для переключения статуса
    FPanels: array[TSSBStatus] of TLayout;
    FTabsRect: array[TSSBStatus] of TRectangle;
    FTabsImg: array[TSSBStatus] of TImage;

    FView: TSSBView;
    FControllers: array[TSSBStatus] of TSSBPresenter;
    FModels: array[TSSBStatus] of TSSBModel;

//    FImagerController: TSSBImagerController;
//    FImagerModel: TSSBImagerModel;

//    FElements: TNamedList<TSSBElement>; // Объекты с шейпами
    FSelectedElement: TSSBElement;
    FSelectedImage: TImage;
    FLockPoint: Boolean;
    FIsMouseDown: Boolean;
    FMouseStartPoint, FMouseElementPoint, FElementStartPosition: TPointF;
    FShaper: TSSBShaperPresenter;
    FObjecter: TSSBObjecterPresenter;
    FImager: TSSBImagerPresenter;

    procedure DoChangeStatus(ASender: TObject);

    procedure DoAddCircle(ASender: TObject);
    procedure DoAddPoly(ASender: TObject);
    procedure DoEditShape(ASender: TObject);

    procedure DoSelectPicture(ASender: TObject);
    procedure DoDeletePicture(ASender: TObject);

    procedure DoSelectObject(ASender: TObject);
    procedure DoDeleteObject(ASender: TObject);
    procedure DoEditObject(ASender: TObject);

    procedure DoSaveProject(ASender: TObject);
    procedure DoLoadProject(ASender: TObject);

    procedure SetSelectedElement(const Value: TSSBElement);
    function Serialize: TJSONObject;
    procedure Deserialize(const AJson: TJSONObject);
    procedure SetStatus(const Value: TSSBStatus);
    function GetController: TSSBPresenter;
  public
    property Status: TSSBStatus read FStatus write SetStatus;
    property IsMouseDown: Boolean read FIsMouseDown write FIsMouseDown;
    property Controller: TSSBPresenter read GetController;
    property Imager: TSSBImagerPresenter read FImager;
    property Objecter: TSSBObjecterPresenter read FObjecter;
    property Shaper: TSSBShaperPresenter read FShaper;
//    property Elements: TNamedList<TSSBElement> read FElements write FElements;
//    property Elements[Index: Integer]: read GetElements write SetElements;
//    property SelectedElement: TSSBElement read FSelectedElement write SetSelectedElement;
    procedure AddElement(const AFileName: string); overload;
    procedure AddElement(const AElement: TSSBElement); overload;
    procedure LoadProject(const AFileName: string);
    procedure SaveProject(const AFileName: string);
    procedure SaveForEngine(const AFileName: string);
    constructor Create(APanel: TPanel; ABackground, ASelected: TImage;
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

procedure TSpriteShapeBuilder.AddElement(const AFileName: string);
var
  vSSBElement: TSSBElement;
begin
  vSSBElement := TSSBElement.Create(FPanel);
  vSSBElement.LoadFromFile(AFileName);

  AddElement(vSSBElement);
end;

procedure TSpriteShapeBuilder.AddElement(const AElement: TSSBElement);
begin
  with AElement do
  begin
    Parent := FPanel;

    OnClick := DoSelectObject;
{    OnMouseDown := DoMouseDown;
    OnMouseUp := DoMouseUp;
    OnMouseMove := DoMouseMove;
    OnMouseWheel := DoZoom;  }
  end;

//  FElements.Add(AElement);
end;

constructor TSpriteShapeBuilder.Create(APanel: TPanel; ABackground,
  ASelected: TImage; AOpenDialog: TOpenDialog);
begin
  //FElements := TNamedList<TSSBElement>.Create;

  FLockPoint := False;
  FIsMouseDown := False;

  FView := TSSBView.Create(APanel, ABackground, ASelected, AOpenDialog);
  FImager := TSSBImagerPresenter.Create(FView);


//  FControllers[sPicture] := TSSBImagerPresenter.Create(FView);
end;

{procedure TSpriteShapeBuilder.AddImage(const AImage: TImage);
begin

end;     }

{procedure TSpriteShapeBuilder.AddImage(const AFileName: string);

end;    }

procedure TSpriteShapeBuilder.Deserialize(const AJson: TJSONObject);
begin

end;

destructor TSpriteShapeBuilder.Destroy;
var
  vSSBElement: TSSBElement;
  vImg: TImage;
begin
 { FModels[sPicture].Free;
  FControllers[sPicture].Free;  }
  FView.Free;

{  for vSSBElement in FElements do
    vSSBElement.Free;
  FElements.Clear;
  FElements.Free; }



  inherited;
end;

procedure TSpriteShapeBuilder.DoAddCircle(ASender: TObject);
begin
  if (FSelectedElement = nil) or (not (FSelectedElement is TSSBElement)) then
    Exit;

  TSSBElement(FSelectedElement).AddCircle;
end;

procedure TSpriteShapeBuilder.DoAddPoly(ASender: TObject);
begin
  if (FSelectedElement = nil) or (not (FSelectedElement is TSSBElement)) then
    Exit;

  TSSBElement(FSelectedElement).AddPoly;
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

//  FView.Model := FModels[Status];

  FPanels[Status].Visible := True;

end;

procedure TSpriteShapeBuilder.DoDeleteObject(ASender: TObject);
begin
 { if FSelectedElement = nil then
    Exit;

  FElements.Delete(FSelectedElement);
  FSelectedElement.Free; }
end;

procedure TSpriteShapeBuilder.DoDeletePicture(ASender: TObject);
begin

end;

procedure TSpriteShapeBuilder.DoEditObject(ASender: TObject);
begin

end;

procedure TSpriteShapeBuilder.DoEditShape(ASender: TObject);
begin

end;

procedure TSpriteShapeBuilder.DoLoadProject(ASender: TObject);
begin
  LoadProject('JSONoutput.txt');
end;

procedure TSpriteShapeBuilder.DoSaveProject(ASender: TObject);
begin
  SaveProject('JSONoutput.txt');
end;

procedure TSpriteShapeBuilder.DoSelectObject(ASender: TObject);
begin
{  if ASender is TSSBElement then
    SelectedElement := TSSBElement(ASender); }
end;

procedure TSpriteShapeBuilder.DoSelectPicture(ASender: TObject);
begin
{  if ASender is TImage then
    SelectedElement := TSSBElement(ASender);   }
end;

function TSpriteShapeBuilder.GetController: TSSBPresenter;
begin
  Result := FControllers[FStatus];
end;

procedure TSpriteShapeBuilder.Init(const AProgForm: TForm);
var
  vDelBtn, vAddCircle, vAddPoly, vSavePrjBtn, vLoadPrjBtn: TCornerButton;
  i: Integer;
  iStatus: TSSBStatus;
begin

//  FView.Init(AProgForm);
  FPanel := TPanel(AProgForm.FindComponent('MainPanel'));
  with FPanel do
  begin
{    OnMouseWheel := FSSBView.DoZoom;
    OnMouseDown := DoMouseDown;
    OnMouseUp := DoMouseUp;
    OnMouseMove := DoMouseMove;   }

    try
      Canvas.BeginScene;
      Canvas.Fill.Color := TAlphaColorRec.Blanchedalmond;
      Canvas.FillRect(FPanel.BoundsRect, 0, 0, [], 1, FMX.Types.TCornerType.Round);
    finally
      Canvas.EndScene;
    end;

  end;

  Status := sPicture;

  FTabsRect[sPicture] := TRectangle(AProgForm.FindComponent('Picture_Rect'));
  FTabsRect[sObject] := TRectangle(AProgForm.FindComponent('Object_Rect'));
  FTabsRect[sShape] := TRectangle(AProgForm.FindComponent('Shape_Rect'));

  FTabsImg[sPicture] := TImage(AProgForm.FindComponent('Picture_Img'));
  FTabsImg[sObject] := TImage(AProgForm.FindComponent('Object_Img'));
  FTabsImg[sShape] := TImage(AProgForm.FindComponent('Shape_Img'));

  for iStatus := Low(TSSBStatus) to High(TSSBStatus) do
  begin
    FTabsImg[iStatus].OnClick := DoChangeStatus;
    FTabsRect[iStatus].OnClick:= DoChangeStatus;
  end;


  FPanels[sPicture] := TLayout(AProgForm.FindComponent('Picture_Inst'));
  FPanels[sObject] := TLayout(AProgForm.FindComponent('Object_Inst'));
  FPanels[sShape] := TLayout(AProgForm.FindComponent('Shape_Inst'));

  //FImageForSelect := TImage(AProgForm.FindComponent('SelectImage'));

  vDelBtn := TCornerButton(AProgForm.FindComponent('DelPictureBtn'));
  vDelBtn.OnClick := DoDeleteObject;

  vSavePrjBtn := TCornerButton(AProgForm.FindComponent('SaveProjectBtn'));
  vSavePrjBtn.OnClick := DoSaveProject;

  vLoadPrjBtn := TCornerButton(AProgForm.FindComponent('LoadProjectBtn'));
  vLoadPrjBtn.OnClick := DoLoadProject;

  vAddCircle := TCornerButton(AProgForm.FindComponent('AddCircleBtn'));
  vAddCircle.OnClick := DoAddCircle;

  vAddPoly := TCornerButton(AProgForm.FindComponent('AddPolyBtn'));
  vAddPoly.OnClick := DoAddPoly;
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
    AddElement(vSSB);
  end;

  vList.Free;
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

procedure TSpriteShapeBuilder.SetSelectedElement(const Value: TSSBElement);
begin
  FSelectedElement := Value;
  //ImageForSelect.Bitmap.Assign(Value.Bitmap);
end;

procedure TSpriteShapeBuilder.SetStatus(const Value: TSSBStatus);
begin
//  FPanels[FStatus].Visible := False;
  FStatus := Value;
//  FPanels[Value].Visible := True;
end;

end.


