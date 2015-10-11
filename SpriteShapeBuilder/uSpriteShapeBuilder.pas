unit uSpriteShapeBuilder;

interface

uses
  System.Generics.Collections, FMX.Objects, FMX.StdCtrls, System.Classes, FMX.Forms,
  FMX.Dialogs, System.SysUtils, System.UITypes, FMX.Types, System.Types,
  System.JSON,
  uSSBElement, uNamedList, uEasyDevice, uSSBFigure, uClasses;

type
  TSpriteShapeBuilder = class(TInterfacedObject, ISerializable)
  private
    FPanel: TPanel;
    FImageForSelect: TImage;
    FElements: TNamedList<TSSBElement>;
    FSelectedElement: TSSBElement;
    FLockPoint: Boolean;
    FIsMouseDown: Boolean;
    FMouseStartPoint, FMouseElementPoint, FElementStartPosition: TPointF;
    procedure DoAddCircle(ASender: TObject);
    procedure DoAddPoly(ASender: TObject);
    procedure DoSelect(ASender: TObject);
    procedure DoDelete(ASender: TObject);
    procedure DoSaveProject(ASender: TObject);
    procedure DoLoadProject(ASender: TObject);

    procedure DoZoom(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure SetSelectedElement(const Value: TSSBElement);
    function Serialize: TJSONObject;
    procedure Deserialize(const AJson: TJSONObject);
  public
    property IsMouseDown: Boolean read FIsMouseDown write FIsMouseDown;
    property Elements: TNamedList<TSSBElement> read FElements write FElements;
//    property Elements[Index: Integer]: read GetElements write SetElements;
    property SelectedElement: TSSBElement read FSelectedElement write SetSelectedElement;
    procedure AddElement(const AFileName: string); overload;
    procedure AddElement(const AElement: TSSBElement); overload;
    procedure LoadProject(const AFileName: string);
    procedure SaveProject(const AFileName: string);
    procedure SaveForEngine(const AFileName: string);
    constructor Create;
    procedure Init(const AProgForm: TForm);
    destructor Destroy; override;
  const
    CPrec = 5;
  end;


implementation

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

    OnClick := DoSelect;
    OnMouseDown := DoMouseDown;
    OnMouseUp := DoMouseUp;
    OnMouseMove := DoMouseMove;
    OnMouseWheel := DoZoom;
  end;

  FElements.Add(AElement);
end;

constructor TSpriteShapeBuilder.Create;
begin
  FElements := TNamedList<TSSBElement>.Create;
  FLockPoint := False;
  FIsMouseDown := False;
end;

procedure TSpriteShapeBuilder.Deserialize(const AJson: TJSONObject);
begin

end;

destructor TSpriteShapeBuilder.Destroy;
var
  vSSBElement: TSSBElement;
begin
  for vSSBElement in FElements do
    vSSBElement.Free;
  FElements.Clear;
  FElements.Free;

  inherited;
end;

procedure TSpriteShapeBuilder.DoAddCircle(ASender: TObject);
begin
  if FSelectedElement = nil then
    Exit;
  FSelectedElement.AddCircle;
end;

procedure TSpriteShapeBuilder.DoAddPoly(ASender: TObject);
begin
  if FSelectedElement = nil then
    Exit;
  FSelectedElement.AddPoly;
end;

procedure TSpriteShapeBuilder.DoDelete(ASender: TObject);
begin
  if FSelectedElement = nil then
    Exit;
  FElements.Delete(FSelectedElement);
  FSelectedElement.Free;
end;

procedure TSpriteShapeBuilder.DoLoadProject(ASender: TObject);
begin
  LoadProject('JSONoutput.txt');
end;

procedure TSpriteShapeBuilder.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  vFigure: TSSBFigure;
  vPoint: TPointF;
begin
  FIsMouseDown := True;
  FMouseStartPoint := MousePos / FPanel.Scale.X;//Point;
  FMouseElementPoint.X := X;
  FMouseElementPoint.Y := Y;
  if Sender is TSSBElement then
  begin
    SelectedElement := TSSBElement(Sender);

    FElementStartPosition := FSelectedElement.Position.Point;

    FMouseElementPoint.X := X;
    FMouseElementPoint.Y := Y;
    vFigure := SelectedElement.FigureByCoord(FMouseElementPoint, True);
    if vFigure <> nil then
    begin
      if vFigure.KeyPointLocal(FMouseElementPoint, vPoint, CPrec*2, True) then
      begin
        FSelectedElement.AddPointToDraw(vPoint, TAlphaColorRec.Red);
        FElementStartPosition := vPoint ;
        FLockPoint := True;
      end;
    end;
   FSelectedElement.Repaint;
  end;

end;

procedure TSpriteShapeBuilder.DoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
var
  i: Integer;
  vX, vY: Integer;
  vFigure: TSSBFigure;
  vPoint: TPointF;
begin
  if Sender = FSelectedElement then
  begin
    if FIsMouseDown then
      with FSelectedElement{TSSBElement(Sender)} do
      begin
        if FLockPoint = False then
        begin
          Position.Point := FElementStartPosition + MousePos / FPanel.Scale.X - FMouseStartPoint;

          for i := 0 to FElements.Count - 1 do
            if FElements[i] <> FSelectedElement then
            begin
              for vX := 0 to 3 do
                for vY := 0 to 3 do
                begin
                  if (Points[vX].X <= FElements[i].Points[vY].X + CPrec) and
                    (Points[vX].X >= FElements[i].Points[vY].X - CPrec) then
                    Points[vX] := PointF(FElements[i].Points[vY].X, Points[vX].Y) ;

                  if (Points[vX].Y <= FElements[i].Points[vY].Y + CPrec) and
                    (Points[vX].Y >= FElements[i].Points[vY].Y - CPrec)   then
                    Points[vX] := PointF(Points[vX].X, FElements[i].Points[vY].Y) ;
                end;
            end;
        end else
        begin
          FSelectedElement.ChangeLockedPoint(
            FElementStartPosition + MousePos / FPanel.Scale.X - FMouseStartPoint);
        end;
      end;

    FMouseElementPoint.X := X;
    FMouseElementPoint.Y := Y;

    vFigure :=SelectedElement.FigureByCoord(FMouseElementPoint);
    if vFigure <> nil then
    begin
      if vFigure.KeyPointLocal(FMouseElementPoint, vPoint, CPrec*2) then
      begin
        FSelectedElement.AddPointToDraw(vPoint, TAlphaColorRec.Red);
      end;
    end;
   FSelectedElement.Repaint;
  end;
end;

procedure TSpriteShapeBuilder.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FIsMouseDown := False;
  FLockPoint := False;

  if FSelectedElement <> Nil then
    FSelectedElement.UnlockPoint;
end;

procedure TSpriteShapeBuilder.DoSaveProject(ASender: TObject);
begin
  SaveProject('JSONoutput.txt');
end;

procedure TSpriteShapeBuilder.DoSelect(ASender: TObject);
begin
  if ASender is TSSBElement then
    SelectedElement := TSSBElement(ASender);
end;

procedure TSpriteShapeBuilder.DoZoom(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  if FPanel.Scale.X + ((WheelDelta / 120) * 0.1) > 0.1 then
  begin
    FPanel.Scale.X := FPanel.Scale.X + ((WheelDelta / 120) * 0.1);
    FPanel.Scale.Y := FPanel.Scale.X;
  end;
end;

procedure TSpriteShapeBuilder.Init(const AProgForm: TForm);
var
  vDelBtn, vAddCircle, vAddPoly, vSavePrjBtn, vLoadPrjBtn: TCornerButton;
begin
  FPanel := TPanel(AProgForm.FindComponent('MainPanel'));
  with FPanel do
  begin
    OnMouseWheel := DoZoom;
    OnMouseDown := DoMouseDown;
    OnMouseUp := DoMouseUp;
    OnMouseMove := DoMouseMove;

    try
      Canvas.BeginScene;
      Canvas.Fill.Color := TAlphaColorRec.Blanchedalmond;
      Canvas.FillRect(FPanel.BoundsRect, 0, 0, [], 1, FMX.Types.TCornerType.Round);
    finally
      Canvas.EndScene;
    end;

  end;

  FImageForSelect := TImage(AProgForm.FindComponent('SelectImage'));

  vDelBtn := TCornerButton(AProgForm.FindComponent('DeleteImageBtn'));
  vDelBtn.OnClick := DoDelete;

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

  for i := 0 to FElements.Count - 1 do
  begin
    vElem := FElements[i].Serialize;
    vArr.AddElement(vElem);
  end;
  vObj.AddPair('Elements', vArr);

  Result := vObj;
end;

procedure TSpriteShapeBuilder.SetSelectedElement(const Value: TSSBElement);
begin
  FSelectedElement := Value;
  FImageForSelect.Bitmap.Assign(Value.Bitmap);
end;

end.


