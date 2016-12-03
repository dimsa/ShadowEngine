unit uSSBModels;

interface

uses
  FMX.Graphics, System.Generics.Collections, System.Classes, {$I 'Utils\DelphiCompatability.inc'}
  System.Math, System.JSON, System.SysUtils, FMX.Types, System.UITypes,
  FMX.Objects, FMX.StdCtrls, FMX.Controls, System.Types, uStreamUtil,
  uNamedList, uClasses, uSSBTypes, uMVPFrameWork, uNewFigure, uIntersectorClasses,
  uSeJsonStrings;

type
  TFigureKind = (fkCircle, fkPoly);

  TItemShapeModel = class(TModel)
  private
    FFigure: TNewFigure;
    FDensity, FFriction, FRestitution: Single; // For the box2d
    FIsSensor: Boolean;
    function GetMaxRadius: Integer;
    function GetFigureKind: TFigureKind;
    procedure SetDensity(const Value: Single);
    procedure SetFriction(const Value: Single);
    procedure SetRestitution(const Value: Single);
    procedure SetIsSensor(const Value: Boolean);
    property Figure: TNewFigure read FFigure;
  public
    procedure Repaint(ABmp: TBitmap; const ALockedPoint: TPointF; const APointIndex: Integer; const AColor: TColor);
    function AsPoly: TPolygon;
    function AsCircle: TCircle;
    property MaxRadius: Integer read GetMaxRadius;
    property Density: Single read FDensity write SetDensity;
    property Friction: Single read FFriction write SetFriction;
    property Restitution: Single read FRestitution write SetRestitution;
    property IsSensor: Boolean read FIsSensor write SetIsSensor;
    function BelongPointLocal(APoint: TPointF): Boolean;
    procedure SetData(const AData: TPolygon); overload;// Трактует данные в зависимости от своего типа
    procedure SetData(const AData: TRectF); overload;// Быстрое задание ректангла
    procedure SetData(const AData: uIntersectorClasses.TCircle); overload;// Трактует данные в зависимости от своего типа
    property FigureKind: TFigureKind read GetFigureKind;
    function AsJson: TJSONObject;
    procedure WriteToStream(AStream: TStreamUtil);
    procedure ReadFromStream(AStream: TStreamUtil);
    constructor CreateCircle(const AUpdateHandler: TNotifyEvent);
    constructor CreatePoly(const AUpdateHandler: TNotifyEvent);
    destructor Destroy; override;
  end;

  TObjectModel = class(TModel)
  private
    FName: string;
    FGroup: string;
    FRes: string;
    FShape: string;
    FCustom: TDictionary<string, Variant>;
    procedure SetGroup(const Value: string);
  public
    property Group: string read FGroup write SetGroup;
    function AsJson: TJSONObject;
    constructor Create(const AUpdateHandler: TNotifyEvent); override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TResourceModel = class(TModel)
  private
    FName: string;
    FWidth: Integer;
    FHeight: Integer;
    FPosition: TPoint;
    FShapes: TList<TItemShapeModel>;
    FShapeAdded: TNotifyEventList;
    FShapeRemoved: TNotifyEventList;
    procedure SetHeight(const Value: Integer);
    procedure SetName(const Value: string);
    procedure SetPosition(const Value: TPoint);
    procedure SetWidth(const Value: Integer);
    procedure SetShapesList(const Value: TList<TItemShapeModel>);
    function GetIsHasShapes: Boolean;
  public
    property Name: string read FName write SetName;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Position: TPoint read FPosition write SetPosition;
    property ShapeAdded: TNotifyEventList read FShapeAdded;
    property ShapeRemoved: TNotifyEventList read FShapeRemoved;
    property IsHasShapes: Boolean read GetIsHasShapes;
    procedure AddShape(AItem: TItemShapeModel);
    procedure RemoveShape(AItem: TItemShapeModel);
    procedure OnAddShapeHandler(ASender: TObject);
    procedure WriteToStream(AStream: TStreamUtil);
    procedure ReadFromStream(AStream: TStreamUtil);
    function AsJson: TJSONObject; // Save Resource
    function AsJsonCollider: TJSONObject; // Saves shapes
    procedure FromJson(const AJson: string);
    constructor Create(const AUpdateHandler: TNotifyEvent); override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TItemImageModel = class(TModel)
  private
    FOriginalImage: TImage;
    FRect: TRect;
    procedure SetOriginalImage(const Value: TImage);
    procedure SetRect(const Value: TRect);
    function GetHeight: Integer;
    function GetPosition: TPoint;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetPosition(const Value: TPoint);
    procedure SetWidth(const Value: Integer);
  public
    property OriginalImage: TImage read FOriginalImage write SetOriginalImage;
    property Rect: TRect read FRect write SetRect;
    property Position: TPoint read GetPosition write SetPosition;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    procedure WriteToStream(AStream: TStreamUtil);
    procedure ReadFromStream(AStream: TStreamUtil);
    constructor Create(const AUpdateHandler: TNotifyEvent); override;
    destructor Destroy; override;
  end;

implementation

{ TElement }

procedure TResourceModel.AddShape(AItem: TItemShapeModel);
begin
  FShapes.Add(AItem);
  FShapeAdded.RaiseEvent(AItem);
end;

function TResourceModel.AsJson: TJSONObject;
var
  i: Integer;
  vObj,vBody: TJSONObject;
  vShapes: TJSONArray;
begin
  vBody := TJSONObject.Create;

  vBody.AddPair(CPosition,
    IntToStr(Self.Position.X) + ',' + IntToStr(Self.Position.Y) + ';' +
    IntToStr(Self.Position.X + Self.Width) + ',' + IntToStr(Self.Position.Y + Self.Height)
  );

  vObj := TJSONObject.Create;
  vObj.AddPair(CName, Self.Name);
  vObj.AddPair(CBody, vBody);
  Result := vObj;
end;

function TResourceModel.AsJsonCollider: TJSONObject;
var
  i: Integer;
  vObj: TJSONObject;
  vShapes: TJSONArray;
  vBody: TJSONObject;
begin
  vObj := TJSONObject.Create;

  vShapes := TJSONArray.Create;
  for i := 0 to FShapes.Count - 1 do
    vShapes.AddElement(FShapes[i].AsJson);

  vBody := TJSONObject.Create;
  vBody.AddPair(CFixtures, vShapes);
  vBody.AddPair(CType, 'Dynamic');

  vObj := TJSONObject.Create;
  vObj.AddPair(CName, Self.Name);
  vObj.AddPair(CBody, vBody);
  Result := vObj;
end;

constructor TResourceModel.Create;
begin
  inherited;

  FShapeAdded := TNotifyEventList.Create;
  FShapeRemoved := TNotifyEventList.Create;
  FShapes := TList<TItemShapeModel>.Create;
end;

constructor TResourceModel.Create(const AUpdateHandler: TNotifyEvent);
begin
  inherited;

  FShapeAdded := TNotifyEventList.Create;
  FShapeRemoved := TNotifyEventList.Create;
  FShapes := TList<TItemShapeModel>.Create;
end;

destructor TResourceModel.Destroy;
var
  i: Integer;
begin
  for i := 0 to FShapes.Count - 1 do
    FShapes[i].Free;

  FShapes.Free;
  FShapeAdded.Free;
  FShapeRemoved.Free;

  inherited;
end;

procedure TResourceModel.FromJson(const AJson: string);
begin
  RaiseUpdateEvent;
end;

function TResourceModel.GetIsHasShapes: Boolean;
begin
  Result := FShapes.Count > 0;
end;

procedure TResourceModel.OnAddShapeHandler(ASender: TObject);
begin
  if ASender is TItemShapeModel then
    AddShape(TItemShapeModel(ASender));
end;

procedure TResourceModel.ReadFromStream(AStream: TStreamUtil);
var
  i: Integer;
  vN: Int64;
  vShape: TItemShapeModel;
begin
  with AStream do
  begin
    ReadStr(CObjectName);
    FName := ReadStr;
    ReadStr(CObjectBody);
    ReadStr(CPosition);
    FPosition.X := ReadInt;
    FPosition.Y := ReadInt;
    ReadStr(CSize);
    FWidth := ReadInt;
    FHeight := ReadInt;

    ReadStr(CObjectFigures);
    vN := ReadInt;
    for i := 0 to vN - 1 do
    begin
      vShape := TItemShapeModel.Create();
      vShape.ReadFromStream(AStream);
      AddShape(vShape);
    end;
    RaiseUpdateEvent;
  end;
end;

procedure TResourceModel.RemoveShape(AItem: TItemShapeModel);
begin
  FShapes.Remove(AItem);
  FShapeRemoved.RaiseEvent(AItem);
  AItem.Free;
end;

procedure TResourceModel.SetHeight(const Value: Integer);
begin
  FHeight := Value;
  RaiseUpdateEvent;
end;

procedure TResourceModel.SetName(const Value: string);
begin
  FName := Value;
  RaiseUpdateEvent;
end;

procedure TResourceModel.SetPosition(const Value: TPoint);
begin
  FPosition := Value;
  RaiseUpdateEvent;
end;

procedure TResourceModel.SetShapesList(const Value: TList<TItemShapeModel>);
begin
  FShapes := Value;
  RaiseUpdateEvent;
end;

procedure TResourceModel.SetWidth(const Value: Integer);
begin
  FWidth := Value;
  RaiseUpdateEvent;
end;

procedure TResourceModel.WriteToStream(AStream: TStreamUtil);
var
  i: Integer;
begin
  with AStream do
  begin
    WriteStr(CObjectName);
    WriteStr(FName);
   { WriteStr('ObjectGroup');
    WriteStr(FGroup);}
    WriteStr(CObjectBody);
    WriteStr(CPosition);
    WriteInt(FPosition.X);
    WriteInt(FPosition.Y);
    WriteStr(CSize);
    WriteInt(FWidth);
    WriteInt(FHeight);

    WriteStr(CObjectFigures);
    WriteInt(FShapes.Count);
    for i := 0 to FShapes.Count - 1 do
      FShapes[i].WriteToStream(AStream);

  end;
end;

{ TItemShapeModel }

function TItemShapeModel.AsCircle: TCircle;
begin
  Result := FFigure.AsCircle;
end;

function TItemShapeModel.AsJson: TJSONObject;
var
  vObj: TJSONObject;
  vType: string;
  vFigure: TNewFigure;
  i: Integer;
  vPoly: TPolygon;
  vArr: TJSONArray;
  vVal: TJSONString;
begin
  vObj := TJSONObject.Create;

  vFigure := Self.FFigure;
  case vFigure.Kind of
    TNewFigure.cfCircle: begin
      vType := CCircle;
      vObj.AddPair(CType, vType);
      vObj.AddPair(CCenter, Round(vFigure.AsCircle.X).ToString() + ',' + Round(vFigure.AsCircle.Y).ToString());
      vObj.AddPair(CRadius, Round(vFigure.AsCircle.Radius).ToString());
      vObj.AddPair(CDensity, FDensity.ToString());
      vObj.AddPair(CFriction, FFriction.ToString());
      vObj.AddPair(CRestitution, FFriction.ToString());
      vObj.AddPair(CIsSensor, FIsSensor.ToString());
    end;
    TNewFigure.cfPoly: begin
     vType := CPoly;
     vObj.AddPair(CType, vType);
     vArr := TJSONArray.Create;
     vPoly := vFigure.AsPoly;
     for i := 0 to High(vPoly) do
     begin
       vVal := TJSONString.Create(Round(vPoly[i].X).ToString() + ',' + Round(vPoly[i].Y).ToString());
       vArr.AddElement(vVal);
     end;
     vObj.AddPair(CPoints, vArr);
     vObj.AddPair(CDensity, FDensity.ToString());
     vObj.AddPair(CFriction, FFriction.ToString());
     vObj.AddPair(CRestitution, FFriction.ToString());
     vObj.AddPair(CIsSensor, FIsSensor.ToString());
    end;
  end;

  Result := vObj;
end;

function TItemShapeModel.AsPoly: TPolygon;
begin
  Result := FFigure.AsPoly;
end;

function TItemShapeModel.BelongPointLocal(APoint: TPointF): Boolean;
begin
  Result := FFigure.BelongPointLocal(APoint);
end;

constructor TItemShapeModel.CreateCircle(const AUpdateHandler: TNotifyEvent);
var
  vCircle: TCircle;
begin
  inherited Create(AUpdateHandler);
  FFigure := TNewFigure.Create(TNewFigure.cfCircle);
  vCircle.X := 0;
  vCircle.Y := 0;
  vCircle.Radius := 25;
  FFigure.SetData(vCircle);
  FDensity := 1;
  FFriction := 0;
  FRestitution := 1;
  FIsSensor := False;
end;

constructor TItemShapeModel.CreatePoly(const AUpdateHandler: TNotifyEvent);
var
  vPoly: TPolygon;
begin
  inherited Create(AUpdateHandler);
  FFigure := TNewFigure.Create(TNewFigure.cfPoly);

  SetLength(vPoly, 3);
{  vPoly[0] := PointF(0, -FItemObjectModel.Height / 4);
  vPoly[1] := PointF(-FItemObjectModel.Width / 4, FItemObjectModel.Height / 4);
  vPoly[2] := PointF(FItemObjectModel.Width / 4, FItemObjectModel.Height / 4);}

  vPoly[0] := PointF(0, -50);
  vPoly[1] := PointF(-50, 50);
  vPoly[2] := PointF(50, 50);

  FFigure.SetData(vPoly);
  FDensity := 1;
  FFriction := 0;
  FRestitution := 1;
  FIsSensor := False;
end;

destructor TItemShapeModel.Destroy;
begin
  FFigure.Free;
  inherited;
end;

function TItemShapeModel.GetFigureKind: TFigureKind;
begin
  if FFigure.Kind = 1 then
    Exit(fkCircle);

  if FFigure.Kind = 2 then
    Exit(fkPoly);

  raise Exception.Create('Unknown figure kind'); 
end;

function TItemShapeModel.GetMaxRadius: Integer;
begin
  Result := Round(FFigure.TempMaxRadius);
end;

procedure TItemShapeModel.ReadFromStream(AStream: TStreamUtil);
var
  i: Integer;
  vPoly: TPolygon;
  vS: String;
  vCircle: TCircle;
  vN: Int64;
begin
  with AStream do
  begin
    ReadStr(CFigureType);
    vS := ReadStr;

    if LowerCase(vS) = 'circle' then
    begin
      FFigure := TNewFigure.Create(TNewFigure.cfCircle);
      ReadStr(CCenter);
      vCircle.X := ReadInt;
      vCircle.Y := ReadInt;
      ReadStr(CRadius);
      vCircle.Radius := ReadInt;
      FFigure.SetData(vCircle);
    end;

    if LowerCase(vS) = 'poly' then
    begin
      FFigure := TNewFigure.Create(TNewFigure.cfPoly);
      vN := ReadInt;
      SetLength(vPoly, vN);

      for i := 0 to High(vPoly) do
      begin
        vPoly[i].X := ReadInt;
        vPoly[i].Y := ReadInt;
      end;
      FFigure.SetData(vPoly);
    end;

    ReadStr(CDensity);
    FDensity := ReadSingle;
    ReadStr(CFriction);
    FFriction := ReadSingle;
    ReadStr(CRestitution);
    FRestitution := ReadSingle;
    ReadStr(CIsSensor);
    FIsSensor := ReadBool;
  end;
  RaiseUpdateEvent;
end;

procedure TItemShapeModel.Repaint(ABmp: TBitmap; const ALockedPoint: TPointF; const APointIndex: Integer; const AColor: TColor);
begin
  ABmp.Canvas.BeginScene();
  FFigure.TempTranslate(PointF(ABmp.Width / 2, ABmp.Height / 2));
  FFigure.Draw(ABmp.Canvas, AColor);

  if APointIndex >= 0 then
    FFigure.DrawPoint(ABmp.Canvas, ALockedPoint, TAlphaColorRec.Red);
  FFigure.Reset;
  ABmp.Canvas.EndScene;
end;

procedure TItemShapeModel.SetData(const AData: TPolygon);
begin
  FFigure.SetData(AData);
  RaiseUpdateEvent;
end;

procedure TItemShapeModel.SetData(const AData: TRectF);
begin
  FFigure.SetData(AData);
  RaiseUpdateEvent;
end;

procedure TItemShapeModel.WriteToStream(AStream: TStreamUtil);
var
  i: Integer;
  vPoly: TPolygon;
begin
  with AStream do
  begin
    WriteStr(CFigureType);
    case FFigure.Kind of
      TNewFigure.cfCircle: begin
        WriteStr(CCircle);
        WriteStr(CCenter);
        WriteInt(Round(FFigure.AsCircle.X));
        WriteInt(Round(FFigure.AsCircle.Y));
        WriteStr(CRadius);
        WriteInt(Round(FFigure.AsCircle.Radius));
      end;
      TNewFigure.cfPoly: begin
        WriteStr(CPoly);
        vPoly := FFigure.AsPoly;
        WriteInt(Length(vPoly));
        for i := 0 to High(vPoly) do
        begin
          WriteInt(Round(vPoly[i].X));
          WriteInt(Round(vPoly[i].Y));
        end;
      end;
    end;
    WriteStr(CDensity);
    WriteSingle(FDensity);
    WriteStr(CFriction);
    WriteSingle(FFriction);
    WriteStr(CRestitution);
    WriteSingle(FRestitution);
    WriteStr(CIsSensor);
    WriteBool(FIsSensor);
  end;
end;

procedure TItemShapeModel.SetData(const AData: uIntersectorClasses.TCircle);
begin
  FFigure.SetData(AData);
  RaiseUpdateEvent;
end;

procedure TItemShapeModel.SetDensity(const Value: Single);
begin
  FDensity := Value;
  RaiseUpdateEvent;
end;

procedure TItemShapeModel.SetFriction(const Value: Single);
begin
  FFriction := Value;
  RaiseUpdateEvent;
end;

procedure TItemShapeModel.SetIsSensor(const Value: Boolean);
begin
  FIsSensor := Value;
  RaiseUpdateEvent;
end;

procedure TItemShapeModel.SetRestitution(const Value: Single);
begin
  FRestitution := Value;
  RaiseUpdateEvent;
end;

constructor TItemImageModel.Create(const AUpdateHandler: TNotifyEvent);
begin
  inherited;
end;

destructor TItemImageModel.Destroy;
begin

  inherited;
end;

function TItemImageModel.GetHeight: Integer;
begin
  Result := FRect.Height;
end;

function TItemImageModel.GetPosition: TPoint;
begin
  Result := FRect.TopLeft;
end;

function TItemImageModel.GetWidth: Integer;
begin
  Result := FRect.Width;
end;

procedure TItemImageModel.ReadFromStream(AStream: TStreamUtil);
var
  vStream: TStream;
  vInt: Int64;
  vPos, vSize: TPoint;
  vBmp: TBitmap;
  vX, vY: Integer;
begin
  with AStream do
  begin
    vInt := ReadInt;
    vX := ReadInt;
    vY := ReadInt;
    vStream := AStream.ReadStream(vInt);
    vStream.Position := 0;

    vBmp := TBitmap.Create(vX,vY);
    vBmp.LoadFromStream(vStream);

    if FOriginalImage = nil then
      FOriginalImage := TImage.Create(nil);

    FOriginalImage.Bitmap.Assign(vBmp);
    FOriginalImage.Width := FOriginalImage.Bitmap.Width;
    FOriginalImage.Height:= FOriginalImage.Bitmap.Height;
    vBmp.Free;
    FreeAndNil(vStream);

    ReadStr(CPosition);
    vPos := Point(ReadInt, ReadInt);
    ReadStr(CSize);
    vSize := Point(ReadInt, ReadInt);
    FRect := System.Types.Rect(vPos.X, vPos.Y, vPos.X + vSize.X, vPos.Y + vSize.Y);

    RaiseUpdateEvent;
  end;

end;

procedure TItemImageModel.SetHeight(const Value: Integer);
begin
  FRect.Height := Value;
  RaiseUpdateEvent;
end;

procedure TItemImageModel.SetOriginalImage(const Value: TImage);
begin
  FOriginalImage := Value;
  RaiseUpdateEvent;
end;

procedure TItemImageModel.SetPosition(const Value: TPoint);
begin
  FRect.SetLocation(Value);
  RaiseUpdateEvent;
end;

procedure TItemImageModel.SetRect(const Value: TRect);
begin
  FRect := Value;
  RaiseUpdateEvent;
end;

procedure TItemImageModel.SetWidth(const Value: Integer);
begin
  FRect.Width := Value;
  RaiseUpdateEvent;
end;

procedure TItemImageModel.WriteToStream(AStream: TStreamUtil);
var
  vStream: TMemoryStream;
  vBmp: TBitmap;
  a: TPixelFormat;
begin
  with AStream do
  begin
    vStream := TMemoryStream.Create;

    vBmp := TBitmap.Create;
    vBmp.Assign(Self.FOriginalImage.Bitmap);
    vBmp.SaveToStream(vStream);
    vBmp.Free;

    vStream.Position := 0;

    WriteInt(vStream.Size);
    WriteInt(vBmp.Width);
    WriteInt(vBmp.Height);
    WriteStream(vStream);

    vStream.Free;

    WriteStr(CPosition);
    WriteInt(Self.Position.X);
    WriteInt(Self.Position.Y);
    WriteStr(CSize);
    WriteInt(Self.Width);
    WriteInt(Self.Height);
  end;
end;

{ TObjectModel }

constructor TObjectModel.Create(const AUpdateHandler: TNotifyEvent);
begin

end;

function TObjectModel.AsJson: TJSONObject;
begin
  Result := TJSONObject.Create;
end;

constructor TObjectModel.Create;
begin

end;

destructor TObjectModel.Destroy;
begin

  inherited;
end;

procedure TObjectModel.SetGroup(const Value: string);
begin
  FGroup := Value;
  RaiseUpdateEvent;
end;

end.

