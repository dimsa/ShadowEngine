unit uSSBModels;

interface

uses
  FMX.Graphics, System.Generics.Collections, System.Classes, {$I 'Utils\DelphiCompatability.inc'}
  System.Math, System.JSON, System.SysUtils, FMX.Types,
  FMX.Objects, FMX.StdCtrls, FMX.Controls, System.Types, uStreamUtil,
  uNamedList, uClasses, uSSBTypes, uMVPFrameWork, uNewFigure, uIntersectorClasses;

type

  TItemShapeModel = class(TModel)
  private
    FFigure: TNewFigure;
    function GetMaxRadius: Integer;
  public
    property Figure: TNewFigure read FFigure;
    property MaxRadius: Integer read GetMaxRadius;
    procedure SetData(const AData: TPolygon); overload;// Трактует данные в зависимости от своего типа
    procedure SetData(const AData: TRectF); overload;// Быстрое задание ректангла
    procedure SetData(const AData: uIntersectorClasses.TCircle); overload;// Трактует данные в зависимости от своего типа
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
  public
    property Name: string read FName write SetName;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Position: TPoint read FPosition write SetPosition;
//    property ShapesList: TList<TItemShapeModel> read FShapes;// write SetShapesList;
    property ShapeAdded: TNotifyEventList read FShapeAdded;
    property ShapeRemoved: TNotifyEventList read FShapeRemoved;
    procedure AddShape(AItem: TItemShapeModel);
    procedure RemoveShape(AItem: TItemShapeModel);
    procedure OnAddShapeHandler(ASender: TObject);
    procedure DelShape(const AShape: TItemShapeModel);
    procedure WriteToStream(AStream: TStreamUtil);
    procedure ReadFromStream(AStream: TStreamUtil);
    function AsJson: TJSONObject;
    function ToJson: string;
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

  vBody.AddPair('Position',
    IntToStr(Self.Position.X) + ',' + IntToStr(Self.Position.Y) + ';' +
    IntToStr(Self.Position.X + Self.Width) + ',' + IntToStr(Self.Position.Y + Self.Height)
  );

  vShapes := TJSONArray.Create;
  for i := 0 to FShapes.Count - 1 do
    vShapes.AddElement(FShapes[i].AsJson);

  vBody.AddPair('Figures', vShapes);

  vObj := TJSONObject.Create;
  vObj.AddPair('Name', Self.Name);
  vObj.AddPair('Body', vBody);
  vObj.AddPair('Body', 'fgg');
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

procedure TResourceModel.DelShape(const AShape: TItemShapeModel);
begin
  FShapes.Remove(AShape);
  AShape.Free;
  RaiseUpdateEvent;
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
    ReadStr('ObjectName');
    FName := ReadStr;
{    ReadStr('ObjectGroup');
    FGroup := ReadStr; }
    ReadStr('ObjectBody');
    ReadStr('Position');
    FPosition.X := ReadInt;
    FPosition.Y := ReadInt;
    ReadStr('Size');
    FWidth := ReadInt;
    FHeight := ReadInt;

    ReadStr('ObjectFigures');
    vN := ReadInt;
    for i := 0 to vN - 1 do
    begin
      vShape := TItemShapeModel.Create();
      vShape.ReadFromStream(AStream);
      AddShape(vShape);
//      FShapes.Add(vShape);
    end;
    RaiseUpdateEvent;
  end;
end;

procedure TResourceModel.RemoveShape(AItem: TItemShapeModel);
begin
  FShapes.Remove(AItem);
  FShapeRemoved.RaiseEvent(AItem);
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

function TResourceModel.ToJson: string;
begin

end;

procedure TResourceModel.WriteToStream(AStream: TStreamUtil);
var
  i: Integer;
begin
  with AStream do
  begin
    WriteStr('ObjectName');
    WriteStr(FName);
   { WriteStr('ObjectGroup');
    WriteStr(FGroup);}
    WriteStr('ObjectBody');
    WriteStr('Position');
    WriteInt(FPosition.X);
    WriteInt(FPosition.Y);
    WriteStr('Size');
    WriteInt(FWidth);
    WriteInt(FHeight);

    WriteStr('ObjectFigures');
    WriteInt(FShapes.Count);
    for i := 0 to FShapes.Count - 1 do
      FShapes[i].WriteToStream(AStream);

  end;
end;

{ TItemShapeModel }

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
      vType := 'Circle';
      vObj.AddPair('Type', vType);
      vObj.AddPair('Center', Round(vFigure.AsCircle.X).ToString() + ',' + Round(vFigure.AsCircle.Y).ToString());
      vObj.AddPair('Radius', Round(vFigure.AsCircle.Radius).ToString());
    end;
    TNewFigure.cfPoly: begin
     vType := 'Poly';
     vObj.AddPair('Type', vType);
     vArr := TJSONArray.Create;
     vPoly := vFigure.AsPoly;
     for i := 0 to High(vPoly) do
     begin
       vVal := TJSONString.Create(Round(vPoly[i].X).ToString() + ',' + Round(vPoly[i].Y).ToString());
       vArr.AddElement(vVal);
     end;
     vObj.AddPair('Points', vArr);
    end;
  end;

  Result := vObj;
end;

constructor TItemShapeModel.CreateCircle(const AUpdateHandler: TNotifyEvent);
begin
  inherited Create(AUpdateHandler);
  FFigure := TNewFigure.Create(TNewFigure.cfCircle);
end;

constructor TItemShapeModel.CreatePoly(const AUpdateHandler: TNotifyEvent);
begin
  inherited Create(AUpdateHandler);
  FFigure := TNewFigure.Create(TNewFigure.cfPoly);
end;

destructor TItemShapeModel.Destroy;
begin
  FFigure.Free;
  inherited;
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
    ReadStr('FigureType');
    vS := ReadStr;

    if LowerCase(vS) = 'circle' then
    begin
      FFigure := TNewFigure.Create(TNewFigure.cfCircle);
      ReadStr('Center');
      vCircle.X := ReadInt;
      vCircle.Y := ReadInt;
      ReadStr('Radius');
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
  end;
  RaiseUpdateEvent;
end;

procedure TItemShapeModel.SetData(const AData: TPolygon);
begin
  FFigure.SetData(AData);
end;

procedure TItemShapeModel.SetData(const AData: TRectF);
begin
  FFigure.SetData(AData);
end;

procedure TItemShapeModel.WriteToStream(AStream: TStreamUtil);
var
  i: Integer;
  vPoly: TPolygon;
begin
  with AStream do
  begin
    WriteStr('FigureType');
    case FFigure.Kind of
      TNewFigure.cfCircle: begin
        WriteStr('Circle');
        WriteStr('Center');
        WriteInt(Round(FFigure.AsCircle.X));
        WriteInt(Round(FFigure.AsCircle.Y));
        WriteStr('Radius');
        WriteInt(Round(FFigure.AsCircle.Radius));
      end;
      TNewFigure.cfPoly: begin
        WriteStr('Poly');
        vPoly := FFigure.AsPoly;
        WriteInt(Length(vPoly));
        for i := 0 to High(vPoly) do
        begin
          WriteInt(Round(vPoly[i].X));
          WriteInt(Round(vPoly[i].Y));
        end;
      end;
    end;
  end;
end;

procedure TItemShapeModel.SetData(const AData: uIntersectorClasses.TCircle);
begin
  FFigure.SetData(AData);
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

    ReadStr('Position');
    vPos := Point(ReadInt, ReadInt);
    ReadStr('Size');
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

    WriteStr('Position');
    WriteInt(Self.Position.X);
    WriteInt(Self.Position.Y);
    WriteStr('Size');
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

