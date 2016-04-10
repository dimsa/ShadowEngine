unit uSSBModels;

interface

uses
  System.Generics.Collections, System.Classes, {$I 'Utils\DelphiCompatability.inc'}
  System.Math, System.JSON, System.SysUtils,
  FMX.Objects, FMX.StdCtrls, FMX.Controls, System.Types, FMX.Graphics,
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

    constructor CreateCircle(const AUpdateHandler: TNotifyEvent);
    constructor CreatePoly(const AUpdateHandler: TNotifyEvent);
    destructor Destroy; override;
  end;

  TItemObjectModel = class(TModel)
  private
    FName: string;
    FWidth: Integer;
    FHeight: Integer;
    FPosition: TPoint;
    FGroup: string;
    FShapes: TList<TItemShapeModel>;
    procedure SetGroup(const Value: string);
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
    property Group: string read FGroup write SetGroup;
    property ShapesList: TList<TItemShapeModel> write SetShapesList;
    procedure AddShape(const AShape: TItemShapeModel);
    function AsJson: TJSONObject;
    function ToJson: string;
    procedure FromJson(const AJson: string);
    constructor Create(const AUpdateHandler: TNotifyEvent); override;
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
    property Rect: TRect write SetRect;
    property Position: TPoint read GetPosition write SetPosition;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    constructor Create(const AUpdateHandler: TNotifyEvent); override;
  end;

  TSSBModel = class(TModel)
  private
    FBitmap: TBitmap; // Подложка объекта
    FElements: TList<TItemObjectModel>;
    FImageElements: TList<TItemImageModel>;
    function GetElementCount: Integer;
    function GetElement(AIndex: Integer): TItemObjectModel;
    function GetImageElement(AIndex: Integer): TItemImageModel;
    function GetImageElementCount: Integer;
    procedure SetElement(AIndex: Integer; const Value: TItemObjectModel);
    procedure SetImageElement(AIndex: Integer; const Value: TItemImageModel);
    procedure OnUpdateItemObject(Sender: TObject);
    procedure OnUpdateImageObject(Sender: TObject);
    procedure OnUpdateShapeObject(Sender: TObject);
  public
    function ToJson: string;
    procedure FromJson(const AJson: string);
    property ElementCount: Integer read GetElementCount;
    property ImageElementCount: Integer read GetImageElementCount;
    property Elements[AIndex: Integer]: TItemObjectModel read GetElement write SetElement;
    property ImageElements[AIndex: Integer]: TItemImageModel read GetImageElement write SetImageElement;
    property Image: TBitmap read FBitmap;
    function AddImageElement: TItemImageModel;
    function AddElement: TItemObjectModel;
    constructor Create(const AUpdateHandler: TNotifyEvent); override;
    destructor Destroy; override;
end;

implementation

{ TSSBModel }

function TSSBModel.AddElement: TItemObjectModel;
var
  vModel: TItemObjectModel;
begin
  vModel := TItemObjectModel.Create(OnUpdateItemObject);
  FElements.Add(vModel);
  Result := vModel;
  RaiseUpdateEvent;
end;

function TSSBModel.AddImageElement: TItemImageModel;
var
  vModel: TItemImageModel;
begin
  vModel := TItemImageModel.Create(OnUpdateImageObject);
  FImageElements.Add(vModel);
  Result := vModel;
  RaiseUpdateEvent;
end;

constructor TSSBModel.Create(const AUpdateHandler: TNotifyEvent);
begin
  inherited;
  FBitmap := TBitmap.Create;
  FElements := TList<TItemObjectModel>.Create;
  FImageElements := TList<TItemImageModel>.Create;
end;

destructor TSSBModel.Destroy;
var
  i: Integer;
begin
  FBitmap.Free;

  for i := 0 to FElements.Count - 1 do
    FElements[i].Free;
  FElements.Free;

  for i := 0 to FImageElements.Count - 1 do
    FImageElements[i].Free;
  FImageElements.Free;
end;

procedure TSSBModel.FromJson(const AJson: string);
begin

end;

function TSSBModel.GetElement(AIndex: Integer): TItemObjectModel;
begin
  Result := FElements[AIndex];
end;

function TSSBModel.GetElementCount: Integer;
begin
  Result := FElements.Count;
end;

function TSSBModel.GetImageElement(AIndex: Integer): TItemImageModel;
begin
  Result := FImageElements[AIndex];
end;

function TSSBModel.GetImageElementCount: Integer;
begin
  Result := FImageElements.Count;
end;

procedure TSSBModel.OnUpdateImageObject(Sender: TObject);
begin

end;

procedure TSSBModel.OnUpdateItemObject(Sender: TObject);
begin

end;

procedure TSSBModel.OnUpdateShapeObject(Sender: TObject);
begin

end;

procedure TSSBModel.SetElement(AIndex: Integer; const Value: TItemObjectModel);
begin
  FElements[AIndex] := Value;
  RaiseUpdateEvent;
end;

procedure TSSBModel.SetImageElement(AIndex: Integer;
  const Value: TItemImageModel);
begin
  FImageElements[AIndex] := Value;
  RaiseUpdateEvent;
end;

function TSSBModel.ToJson: string;
var
  vJson: TJSONObject;
  vObjects: TJSONPair;
  vObjArr: TJSONArray;
  i: Integer;
begin
  vJson := TJSONObject.Create;
  vJson.AddPair('ImageFile', 'test.txt');
  vObjArr := TJSONArray.Create;

  for i := 0 to FElements.Count - 1 do
  begin
    vObjArr.AddElement(FElements[i].AsJson);
  end;

  vJson.AddPair('Objects', vObjArr);

  Result := vJson.ToJSON;
end;

{ TElement }

procedure TItemObjectModel.AddShape(const AShape: TItemShapeModel);
begin
  FShapes.Add(AShape);
  RaiseUpdateEvent;
end;

function TItemObjectModel.AsJson: TJSONObject;
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
  vObj.AddPair('Group', Self.Group);
  vObj.AddPair('Body', vBody);

  Result := vObj;
end;

constructor TItemObjectModel.Create(const AUpdateHandler: TNotifyEvent);
begin
  inherited;

  FShapes := TList<TItemShapeModel>.Create;
end;

destructor TItemObjectModel.Destroy;
var
  i: Integer;
begin
  for i := 0 to FShapes.Count - 1 do
    FShapes[i].Free;

  FShapes.Free;

  inherited;
end;

procedure TItemObjectModel.FromJson(const AJson: string);
begin
  RaiseUpdateEvent;
end;

procedure TItemObjectModel.SetGroup(const Value: string);
begin
  FGroup := Value;
  RaiseUpdateEvent;
end;

procedure TItemObjectModel.SetHeight(const Value: Integer);
begin
  FHeight := Value;
  RaiseUpdateEvent;
end;

procedure TItemObjectModel.SetName(const Value: string);
begin
  FName := Value;
  RaiseUpdateEvent;
end;

procedure TItemObjectModel.SetPosition(const Value: TPoint);
begin
  FPosition := Value;
  RaiseUpdateEvent;
end;

procedure TItemObjectModel.SetShapesList(const Value: TList<TItemShapeModel>);
begin
  FShapes := Value;
  RaiseUpdateEvent;
end;

procedure TItemObjectModel.SetWidth(const Value: Integer);
begin
  FWidth := Value;
  RaiseUpdateEvent;
end;

function TItemObjectModel.ToJson: string;
begin

end;

{ TImageElement }

constructor TItemImageModel.Create(const AUpdateHandler: TNotifyEvent);
begin
  inherited;

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

procedure TItemShapeModel.SetData(const AData: TPolygon);
begin
  FFigure.SetData(AData);
end;

procedure TItemShapeModel.SetData(const AData: TRectF);
begin
  FFigure.SetData(AData);
end;

procedure TItemShapeModel.SetData(const AData: uIntersectorClasses.TCircle);
begin
  FFigure.SetData(AData);
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
//  FRect.TopLeft:= Value;

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

end.

