unit uSSBModels;

interface

uses
  System.Generics.Collections, System.Classes,
  FMX.Objects, FMX.StdCtrls, FMX.Controls, System.Types, FMX.Graphics,
  uNamedList, uClasses, uSSBTypes, uMVPFrameWork, uNewFigure;

type

  TItemShapeModel = class(TModel)
  private
    FFigure: TNewFigure;
  public
    property Figure: TNewFigure read FFigure;
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
  public
    property Name: string read FName write FName;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Position: TPoint read FPosition write FPosition;
    property Group: string read FGroup write FGroup;
    property Shapes: TList<TItemShapeModel> read FShapes write FShapes;
    function ToJson: string;
    procedure FromJson(const AJson: string);
    constructor Create(const AUpdateHandler: TNotifyEvent); override;
    destructor Destroy; override;
  end;

  TItemImageModel = class(TModel)
  private
    FOriginalImage: TImage;
    FRect: TRect;
  public
    property OriginalImage: TImage read FOriginalImage write FOriginalImage;
    property Rect: TRect read FRect write FRect;
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
  vModel := TItemObjectModel.Create;
  FElements.Add(vModel);
  Result := vModel;
end;

function TSSBModel.AddImageElement: TItemImageModel;
var
  vModel: TItemImageModel;
begin
  vModel := TItemImageModel.Create;
  FImageElements.Add(vModel);
  Result := vModel;
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

procedure TSSBModel.SetElement(AIndex: Integer; const Value: TItemObjectModel);
begin
  FElements[AIndex] := Value;
end;

procedure TSSBModel.SetImageElement(AIndex: Integer;
  const Value: TItemImageModel);
begin
  FImageElements[AIndex] := Value;
end;

function TSSBModel.ToJson: string;
begin

end;

{ TElement }

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

end.

