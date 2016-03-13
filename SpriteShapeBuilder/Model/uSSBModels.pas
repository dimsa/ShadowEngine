unit uSSBModels;

interface

uses
  System.Generics.Collections, System.Classes,
  FMX.Objects, FMX.StdCtrls, FMX.Controls, System.Types, FMX.Graphics,
  uNamedList, uClasses, uSSBTypes, uMVPFrameWork;

type

  TElement = class(TModel)
  private
    FName: string;
    FWidth: Integer;
    FHeight: Integer;
    FPosition: TPoint;
    FGroup: string;
  public
    property Name: string read FName write FName;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Position: TPoint read FPosition write FPosition;
    property Group: string read FGroup write FGroup;
    function ToJson: string;
    procedure FromJson(const AJson: string);
    constructor Create(const AUpdateHandler: TNotifyEvent); override;
  end;

  TImageElement = class(TModel)
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
    FElements: TList<TElement>;
    FImageElements: TList<TImageElement>;
    function GetElementCount: Integer;
    function GetElement(AIndex: Integer): TElement;
    function GetImageElement(AIndex: Integer): TImageElement;
    function GetImageElementCount: Integer;
    procedure SetElement(AIndex: Integer; const Value: TElement);
    procedure SetImageElement(AIndex: Integer; const Value: TImageElement);
  public
    function ToJson: string;
    procedure FromJson(const AJson: string);
    property ElementCount: Integer read GetElementCount;
    property ImageElementCount: Integer read GetImageElementCount;
    property Elements[AIndex: Integer]: TElement read GetElement write SetElement;
    property ImageElements[AIndex: Integer]: TImageElement read GetImageElement write SetImageElement;
    property Image: TBitmap read FBitmap;
    constructor Create(const AUpdateHandler: TNotifyEvent); override;
    destructor Destroy; override;
end;

implementation

{ TSSBModel }

constructor TSSBModel.Create(const AUpdateHandler: TNotifyEvent);
begin
  inherited;
  FBitmap := TBitmap.Create;
  FElements := TList<TElement>.Create;
  FImageElements := TList<TImageElement>.Create;
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

function TSSBModel.GetElement(AIndex: Integer): TElement;
begin
  Result := FElements[AIndex];
end;

function TSSBModel.GetElementCount: Integer;
begin
  Result := FElements.Count;
end;

function TSSBModel.GetImageElement(AIndex: Integer): TImageElement;
begin
  Result := FImageElements[AIndex];
end;

function TSSBModel.GetImageElementCount: Integer;
begin
  Result := FImageElements.Count;
end;

procedure TSSBModel.SetElement(AIndex: Integer; const Value: TElement);
begin
  FElements[AIndex] := Value;
end;

procedure TSSBModel.SetImageElement(AIndex: Integer;
  const Value: TImageElement);
begin
  FImageElements[AIndex] := Value;
end;

function TSSBModel.ToJson: string;
begin

end;

{ TElement }

constructor TElement.Create(const AUpdateHandler: TNotifyEvent);
begin
  inherited;
end;

procedure TElement.FromJson(const AJson: string);
begin

end;

function TElement.ToJson: string;
begin

end;

{ TImageElement }

constructor TImageElement.Create(const AUpdateHandler: TNotifyEvent);
begin
  inherited;

end;

end.

