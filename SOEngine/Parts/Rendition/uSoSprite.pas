unit uSoSprite;

interface

uses
  System.Types, uGeometryClasses,
  uSoTypes, uNamedList, uE2DRendition, uE2DResource, uSoObject, uEngine2DClasses,
  uSoObjectDefaultProperties;

type
  // Instead of old TEngine2DSprite it containts not all sprites, but only the sprites it can use.
  TSoSprite = class(TEngine2DRendition)
  private type
    TSoObjectFriend = class(TSoObject);
  private
    FResourceList: TNamedList<TSoSpriteResource>;
    FResIndex: Integer;
    function GetResName: string;
    procedure SetResIndex(const Value: Integer);
    procedure SetResName(const Value: string);
    procedure OnWidthChanged(ASender: TObject);
    procedure OnHeightChanged(ASender: TObject);
  protected
    function GetHeight: Single; override;
    function GetWidth: Single; override;
  public
    property ResIndex: Integer read FResIndex write SetResIndex;
    property ResName: string read GetResName write SetResName;
    procedure Repaint; override;
    procedure NextFrame;
    procedure PrevFrame;
    constructor Create(const ASubject: TSoObject; const AImage: TAnonImage;
       const AResourceList: TNamedList<TSoSpriteResource>; const APrefix: string = ''); overload;
    destructor Destroy; override;
  end;

implementation

uses
  uSoProperty;

{ TEngine2DSPrite }

constructor TSoSprite.Create(const ASubject: TSoObject; const AImage: TAnonImage;
  const AResourceList: TNamedList<TSoSpriteResource>; const APrefix: string = '');
var
  vProp: TSoProperty;
begin
  inherited Create(ASubject, AImage);
  FResourceList := AResourceList;

//  FSize := TSizeObject.Create;
  RecalculateSize;
  with TSoObjectFriend(ASubject) do begin
    if not FProperties.HasProperty(RenditionRect) then
    begin
      vProp := FProperties.Add(RenditionRect);
      vProp.Obj := FRect;

      if APrefix <> '' then
        FProperties.Add(APrefix + 'Width', vProp);

      vProp.AddOnChangeHandler(OnWidthChanged);
    end;



{    if not FProperties.HasProperty(SummaryWidth) then
    begin
      vProp := FProperties.Add(SummaryWidth);
      vProp.AsDouble := Self.Width;
      if APrefix <> '' then
        FProperties.Add(APrefix + 'Width', vProp);

      vProp.AddOnChangeHandler(OnWidthChanged);
    end;

    if not FProperties.HasProperty(SummaryHeight) then
    begin
      vProp := FProperties.Add(SummaryHeight);
      vProp.AsDouble := Self.Height;
      if APrefix <> '' then
        FProperties.Add(APrefix + 'Height', vProp);

      vProp.AddOnChangeHandler(OnHeightChanged);
    end;                                        }
  end;
end;

destructor TSoSprite.Destroy;
begin
  FResourceList.Free;
  inherited;
end;

function TSoSprite.GetHeight: Single;
begin
  Result := FResourceList[FResIndex].Height;
end;

function TSoSprite.GetResName: string;
begin
  FResourceList.NameOf(FResIndex);
end;

function TSoSprite.GetWidth: Single;
begin
  Result := FResourceList[FResIndex].Width;
end;

procedure TSoSprite.NextFrame;
begin
  ResIndex := FResIndex + 1;
end;

procedure TSoSprite.OnHeightChanged(ASender: TObject);
begin
  if Height <> 0 then
    FSubject.Scale := TSoProperty(ASender).Val<TSizeObject>.Height / Height
  else
    FSubject.Scale := 0;
end;

procedure TSoSprite.OnWidthChanged(ASender: TObject);
begin
  if Width <> 0 then
    FSubject.Scale := TSoProperty(ASender).Val<TSizeObject>.Width / Width
  else
    FSubject.Scale := 0;
end;

procedure TSoSprite.PrevFrame;
begin
  ResIndex := FResIndex - 1;
end;

procedure TSoSprite.Repaint;
begin
  FImage.Bitmap.Canvas.DrawBitmap(
    FResourceList[FResIndex].Picture,
    FResourceList[FResIndex].Rect,
    RectF(
      FSubject.X + FResourceList[FResIndex].WHalf * CJustifyPoints[Justify].Left + FMargin.X,
      FSubject.Y + FResourceList[FResIndex].HHalf * CJustifyPoints[Justify].Top + FMargin.Y,
      FSubject.X + FResourceList[FResIndex].WHalf * CJustifyPoints[Justify].Right + FMargin.X,
      FSubject.Y + FResourceList[FResIndex].HHalf * CJustifyPoints[Justify].Bottom + FMargin.Y),
    1,
    True);
end;

procedure TSoSprite.SetResIndex(const Value: Integer);
begin
  FResIndex := Abs(Value mod FResourceList.Count);
  RecalculateSize;
end;

procedure TSoSprite.SetResName(const Value: string);
begin
  FResIndex := FResourceList.IndexOf(Value);
  RecalculateSize;
end;

end.
