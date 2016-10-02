unit uSoSprite;

interface

uses
  System.Types,
  uSoTypes, uNamedList, uE2DRendition, uE2DResource, uSoObject, uEngine2DClasses;

type
  TSoObjectFriend = class(TSoObject);
  // Instead of old TEngine2DSprite it containts not all sprites, but only the sprites it can use.
  TSoSprite = class(TEngine2DRendition)
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
    constructor Create(const ASubject: TSoObject; const AImage: TAnonImage; const AResourceList: TNamedList<TSoSpriteResource>); overload;
    destructor Destroy; override;
  end;

implementation

uses
  uSoProperty;

{ TEngine2DSPrite }

constructor TSoSprite.Create(const ASubject: TSoObject; const AImage: TAnonImage;
  const AResourceList: TNamedList<TSoSpriteResource>);
var
  vProp: TSoProperty;
begin
  inherited Create(ASubject, AImage);
  FResourceList := AResourceList;

  with TSoObjectFriend(ASubject) do begin
    vProp := FProperties.Add('Width');
    vProp.AsDouble := Width;
    vProp.AddOnChangeHandler(OnWidthChanged);
    vProp := FProperties.Add('Height');
    vProp.AsDouble := Height;
    vProp.AddOnChangeHandler(OnHeightChanged);
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

procedure TSoSprite.OnHeightChanged(ASender: TObject);
begin
  if Height <> 0 then
    FSubject.Scale := TSoProperty(ASender).AsDouble / Height
  else
    FSubject.Scale := 0;
end;

procedure TSoSprite.OnWidthChanged(ASender: TObject);
begin
  if Width <> 0 then
    FSubject.Scale := TSoProperty(ASender).AsDouble / Width
  else
    FSubject.Scale := 0;
end;

procedure TSoSprite.Repaint;
begin
  FImage.Bitmap.Canvas.DrawBitmap(
    FResourceList[FResIndex].Picture,
    FResourceList[FResIndex].Rect,
    RectF(
      FSubject.X + FResourceList[FResIndex].WHalf * FSubject.ScaleX * CJustifyPoints[Justify].Left,
      FSubject.Y + FResourceList[FResIndex].HHalf * FSubject.ScaleY * CJustifyPoints[Justify].Top,
      FSubject.X + FResourceList[FResIndex].WHalf * FSubject.ScaleX * CJustifyPoints[Justify].Right,
      FSubject.Y + FResourceList[FResIndex].HHalf * FSubject.ScaleY * CJustifyPoints[Justify].Bottom),
    1,
    True);
end;

procedure TSoSprite.SetResIndex(const Value: Integer);
begin
  FResIndex := Value mod FResourceList.Count;
end;

procedure TSoSprite.SetResName(const Value: string);
begin
  FResIndex := FResourceList.IndexOf(Value);
end;

end.
