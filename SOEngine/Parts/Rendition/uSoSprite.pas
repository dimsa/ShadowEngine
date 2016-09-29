unit uSoSprite;

interface

uses
  System.Types,
  uSoTypes, uNamedList, uE2DRendition, uE2DResource, uSoObject, uEngine2DClasses;

type
  // Instead of old TEngine2DSprite it containts not all sprites, but only the sprites it can use.
  TSoSprite = class(TEngine2DRendition)
  private
    FResourceList: TNamedList<TSoSpriteResource>;
    FResIndex: Integer;
    function GetResName: string;
    procedure SetResIndex(const Value: Integer);
    procedure SetResName(const Value: string);
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

{ TEngine2DSPrite }

constructor TSoSprite.Create(const ASubject: TSoObject; const AImage: TAnonImage;
  const AResourceList: TNamedList<TSoSpriteResource>);
begin
  inherited Create(ASubject, AImage);
  FResourceList := AResourceList;
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
