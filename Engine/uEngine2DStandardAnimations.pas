unit uEngine2DStandardAnimations;

interface

uses
  uIntersectorClasses,
  uEngine2DClasses, uEngine2DAnimation, uEngine2DObject, uEngine2D,
  uEngine2DSprite;

type
  TMigrationAnimation = class(TAnimation)
  strict private
    FEndPos: tPosition;
  public
    function Animate: Byte; override;
    procedure Finalize; override;
    procedure RecoverStart; override;
    property EndPos: TPosition read FEndPos write FEndPos;
    constructor Create; override;
end;

  TSpriteAnimation = class(TAnimation)
  strict private
    FCurSlide: Integer;
    FSprite: tEngine2DObject;
    FSlides: tIntArray;
  public
    function Animate: Byte; override;
    procedure Finalize; override;
    procedure RecoverStart; override;
    property Slides: tIntArray read FSlides write FSlides;
    property CurSlide: Integer read FCurSlide write FCurSlide;
    // Time for one slide is (fulltime / slide count)
    constructor Create; override;
end;

  TEmptyAnimation = class(TAnimation)
  public
    function Animate: Byte; override;
  end;

implementation


{ TMigrationAnimation }

function TMigrationAnimation.Animate: Byte;
var
  vSprite: TEngine2dObject;
  vRes: Byte;
begin
  vRes := inherited;
  vSprite := Subject;
  if vRes = CAnimationInProcess then
  begin
    vSprite.x := ((FEndPos.X - StartPosition.X) / TimeTotal) * (TimePassed) + StartPosition.X;
    vSprite.y := ((FEndPos.Y - StartPosition.Y) / TimeTotal) * (TimePassed) + StartPosition.Y;
    vSprite.Rotate := StartPosition.rotate + ((FEndPos.rotate - StartPosition.rotate) / TimeTotal) * (TimePassed);
    vSprite.Scale := StartPosition.ScaleX + ((FEndPos.ScaleX - StartPosition.ScaleX) / TimeTotal) * (TimePassed);
  end;

  Result := vRes;
end;

constructor TMigrationAnimation.Create;
begin
  inherited;

end;

procedure TMigrationAnimation.Finalize;
var
  vSprite: tEngine2DObject;
begin
  inherited;
  vSprite := Subject;
  vSprite.Position := FEndPos;
end;

procedure TMigrationAnimation.RecoverStart;
begin
  inherited;

end;

{ TEmptyAnimation }

function TEmptyAnimation.Animate: Byte;
begin
  Result := inherited;
end;

{ TSpriteAnimation }

function TSpriteAnimation.Animate: Byte;
var
  vSprite: TSprite;
  vRes: Byte;
begin
  vRes := inherited;
  vSprite := Subject;
  if vRes = CAnimationInProcess then
  begin
    vSprite.CurRes :=
      FSlides[Trunc((TimePassed / TimeTotal) * (Length(FSlides) - 1))];
  end;

  Result := vRes;
end;

constructor TSpriteAnimation.Create;
begin
  inherited;

end;

procedure TSpriteAnimation.Finalize;
var
  vSprite: TSprite;
begin
  inherited;
  vSprite := Subject;
  vSprite.CurRes := FSlides[High(FSlides)];
end;

procedure TSpriteAnimation.RecoverStart;
begin
  inherited;

end;

end.
