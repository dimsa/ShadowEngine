unit uEngine2DStandardAnimations;

interface

uses
  uGeometryClasses, uEngine2DClasses, uEngine2DAnimation, uEngine2DObject, uEngine2D,
  uEngine2DSprite, uClasses;

type
  TMigrationAnimation = class(TAnimation)
  strict private
    FEndPos: TPosition;
  public
    function Animate: Byte; override;
    procedure Finalize; override;
    procedure RecoverStart; override;
    property EndPos: TPosition read FEndPos write FEndPos;
    constructor Create; override;
  end;

  TMouseDownMigrationAnimation = class(TAnimation)
  private
    FAway: Boolean;
    FPressed: Boolean; // Нажата ли уже кнопка была
    FTempEndPos, FTempStartPos: TPosition;
    FEndPos: TPosition;
  public
    procedure Setup; override;
    function Animate: Byte; override;
    property EndPos: TPosition read FEndPos write FEndPos;
    procedure Finalize; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TSpriteAnimation = class(TAnimation)
  strict private
    FCurSlide: Integer;
    FSlides: TArray<Integer>;
  public
    function Animate: Byte; override;
    procedure Finalize; override;
    property Slides: TArray<Integer> read FSlides write FSlides;
    property CurSlide: Integer read FCurSlide write FCurSlide;
    // Time for one slide is (fulltime / slide count)
    constructor Create; override;
end;

 TOpacityAnimation = class(TAnimation)
  strict private
    FEndOpaque: Single;
    FStartOpaque: Single;
  public
    function Animate: Byte; override;
    procedure Finalize; override;
    procedure RecoverStart; override;
    property EndOpaque: Single read FEndOpaque write FEndOpaque;
    property StartOpaque: Single read FStartOpaque write FStartOpaque;
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
  if (vRes = CAnimationInProcess) then
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

{ TOpaqueAnimation }

function TOpacityAnimation.Animate: Byte;
var
  vSprite: TSprite;
  vRes: Byte;
begin
  vRes := inherited;
  vSprite := Subject;
  if vRes = CAnimationInProcess then
  begin
    vSprite.Opacity := FStartOpaque + (TimePassed) * ((FEndOpaque - FStartOpaque) / TimeTotal);
  end;

  Result := vRes;
end;

constructor TOpacityAnimation.Create;
begin
  inherited;

end;

procedure TOpacityAnimation.Finalize;
var
  vSprite: TSprite;
begin
  inherited;
  vSprite := Subject;
  vSprite.Opacity := FEndOpaque;
end;

procedure TOpacityAnimation.RecoverStart;
begin
  inherited;

end;

{ TMouseDownMigrationAnimation }

function TMouseDownMigrationAnimation.Animate: Byte;
var
  vObject: TEngine2dObject;
  vRes: Byte;
begin
  vRes := inherited;
  vObject := Subject;

  if (TimePassed >= TimeTotal)  then
    TimePassed := TimeTotal;

  if (Status.IsMouseDowned) then
    vRes := CAnimationInProcess;

  if (Status.IsMouseDowned) and (not FPressed) then
  begin
    vRes := CAnimationInProcess;
    FPressed := True;
    FAway := False;
    FTempEndPos := EndPos;
    FTempStartPos := StartPosition;
  end;

  if (not Status.IsMouseDowned) and (FPressed) then
  begin
    FPressed := False;
    FTempEndPos := StartPosition;
    FTempStartPos := EndPos;
    FAway := True;
    TimePassed := TimeTotal - TimePassed;
    vRes := CAnimationInProcess;
  end;

  if vRes = CAnimationInProcess then
  begin
    vObject.x := ((FTempEndPos.X - FTempStartPos.X) / TimeTotal) * (TimePassed) + FTempStartPos.X;
    vObject.y := ((FTempEndPos.Y - FTempStartPos.Y) / TimeTotal) * (TimePassed) + FTempStartPos.Y;
    vObject.Rotate := FTempStartPos.rotate + ((FTempEndPos.rotate - FTempStartPos.rotate) / TimeTotal) * (TimePassed);
    vObject.Scale := FTempStartPos.ScaleX + ((FTempEndPos.ScaleX - FTempStartPos.ScaleX) / TimeTotal) * (TimePassed);
  end;

  Result := vRes;
end;

constructor TMouseDownMigrationAnimation.Create;
begin
  inherited;
  FAway := False;
  FPressed := False;
end;

destructor TMouseDownMigrationAnimation.Destroy;
begin
  Finalize;
  inherited;
end;

procedure TMouseDownMigrationAnimation.Finalize;
var
  vObject: tEngine2DObject;
begin
  inherited;

  vObject := Subject;
  vObject.Position := FTempEndPos;
  FAway := False;
  FPressed := False;
end;

procedure TMouseDownMigrationAnimation.Setup;
begin
  inherited;
  FTempEndPos := EndPos;
  FTempStartPos := StartPosition;
end;

end.
