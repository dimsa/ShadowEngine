unit uEngine2DStandardAnimations;

interface

uses
  uEngine2DClasses, uEngine2DAnimation, uEngine2DObject, uEngine2D;

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
 {  else
  begin
    vSprite.Position := FEndPos;
  end;  }

  Result := vRes;
 { Result := CAnimationInProcess;
  vSprite := tEngine2DObject(Subject);
  vEngine := Parent;

  if TimePassed < TimeTotal then
  begin
    TimePassed := TimePassed + Round((1000 / vEngine.EngineThread.FPS));

  end else
  begin
    vSprite.Position := FEndPos;
    Result := Inherited;
  end;  }
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
{  vEngine := Parent;
  TimePassed := TimePassed + Round((1000 /  vEngine.EngineThread.FPS));

  if TimePassed < TimeTotal then
  begin
    Result := CAnimationInProcess;
  end else
    Result := Inherited;  }
end;

end.
