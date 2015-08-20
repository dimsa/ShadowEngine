unit uEngine2DAnimationList;

interface
uses
  System.SyncObjs,
  uEngine2DClasses, uEngine2DAnimation, uEngine2DUnclickableObject;

type
  TEngine2DAnimationList = class(TEngine2DNamedList<TAnimation>)
  strict private
  public
    // Делает отложенную анимацию, которая начинается не сразу
  //  procedure AddDelayedAnimation(const ATime: Integer; AAnimation: tAnimation);
    procedure ClearForSubject(const ASubject: Pointer);
    procedure RecoverStartForSubject(const ASubject: Pointer);
//    procedure ClearAndRecoverStartForSubject(const ASubject: Pointer); experimental;
    procedure ClealAndRecoverForSubject(const ASubject: Pointer); experimental; // ClearAndRecoverStartForSubject;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  uEngine2D;

{ TEngineAnimationList }

{procedure TEngine2DAnimationList.AddDelayedAnimation(const ATime: Integer;
  AAnimation: tAnimation);
var
  vAni: TAnimation;
begin
  vAni := TAnimation.Create;
end;}

procedure TEngine2DAnimationList.ClealAndRecoverForSubject(
  const ASubject: Pointer);
var
  i, vN: Integer;
  vAni: TAnimation;
begin
  tEngine2d(Parent).Critical.Enter;
  vN := Self.Count - 1;
  for i := vN downto 0 do
    if Items[i].Subject = ASubject then
    begin
      vAni := Items[i];
      if vAni.Setupped then
        vAni.RecoverStart;
      Self.Delete(Items[i]);
      vAni.Free;
    end;
  tEngine2d(Parent).Critical.Leave;
end;

procedure TEngine2DAnimationList.ClearForSubject(
  const ASubject: Pointer);
var
  i, vN: Integer;
  vAni: TAnimation;
begin
  tEngine2d(Parent).Critical.Enter;
  vN := Self.Count - 1;
  for i := vN downto 0 do
    if Items[i].Subject = ASubject then
    begin
      vAni := Items[i];
      Self.Delete(Items[i]);
//      Self.Delete(Items[i]);   l
      vAni.Free;
    end;
  tEngine2d(Parent).Critical.Leave;
end;

constructor TEngine2DAnimationList.Create{(const AParent: Pointer)};
begin
  inherited;

end;

destructor TEngine2DAnimationList.Destroy;
begin

  inherited;
end;

procedure TEngine2DAnimationList.RecoverStartForSubject(
  const ASubject: Pointer);
var
  i, vN: Integer;
  vAni: TAnimation;
begin
  tEngine2d(Parent).Critical.Enter;
  vN := Self.Count - 1;
  for i := vN downto 0 do
    if Items[i].Subject = ASubject then
    begin
      vAni := Items[i];
      if vAni.Setupped then
        vAni.RecoverStart;
    end;
  tEngine2d(Parent).Critical.Leave;
end;

end.









