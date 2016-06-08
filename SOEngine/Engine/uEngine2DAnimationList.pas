unit uEngine2DAnimationList;

interface
uses
  System.SyncObjs,
  uEngine2DClasses, uEngine2DAnimation, uEngine2DUnclickableObject;

type
  TEngine2DAnimationList = class(TEngine2DNamedList<TAnimation>)
  strict private
  public
  //  procedure AddDelayedAnimation(const ATime: Integer; AAnimation: tAnimation); // Adding of Delayed Animation
    procedure ClearForSubject(const ASubject: Pointer);
    procedure RecoverStartForSubject(const ASubject: Pointer);
    procedure ClearAndRecoverForSubject(const ASubject: Pointer); experimental; // ClearAndRecoverStartForSubject;
    destructor Destroy; override;
  end;

implementation

uses
  uEngine2D;

{ TEngineAnimationList }

procedure TEngine2DAnimationList.ClearAndRecoverForSubject(
  const ASubject: Pointer);
var
  i, vN: Integer;
  vAni: TAnimation;
begin
  FCriticalSection.Enter;
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
  FCriticalSection.Leave;
end;

procedure TEngine2DAnimationList.ClearForSubject(
  const ASubject: Pointer);
var
  i, vN: Integer;
  vAni: TAnimation;
begin
  FCriticalSection.Enter;
  vN := Self.Count - 1;
  for i := vN downto 0 do
    if Items[i].Subject = ASubject then
    begin
      vAni := Items[i];
      Self.Delete(Items[i]);
//      Self.Delete(Items[i]);   l
      vAni.Free;
    end;
  FCriticalSection.Leave;
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
  FCriticalSection.Enter;
  vN := Self.Count - 1;
  for i := vN downto 0 do
    if Items[i].Subject = ASubject then
    begin
      vAni := Items[i];
      if vAni.Setupped then
        vAni.RecoverStart;
    end;
  FCriticalSection.Leave;
end;

end.









