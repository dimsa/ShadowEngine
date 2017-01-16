unit uSoAnimator;

interface

uses
  System.SysUtils, uSoTypes,
  uEngine2DClasses, uNamedList, uSoAnimation, uSoBaseOperator, uSoObject,
  uSoContainerTypes, uSoBasePart;
type
  TSoAnimationTemplate = class

  end;

  TSoAnimator = class(TSoOperator<TSoAnimation>)
  private
    FTemplates: TNamedList<TSoAnimationTemplate>;
    procedure OnItemDestroy(ASender: TObject);
  public
    procedure Execute; // Render On Tick
    procedure Add(const AItem: TSoAnimation; const AName: string = ''); override;
    procedure LoadTemplates(const AFileName: string);
    function AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoAnimation; override;
  end;

implementation

{ TSoAnimationList }

procedure TSoAnimator.Add(const AItem: TSoAnimation; const AName: string);
var
  vName: string;
begin
  {$I .\SoObject\uItemAdd.inc}
end;

function TSoAnimator.AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoAnimation;
begin

end;

procedure TSoAnimator.Execute;
var
  i: Integer;
begin
  with FList.LockPointer do
  begin
    for i := 0 to Count - 1 do
      if TSoAnimation(Item[i]).Enabled then
        TSoAnimation(Item[i]).Animate;
    FList.UnlockPointer;
  end;
end;

procedure TSoAnimator.LoadTemplates(const AFileName: string);
begin

end;

procedure TSoAnimator.OnItemDestroy(ASender: TObject);
begin
  FList.RemoveAllValues(TSoAnimation(ASender));
end;

end.
