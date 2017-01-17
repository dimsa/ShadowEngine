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
    procedure Add(const AItem: TSoAnimation); override;
    procedure LoadTemplates(const AFileName: string);
    function AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string): TSoAnimation; override;
  end;

implementation

{ TSoAnimationList }

procedure TSoAnimator.Add(const AItem: TSoAnimation);
var
  vName: string;
begin
  {$I .\SoObject\uItemAdd.inc}
end;

function TSoAnimator.AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string): TSoAnimation;
begin

end;

procedure TSoAnimator.Execute;
var
  i: Integer;
  vItem: TObject;
begin
  for i := 0 to FList.Count - 1 do
    if FList.TryGetValue(i, vItem) then
      if TSoAnimation(vItem).Enabled then
        TSoAnimation(vItem).Animate;
end;

procedure TSoAnimator.LoadTemplates(const AFileName: string);
begin

end;

procedure TSoAnimator.OnItemDestroy(ASender: TObject);
begin
  FList.Remove(ASender);
end;

end.
