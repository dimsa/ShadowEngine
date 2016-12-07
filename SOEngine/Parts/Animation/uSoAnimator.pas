unit uSoAnimator;

interface

uses
  System.SysUtils,
  uEngine2DClasses, uNamedList, uSoAnimation, uSoBaseOperator, uSoObject, uSoContainerTypes, uSoBasePart;
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
  {$I .\Template\uItemAdd.inc}
end;

function TSoAnimator.AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoAnimation;
begin

end;

procedure TSoAnimator.Execute;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    if FList[i].Enabled then
      FList[i].Animate;
end;

procedure TSoAnimator.LoadTemplates(const AFileName: string);
begin

end;

procedure TSoAnimator.OnItemDestroy(ASender: TObject);
begin
  FList.Delete(TSoAnimation(ASender));
end;

end.
