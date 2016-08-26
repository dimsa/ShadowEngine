unit uSoAnimator;

interface

uses
  System.SysUtils,
  uEngine2DClasses, uNamedList, uSoAnimation, uSoBaseOperator;
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
    procedure AddFromTemplate(const AAnimationName: string);
  end;

implementation

{ TSoAnimationList }

procedure TSoAnimator.Add(const AItem: TSoAnimation; const AName: string);
var
  vName: string;
begin
  {$I .\Template\uItemAdd.inc}
end;

procedure TSoAnimator.AddFromTemplate(const AAnimationName: string);
begin

end;

procedure TSoAnimator.Execute;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
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
