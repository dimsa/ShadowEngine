unit uSoAnimator;

interface

uses
  uEngine2DClasses, uNamedList, uSoAnimation;
type
  TSoAnimationTemplate = class

  end;

  TSoAnimator = class(TEngine2DNamedList<TSoAnimation>)
  private
    FTemplates: TNamedList<TSoAnimationTemplate>;
  public
    procedure LoadTemplates(const AFileName: string);
    procedure AddFromTemplate(const AAnimationName: string);
  end;

implementation

{ TSoAnimationList }

procedure TSoAnimator.AddFromTemplate(const AAnimationName: string);
begin

end;

procedure TSoAnimator.LoadTemplates(const AFileName: string);
begin

end;

end.
