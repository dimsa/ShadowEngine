unit uSoAnimationList;

interface

uses
  uEngine2DClasses, uNamedList, uSoAnimation;
type
  TSoAnimationTemplate = class

  end;

  TSoAnimationList = class(TEngine2DNamedList<TSoAnimation>)
  private
    FTemplates: TNamedList<TSoAnimationTemplate>;
  public
    procedure LoadTemplates(const AFileName: string);
    procedure AddFromTemplate(const AAnimationName: string);
  end;

implementation

{ TSoAnimationList }

procedure TSoAnimationList.AddFromTemplate(const AAnimationName: string);
begin

end;

procedure TSoAnimationList.LoadTemplates(const AFileName: string);
begin

end;

end.
