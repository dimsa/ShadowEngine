unit uSpriteList;

interface

uses
  uEngine2DClasses, uEngine2DResources, uEngine2DObject, uNamedList;

type
  TSpriteList = class(TEngine2dNamedList<tEngine2DObject>)
  strict private
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TSpriteList }

constructor TSpriteList.Create;
begin
  inherited;

end;

destructor TSpriteList.Destroy;
begin

  inherited;
end;

end.




