unit uSpriteList;

interface

uses
  uEngine2DClasses, uEngine2DResources, uEngine2DObject, uNamedList;

type
  TObjectsList = class(TEngine2dNamedList<tEngine2DObject>)
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TSpriteList }

constructor TObjectsList.Create;
begin
  inherited;

end;

destructor TObjectsList.Destroy;
begin

  inherited;
end;

end.





