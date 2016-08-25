unit uSoRenderer;

interface

uses
  System.SyncObjs, System.Classes, System.SysUtils,
  uEngine2DClasses, uE2DRendition;

type

  TSoRenderer = class
  private
    FCritical: TCriticalSection;
    FList: TEngine2DNamedList<TEngine2DRendition>;
    FAddedObjects: Integer;
    procedure OnItemDestroy(ASender: TObject);
  public
    procedure Execute; // Render On Tick
    procedure Add(const AItem: TEngine2DRendition; const AName: string = '');
    constructor Create(const ACritical: TCriticalSection);
    destructor Destroy; override;
  end;

implementation

{ TSoRenderer }

procedure TSoRenderer.Add(const AItem: TEngine2DRendition; const AName: string);
var
  l: integer;
  vName: string;
begin
  Inc(FAddedObjects);
  if AName = '' then
    vName := 'genname'+IntToStr(FAddedObjects)
  else
    vName := AName;

  if FList.IsHere(AItem) then
  begin
    raise Exception.Create('You are trying to add Object to Engine that already Exist');
    Exit;
  end;

  AItem.OnDestroy := OnItemDestroy;
  FList.Add(AName, AItem)
end;

constructor TSoRenderer.Create(const ACritical: TCriticalSection);
begin
  FAddedObjects := 0;
  FList := TEngine2DNamedList<TEngine2DRendition>.Create(ACritical);
end;

destructor TSoRenderer.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    FList[i].Free;

  FList.Clear;
  FList.Free;

  inherited;
end;

procedure TSoRenderer.Execute;
begin

end;

procedure TSoRenderer.OnItemDestroy(ASender: TObject);
begin
  FList.Delete(TEngine2DRendition(ASender));
end;

end.
