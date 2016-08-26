// This class contains unit renditions that will be repainted on eash tick.
unit uSoRenderer;

interface

uses
  System.SyncObjs, System.Classes, System.SysUtils,
  uEngine2DClasses, uE2DRendition, uSoBaseOperator;

type

  TSoRenderer = class(TSoOperator<TEngine2DRendition>)
  private
    procedure OnItemDestroy(ASender: TObject);
  public
    procedure Execute; // Render On Tick
    procedure Add(const AItem: TEngine2DRendition; const AName: string = ''); override;
  end;

implementation

{ TSoRenderer }

procedure TSoRenderer.Add(const AItem: TEngine2DRendition; const AName: string);
var
  vName: string;
begin
  {$I .\Template\uItemAdd.inc}
end;

procedure TSoRenderer.Execute;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    FList[i].Repaint;
end;

procedure TSoRenderer.OnItemDestroy(ASender: TObject);
begin
  FList.Delete(TEngine2DRendition(ASender));
end;

end.
