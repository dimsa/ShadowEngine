// This class contains unit renditions that will be repainted on eash tick.
unit uSoRenderer;

interface

uses
  System.SyncObjs, System.Classes, System.SysUtils,
  uEngine2DClasses, uE2DRendition, uSoBaseOperator, uSoContainer;

type

  TSoRenderer = class(TSoOperator<TEngine2DRendition>)
  private
    FImage: TAnonImage;
    procedure OnItemDestroy(ASender: TObject);
  public
    constructor Create(const ACritical: TCriticalSection; const AImage: TAnonImage);
    procedure Execute; // Render On Tick
    procedure Add(const AItem: TEngine2DRendition; const AName: string = ''); override;
    function AddFromTemplate(const ASubject: TSoContainer; const ATemplateName: string): TEngine2DRendition;
  end;

implementation

{ TSoRenderer }

procedure TSoRenderer.Add(const AItem: TEngine2DRendition; const AName: string);
var
  vName: string;
begin
  {$I .\Template\uItemAdd.inc}
end;


function TSoRenderer.AddFromTemplate(const ASubject: TSoContainer;
  const ATemplateName: string): TEngine2DRendition;
begin
  Result := TEngine2DRendition.Create(ASubject, FImage);
end;

constructor TSoRenderer.Create(const ACritical: TCriticalSection;
  const AImage: TAnonImage);
begin
  inherited Create(ACritical);
  FImage := AImage;
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
