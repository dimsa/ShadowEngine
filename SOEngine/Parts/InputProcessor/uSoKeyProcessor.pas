unit uSoKeyProcessor;

interface

uses
  System.SyncObjs, System.Classes, System.SysUtils,
  uSoBaseOperator, uSoKeyHandler, uSoContainerTypes, uSoBasePart;

type
  TSoKeyHandlerFriend = class(TSoKeyHandler);

  TSoKeyProcessor = class(TSoOperator<TSoKeyHandler>)
  private
    procedure OnItemDestroy(ASender: TObject);
  public
    procedure ExecuteKeyUp(Key: Word; KeyChar: Char; Shift: TShiftState); // Process key on tick
    procedure ExecuteKeyDown(Key: Word; KeyChar: Char; Shift: TShiftState); // Process key on tick
    procedure Add(const AItem: TSoKeyHandler; const AName: string = ''); override;
    constructor Create(const ACritical: TCriticalSection); override;
  end;

implementation

{ TSoKeyProcessor }

procedure TSoKeyProcessor.Add(const AItem: TSoKeyHandler; const AName: string);
var
  vName: string;
begin
  {$I .\Template\uItemAdd.inc}
end;

constructor TSoKeyProcessor.Create(const ACritical: TCriticalSection);
begin
  inherited;
end;

procedure TSoKeyProcessor.ExecuteKeyDown(Key: Word; KeyChar: Char; Shift: TShiftState);
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TSoKeyHandlerFriend(FList[i]).KeyDown(Key, KeyChar, Shift);
end;

procedure TSoKeyProcessor.ExecuteKeyUp(Key: Word; KeyChar: Char; Shift: TShiftState);
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TSoKeyHandlerFriend(FList[i]).KeyUp(Key, KeyChar, Shift);
end;

procedure TSoKeyProcessor.OnItemDestroy(ASender: TObject);
begin
  FList.Delete(TSoKeyHandler(ASender));
end;

end.


