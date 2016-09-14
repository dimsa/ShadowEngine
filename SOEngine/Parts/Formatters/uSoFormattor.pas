unit uSoFormattor;

interface

uses
  System.SyncObjs, System.Classes, System.SysUtils,
  uSoBaseOperator, uSoFormatter, uNamedList, uSoObject, uSoContainerTypes, uSoBasePart;

type
  TSoFormatterTemplate = class
  end;

  TSoFormattor = class(TSoOperator<TSoFormatter>)
  private
    FTemplates: TNamedList<TSoFormatterTemplate>;
    procedure OnItemDestroy(ASender: TObject);
  public
    procedure LoadTemplates(const AFileName: string);
    procedure Execute;
    procedure Add(const AItem: TSoFormatter; const AName: string = ''); override;
    function AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoFormatter; override;
    constructor Create(const ACritical: TCriticalSection); override;
    destructor Destroy; override;
  end;

implementation

{ TSoFormattor }

procedure TSoFormattor.Add(const AItem: TSoFormatter; const AName: string);
var
  vName: string;
begin
  {$I .\Template\uItemAdd.inc}
end;

function TSoFormattor.AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoFormatter;
begin

end;

constructor TSoFormattor.Create(const ACritical: TCriticalSection);
begin
  inherited;
  FTemplates := TNamedList<TSoFormatterTemplate>.Create;
end;

destructor TSoFormattor.Destroy;
var
  i: Integer;
begin
  for i := 0 to FTemplates.Count - 1 do
    FTemplates[i].Free;

  FTemplates.Free;
  inherited;
end;

procedure TSoFormattor.Execute;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    if FList[i].Enabled then
      FList[i].Format;
end;

procedure TSoFormattor.LoadTemplates(const AFileName: string);
begin

end;

procedure TSoFormattor.OnItemDestroy(ASender: TObject);
begin
  FList.Delete(TSoFormatter(ASender));
end;

end.
