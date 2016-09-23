unit uSoFormattor;

interface

uses
  System.SyncObjs, System.Classes, System.SysUtils, System.RegularExpressions,
  uSoBaseOperator, uSoFormatter, uNamedList, uSoObject, uSoContainerTypes, uSoBasePart;

type
  TSoFormatterTemplate = class
  private
    FExpression: string;
  public
    property Expression: string read FExpression;
    constructor Create(const AExpression: string);
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
var
  vReg: TRegEx;
  vFile: TStringList;
  vStrs, vDirective: TArray<string>;
  i: Integer;
begin
  vFile := TStringList.Create;
  vFile.LoadFromFile(AFileName);
  // Убираем переносы строк
  vReg := TRegEx.Create('[\r\n\s\t]*');
  vFile.Text := vReg.Replace(vFile.Text, '');

  // Делим по стилям
  vReg := TRegEx.Create('}');
  vStrs := vReg.Split(vFile.Text);
  // Делим на название стиля и его текст
  vReg := TRegEx.Create('{');
  for i := 0 to Length(vStrs) - 1 do
  begin
    vDirective := vReg.Split(vStrs[i]);
    if Length(vDirective) >= 2 then
    begin
      FTemplates.Add(vDirective[0], TSoFormatterTemplate.Create(vDirective[1]));
    end;
  end;
  vFile.Free;
end;

procedure TSoFormattor.OnItemDestroy(ASender: TObject);
begin
  FList.Delete(TSoFormatter(ASender));
end;

{ TSoFormatterTemplate }

constructor TSoFormatterTemplate.Create(const AExpression: string);
begin
  FExpression := AExpression;
end;

end.
