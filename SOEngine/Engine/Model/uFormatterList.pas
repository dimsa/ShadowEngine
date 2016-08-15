unit uFormatterList;

interface

uses
  System.SyncObjs, System.Classes, System.RegularExpressions,
  uEngineFormatter, uEngine2DObject, uEngine2DSprite, uEngine2DClasses,
  uNamedList, uGeometryClasses;

type


  // Список форматировщиков. Позволяет сделать меню и т.д.
  // List of TFormattersm that Engine is used to format menu, maps and etc

  TFormatterList = class(TEngine2DNamedList<TEngineFormatter>)
  private
    FLoadedStyles: TNamedList<string>;
    function GetStyleByName(AName: string): string;
  public
    property StyleByName[AName: string]: string read GetStyleByName;
    procedure LoadSECSS(const AFileName: string);
    procedure ClearForSubject(AObject: tEngine2DObject);
    procedure ApplyForSubject(AObject: tEngine2DObject);
    { TODO : Move it to Engine2D }
//    function PseudoFormatter(const ASpriteForClone: TSprite; const AText: string): tEngine2DObject;
//    function PseudoFormatterPosition(const ASpriteForClone: TSprite; const AText: string): TPosition;
    constructor Create(const ACritical: TCriticalSection); reintroduce; virtual;
    destructor Destroy; override;
  end;

implementation

uses
  uEngine2D;

{ TFormatterList }

procedure TFormatterList.ApplyForSubject(AObject: tEngine2DObject);
var
  i, vN: Integer;
begin
  FCriticalSection.Enter;
  vN := Self.Count - 1;
  for i := 0 to vN do
    if Self[i].Subject = AObject then
    begin
      Self[i].Format;

    end;
  FCriticalSection.Leave;
end;

procedure TFormatterList.ClearForSubject(AObject: tEngine2DObject);
var
  i, vN: Integer;

begin
  FCriticalSection.Enter;
  vN := Self.Count - 1;
  for i := vN downto 0 do
    if Self[i].Subject = AObject then
    begin
      Self.Delete(i);
    end;                      //тут есть вопрос. надо попробовать уничтожить форматтерсы
  FCriticalSection.Leave;
end;

constructor TFormatterList.Create(const ACritical: TCriticalSection);
begin
  inherited Create(ACritical);
  FLoadedStyles := TNamedList<string>.Create;
end;

destructor TFormatterList.Destroy;
begin
  FLoadedStyles.Free;
  inherited;
end;

function TFormatterList.GetStyleByName(AName: string): string;
begin
  if FLoadedStyles.IsHere(AName) then
    Exit(FLoadedStyles[AName]);
  Result := '';
end;

procedure TFormatterList.LoadSECSS(const AFileName: string);
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
      FLoadedStyles.Add(vDirective[0], vDirective[1]);
    end;
  end;
  vFile.Free;
end;

{function TFormatterList.PseudoFormatter(
  const ASpriteForClone: tSprite;
  const AText: String): tEngine2DObject;
var
  vRes: TEngineFormatter;
 // vObj: tSprite;
  vObj: tEngine2DObject;
  vEngine: tEngine2D;
begin
  // Проверить механизм клонирования. Не уверен, что всё правильно
  vEngine := FEngine;
  vObj := vEngine.ShadowObject;
  vEngine.AssignShadowObject(ASpriteForClone);
  vObj.Visible := False;

  vRes := TEngineFormatter.Create(vObj, vEngine);
  vRes.Parent := FEngine;
  vRes.Text := AText;
  vRes.Format;

  Result := vEngine.ShadowObject;// vRes;


  // Не работает, потому что форматтер считает себя по Субъекту. А субхет уже убит
  //vObj.Free;
end;    }

{function TFormatterList.PseudoFormatterPosition(const ASpriteForClone: tSprite;
  const AText: String): TPosition;
var
  vRes: TEngineFormatter;
  vObj: tEngine2DObject;
  vEngine: tEngine2D;
begin
  // Проверить механизм клонирования. Не уверен, что всё правильно

  vEngine := FEngine;
  vObj := vEngine.ShadowObject;
  vEngine.AssignShadowObject(ASpriteForClone);
  vObj.Visible := False;

  vRes := TEngineFormatter.Create(vObj, vEngine);
  vRes.Parent := FEngine;
  vRes.Text := AText;
  vRes.Format;

  Result := vEngine.ShadowObject.Position;
  vRes.Free;
end; }

end.









