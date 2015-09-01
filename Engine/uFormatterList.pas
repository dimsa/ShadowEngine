unit uFormatterList;

{
  Список форматировщиков. Позволяет сделать меню и т.д.
}

interface

uses
  System.SyncObjs, System.Classes, System.RegularExpressions,
  uEngineFormatter, uEngine2DObject, uEngine2DSprite, uEngine2DClasses,
  uNamedList, uIntersectorClasses;

type
  TFormatterList = class(TEngine2DNamedList<TEngineFormatter>)
  private
    FLoadedStyles: TNamedList<string>;
  public
    procedure Add(AObject: tEngine2DObject; const AText: string); overload;
    procedure LoadSECSS(const AFileName: string);
    procedure ClearForSubject(AObject: tEngine2DObject);
    procedure ApplyForSubject(AObject: tEngine2DObject);
    function PseudoFormatter(const ASpriteForClone: TSprite; const AText: string): tEngine2DObject;
    function PseudoFormatterPosition(const ASpriteForClone: TSprite; const AText: string): TPosition;
    constructor Create{(const AParent: Pointer)}; override;
    destructor Destroy; override;
  end;

implementation

uses
  uEngine2D;

{ TFormatterList }

procedure TFormatterList.Add(AObject: tEngine2DObject; const AText: String);
var
  vTmp: TEngineFormatter;
begin
  tEngine2d(Parent).Critical.Enter;
  vTmp := TEngineFormatter.Create(AObject);
  vTmp.Parent := Parent;
  vTmp.Text := AText;
  Self.Add(vTmp);
  tEngine2d(Parent).Critical.Leave;
end;

procedure TFormatterList.ApplyForSubject(AObject: tEngine2DObject);
var
  i, vN: Integer;
begin
  tEngine2d(Parent).Critical.Enter;
  vN := Self.Count - 1;
  for i := 0 to vN do
    if Self[i].Subject = AObject then
    begin
      Self[i].Format;

    end;
  tEngine2d(Parent).Critical.Leave;
end;

procedure TFormatterList.ClearForSubject(AObject: tEngine2DObject);
var
  i, vN: Integer;

begin
  tEngine2d(Parent).Critical.Enter;
  vN := Self.Count - 1;
  for i := vN downto 0 do
    if Self[i].Subject = AObject then
    begin
      Self.Delete(i);
    end;                      //тут есть вопрос. надо попробовать уничтожить форматтерсы
  tEngine2d(Parent).Critical.Leave;
end;

constructor TFormatterList.Create{(const AParent: Pointer)};
begin
  inherited;
  FLoadedStyles := TNamedList<string>.Create;
end;

destructor TFormatterList.Destroy;
begin
  FLoadedStyles.Free;
  inherited;
end;

procedure TFormatterList.LoadSECSS(const AFileName: string);
var
  vReg: TRegEx;
  vFile: TStringList;
  vStrs, vDirective: TArray<string>;
  i: Integer;
  vS: string;
  vMatches: TMatchCollection;
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
    if Length(vStrs) >= 2 then
      FLoadedStyles.Add(vDirective[0], vDirective[1]);
  end;
  vFile.Free;
end;

function TFormatterList.PseudoFormatter(
  const ASpriteForClone: tSprite;
  const AText: String): tEngine2DObject;
var
  vRes: TEngineFormatter;
 // vObj: tSprite;
  vObj: tEngine2DObject;
  vEngine: tEngine2D;
begin
  // Проверить механизм клонирования. Не уверен, что всё правильно
{  vObj := ASpriteForClone.Clone;
  vObj.Visible := False;

  vEngine := Self.Parent;   }


  //vEngine.addObject(vObj);

  vEngine := Self.Parent;
  vObj := vEngine.ShadowObject;
  vEngine.AssignShadowObject(ASpriteForClone);
  vObj.Visible := False;

  vRes := TEngineFormatter.Create(vObj);
  vRes.Parent := Self.Parent;
//  vRes.
  vRes.Text := AText;
  vRes.Format;
 // vEngine.FormatterList.Add(vRes);
  Result := vEngine.ShadowObject;// vRes;
//  vEngine.deleteObject(vObj);

  // Не работает, потому что форматтер считает себя по Субъекту. А субхет уже убит
  //vObj.Free;
end;

function TFormatterList.PseudoFormatterPosition(const ASpriteForClone: tSprite;
  const AText: String): TPosition;
var
  vRes: TEngineFormatter;
  vObj: tEngine2DObject;
  vEngine: tEngine2D;
begin
  // Проверить механизм клонирования. Не уверен, что всё правильно


  vEngine := Self.Parent;
  vObj := vEngine.ShadowObject;
  vEngine.AssignShadowObject(ASpriteForClone);
  vObj.Visible := False;

  vRes := TEngineFormatter.Create(vObj);
  vRes.Parent := Self.Parent;
  vRes.Text := AText;
  vRes.Format;

  Result := vEngine.ShadowObject.Position;
  vRes.Free;
end;

end.








