unit uFormatterList;

{
  Список форматировщиков. Позволяет сделать меню и т.д.
}

interface

uses
  System.SyncObjs,
  uEngineFormatter, uEngine2DObject, uEngine2DSprite, uEngine2DClasses, uIntersectorClasses;

type
  TFormatterList = class(TEngine2DNamedList<TEngineFormatter>)
  public
    procedure Add(AObject: tEngine2DObject; const AText: String); overload;
    procedure ClearForSubject(AObject: tEngine2DObject);
    procedure ApplyForSubject(AObject: tEngine2DObject);
    function PseudoFormatter(const ASpriteForClone: tSprite; const AText: String): tEngine2DObject;
    function PseudoFormatterPosition(const ASpriteForClone: tSprite; const AText: String): TPosition;
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

end;

destructor TFormatterList.Destroy;
begin

  inherited;
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
