unit uFunctionGroup;

interface

uses
  System.SysUtils,
  uParserValue, uExpressionParser;

type
  TFunction = class(TExpression)
  protected
    procedure SetText(const AValue: String); override;
  public
    class function IsFunction(const AValue: String): Boolean;
  end;

  TFunctionType = class of TFunction;

  TSin = class(TFunction)
  public
    function Value: Double; override;
  end;

  TCos = class(TFunction)
  public
    function Value: Double; override;
  end;

  TSqr = class(TFunction)
  public
    function Value: Double; override;
  end;

  TSqrt = class(TFunction)
  public
    function Value: Double; override;
  end;

  const
    CFuncNames: array[0..3] of string = ('sin','cos','sqr','sqrt');
    CFuncNamesCount = 4;
    CFastFuncNames: array[0..3] of TFunctionType = (TSin, TCos, TSqr, TSqrt);

implementation

{ TSin }

function TSin.Value: Double;
begin
  Result := Sin(Inherited);
end;

{ TCos }

function TCos.Value: Double;
begin
  Result := Cos(Inherited);
end;

{ TSqr }

function TSqr.Value: Double;
begin
  Result := Sqr(Inherited);
end;

{ TSqrt }

function TSqrt.Value: Double;
begin
  Result := Sqrt(Inherited);
end;

{ TFunction }

class function TFunction.IsFunction(const AValue: String): Boolean;
var
  vText: string;
  i: Integer;
begin
  vText := LowerCase(AValue);

  for i := 0 to High(CFuncNames) do
    if vText = CFuncNames[i] then
      Exit(True);
  Result := False;
end;

procedure TFunction.SetText(const AValue: String);
var
  vText: string;
begin
  //inherited;
  {FExpression := StringReplace(Text, ' ', '',[rfReplaceAll]);
  FParsed := False;
  DeleteSurfaceBrackets(FExpression);
  if not FParsed then
    ParseAll; }
  FText := AValue;
  FName := LowerCase(AValue);
  FExpression := StringReplace(Text, ' ', '',[rfReplaceAll]);
  FParsed := False;
  vText := Expression;
  Delete(vText, 1, Pos('(', vText));
  Delete(vText, Length(vText), 1);
  FExpression := vText;
end;

end.






