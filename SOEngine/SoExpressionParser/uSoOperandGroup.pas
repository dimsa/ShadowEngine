unit uSoOperandGroup;

interface

uses
  System.Math,
  uSoParserValue;

type
 {Операции сложения, вычитания и т.д.}
  TOperand = class(TValue)
  protected
    FValue1, FValue2: TValue;
  public
    constructor Create(const AValue1, AValue2: TValue); reintroduce; virtual;
  end;

 {Типы операндов}
  TOperands = class of TOperand;

  {Скобки. Набор операндов}
  TAddition = class(TOperand)
  public
    function Value: Double; override;
  end;

  TSubtraction = class(TOperand)
  public
    function Value: Double; override;
  end;

  TMultiplication = class(TOperand)
  public
    function Value: Double; override;
  end;

  TDivision = class(TOperand)
  public
    function Value: Double; override;
  end;

  TMod = class(TOperand)
  public
    function Value: Double; override;
  end;

  TPower = class(TOperand)
  public
    function Value: Double; override;
  end;

  const
    Operands: array[0..5] of string = ('^','mod','/','*','-','+');
    OperandCount = 6;
    OperandChar: array[0..7] of string = ('^','mod','/','*','-','+','(',')');
    OperandCharCount = 8;
    FastOperandClass: array[0..5] of TOperands = (TPower, TMod, TDivision, TMultiplication, TSubtraction, TAddition);

implementation

{ TOperand }

constructor TOperand.Create(const AValue1, AValue2: TValue);
begin
  FValue1 := AValue1;
  FValue2 := AValue2;
end;

{ TAddition }

function TAddition.Value: Double;
begin
  Result := FValue1.Value + FValue2.Value;
end;

{ TSubtraction }

function TSubtraction.Value: Double;
begin
  Result := FValue1.Value - FValue2.Value;
end;

{ TMultiplication }

function TMultiplication.Value: Double;
begin
  Result := FValue1.Value * FValue2.Value;
end;

{ TDivision }

function TDivision.Value: Double;
begin
  Result := FValue1.Value / FValue2.Value;
end;


{ TPower }

function TPower.Value: Double;
begin
  Result := Power(FValue1.Value, FValue2.Value);
end;

{ TMod }

function TMod.Value: Double;
begin
  Result := Round(FValue1.Value) mod round(FValue2.Value);
end;

end.
