unit uSoLogic;

interface

uses
  uCommonClasses, uSoBasePart, uSoContainer;

type
  TSoLogic = class(TSoBasePart)
  private
    FOnExecute: TNotifyEvent<TSoObject>;
    procedure EmptyHandler(ASender: TSoObject);
    procedure SetOnExecute(const Value: TNotifyEvent<TSoObject>);
  protected
    procedure Execute;
  public
    property OnExecute: TNotifyEvent<TSoObject> read FOnExecute write SetOnExecute;
  end;

implementation

{ TSoLogic }

procedure TSoLogic.EmptyHandler(ASender: TSoObject);
begin

end;

procedure TSoLogic.Execute;
begin
  FOnExecute(FSubject);
end;

procedure TSoLogic.SetOnExecute(const Value: TNotifyEvent<TSoObject>);
begin
  FOnExecute := Value;
end;

end.
