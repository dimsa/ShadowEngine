unit uSoLogic;

interface

uses
  uCommonClasses, uSoBasePart, uSoContainer;

type
  TSoLogic = class(TSoBasePart)
  private
    FOnExecute: TNotifyEvent<TSoContainer>;
    procedure EmptyHandler(ASender: TSoContainer);
    procedure SetOnExecute(const Value: TNotifyEvent<TSoContainer>);
  protected
    procedure Execute;
  public
    property OnExecute: TNotifyEvent<TSoContainer> read FOnExecute write SetOnExecute;
  end;

implementation

{ TSoLogic }

procedure TSoLogic.EmptyHandler(ASender: TSoContainer);
begin

end;

procedure TSoLogic.Execute;
begin
  FOnExecute(FSubject);
end;

procedure TSoLogic.SetOnExecute(const Value: TNotifyEvent<TSoContainer>);
begin
  FOnExecute := Value;
end;

end.
