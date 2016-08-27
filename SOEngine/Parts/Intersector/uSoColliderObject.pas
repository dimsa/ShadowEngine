unit uSoColliderObject;

interface

uses
  uCommonClasses, uSoBasePart, uSoContainer;

type
  TSoColliderObj = class(TSoBasePart)
  private
    FOnCollide: TEvent<TSoContainer>;
    procedure EmptyHandler(ASender: TObject; AE: TSoContainer);
    procedure SetOnExecute(const Value: TNotifyEvent<TSoContainer>);
  public
    property OnCollide: TEvent<TSoContainer> read FOnCollide write FOnCollide;
    function IsContainsPoint(const AX, AY: Single): Boolean;
    constructor Create(const ASubject: TSoContainer); override;
  end;

implementation

{ TSoColliderObj }

constructor TSoColliderObj.Create(const ASubject: TSoContainer);
begin
  inherited Create(ASubject);
  OnCollide := EmptyHandler;
end;

procedure TSoColliderObj.EmptyHandler(ASender: TObject; AE: TSoContainer);
begin

end;

function TSoColliderObj.IsContainsPoint(const AX, AY: Single): Boolean;
begin

end;

procedure TSoColliderObj.SetOnExecute(const Value: TNotifyEvent<TSoContainer>);
begin

end;

end.
