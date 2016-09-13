unit uSoColliderObject;

interface

uses
  uCommonClasses, uSoBasePart, uSoObject;

type
  TSoColliderObj = class(TSoBasePart)
  private
    FOnCollide: TEvent<TSoObject>;
    procedure EmptyHandler(ASender: TObject; AE: TSoObject);
    procedure SetOnExecute(const Value: TNotifyEvent<TSoObject>);
  public
    property OnCollide: TEvent<TSoObject> read FOnCollide write FOnCollide;
    function IsContainsPoint(const AX, AY: Single): Boolean;
    constructor Create(const ASubject: TSoObject); override;
  end;

implementation

{ TSoColliderObj }

constructor TSoColliderObj.Create(const ASubject: TSoObject);
begin
  inherited Create(ASubject);
  OnCollide := EmptyHandler;
end;

procedure TSoColliderObj.EmptyHandler(ASender: TObject; AE: TSoObject);
begin

end;

function TSoColliderObj.IsContainsPoint(const AX, AY: Single): Boolean;
begin

end;

procedure TSoColliderObj.SetOnExecute(const Value: TNotifyEvent<TSoObject>);
begin

end;

end.
