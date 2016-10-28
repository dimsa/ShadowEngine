unit uSoColliderObject;

interface

uses
  uCommonClasses, uSoTypes, uSoBasePart, uSoObject, uSoColliderTypes;

type
  TSoColliderObj = class abstract(TSoBasePart)
  private
    FOnCollide: TEvent<TSoObject>;
    procedure EmptyHandler(ASender: TObject; AE: TSoObject);
    procedure SetOnExecute(const Value: TNotifyEvent<TSoObject>);
  protected
    FOnEndContact: TEventList<TObjectCollidedEventArgs>;
    FOnBeginContact: TEventList<TObjectCollidedEventArgs>;
  public
    //property OnCollide: TEvent<TSoObject> read FOnCollide write FOnCollide;
    property OnBeginContact: TEventList<TObjectCollidedEventArgs> read FOnBeginContact;
    property OnEndContact: TEventList<TObjectCollidedEventArgs> read FOnEndContact;
    function IsContainsPoint(const AX, AY: Single): Boolean; overload; virtual; abstract;
    function IsContainsPoint(const APoint: TPointF): Boolean; overload; virtual; abstract;
    constructor Create(const ASubject: TSoObject); override;
  end;

implementation

{ TSoColliderObj }

constructor TSoColliderObj.Create(const ASubject: TSoObject);
begin
  inherited Create(ASubject);

  FOnBeginContact := TEventList<TObjectCollidedEventArgs>.Create;
  FOnEndContact := TEventList<TObjectCollidedEventArgs>.Create;
//  OnCollide := EmptyHandler;
end;

procedure TSoColliderObj.EmptyHandler(ASender: TObject; AE: TSoObject);
begin

end;

procedure TSoColliderObj.SetOnExecute(const Value: TNotifyEvent<TSoObject>);
begin

end;

end.
