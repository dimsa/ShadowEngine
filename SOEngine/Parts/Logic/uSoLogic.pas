unit uSoLogic;

interface

uses
  uCommonClasses, uSoBasePart, uSoObject;

type
  TSoLogic = class abstract(TSoBasePart)
  protected
    procedure Execute; virtual; abstract;
  end;

  TSoObjectLogic = class(TSoLogic)
  private
    FOnExecute: TNotifyEvent<TSoObject>;
  public
    procedure Execute; override;
    constructor Create(const ASubject: TSoObject; AOnExecute: TNotifyEvent<TSoObject>);
  end;

  TSoStaticLogic = class(TSoLogic)
  private
    FOnExecute: TStaticNotifyEvent<TSoObject>;
  public
    procedure Execute; override;
    constructor Create(const ASubject: TSoObject; AOnExecute: TStaticNotifyEvent<TSoObject>);
  end;

implementation

{ TSoObjectLogic }

constructor TSoObjectLogic.Create(const ASubject: TSoObject;
  AOnExecute: TNotifyEvent<TSoObject>);
begin
  inherited Create(ASubject);
  FOnExecute := AOnExecute;
end;

procedure TSoObjectLogic.Execute;
begin
  FOnExecute(FSubject);
end;

{ TSoStaticLogic }

constructor TSoStaticLogic.Create(const ASubject: TSoObject;
  AOnExecute: TStaticNotifyEvent<TSoObject>);
begin
  inherited Create(ASubject);
  FOnExecute := AOnExecute;
end;

procedure TSoStaticLogic.Execute;
begin
  FOnExecute(FSubject);
end;

end.
