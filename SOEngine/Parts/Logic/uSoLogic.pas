unit uSoLogic;

interface

uses
  uCommonClasses, uSoBasePart, uSoObject, uISoObject;

type
  TSoLogic = class abstract(TSoBasePart)
  protected
    procedure Execute; virtual; abstract;
  end;

  TSoObjectLogic = class(TSoLogic)
  private
    FOnExecute: TNotifyEvent<ISoObject>;
  public
    procedure Execute; override;
    constructor Create(const ASubject: TSoObject; AOnExecute: TNotifyEvent<ISoObject>);
  end;

  TSoStaticLogic = class(TSoLogic)
  private
    FOnExecute: TStaticNotifyEvent<ISoObject>;
  public
    procedure Execute; override;
    constructor Create(const ASubject: TSoObject; AOnExecute: TStaticNotifyEvent<ISoObject>);
  end;

implementation

{ TSoObjectLogic }

constructor TSoObjectLogic.Create(const ASubject: TSoObject;
  AOnExecute: TNotifyEvent<ISoObject>);
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
  AOnExecute: TStaticNotifyEvent<ISoObject>);
begin
  inherited Create(ASubject);
  FOnExecute := AOnExecute;
end;

procedure TSoStaticLogic.Execute;
begin
  FOnExecute(FSubject);
end;

end.
