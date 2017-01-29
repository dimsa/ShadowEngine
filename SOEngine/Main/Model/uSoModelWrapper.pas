unit uSoModelWrapper;

interface

uses
  uSoTypes,
  uSoCoreModel, uSoModel,
  uSoIOperator, uSoIDelegateCollector, uSoDelegateCollector

  uSoEngineOptions, uSoEngineEvents;

type
  TSoModelWrapper = class
  private
    FModel: TSoModel;
    FCoreModel: TSoCoreModel;
    FEvents: TSoEngineEvents;
    FOperators: TList<ISoOperator>;
    FRegisterDelegate: IRegisterDelegateCollector;
    procedure InitDelegateCollector;
  public
    procedure ExecuteOnTick;
    procedure ExecuteKeyUp(ASender: TObject; Key: Word; KeyChar: Char; Shift: TShiftState); // Process key on tick
    procedure ExecuteKeyDown(ASender: TObject; Key: Word; KeyChar: Char; Shift: TShiftState); // Process key on tick
    procedure ExecuteMouseDown(ASender: TObject; AEventArgs: TMouseEventArgs);
    procedure ExecuteMouseUp(ASender: TObject; AEventArgs: TMouseEventArgs);
    procedure ExecuteMouseMove(ASender: TObject; AEventArgs: TMouseMoveEventArgs);

    constructor Create(
      const AImage: TAnonImage;
      const ACritical: TCriticalSection;
      const AOptions: TSoEngineOptions);
  end;

implementation

{ TSoModelWrapper }

constructor TSoModelWrapper.Create(
  const AImage: TAnonImage;
  const ACritical: TCriticalSection;
  const AOptions: TSoEngineOptions);
begin
  FEvents := TSoEngineEvents.Create(AImage);
  //FCoreModel := TSoCoreModelPart.Create(FEvents, AImage, FCritical);
  FCoreModel := TSoCoreModel.Create(FEvents, AImage, ACritical);
  FModel := TSoModel.Create(FEvents, AImage, ACritical, AOptions);

  FOperators := TList<ISoOperator>.Create;
  with FOperators do begin
    Add(FModel.ObjectKeeper);
    Add(FModel.LogicKeeper);
    Add(FModel.Collider);
    Add(FModel.Formattor);
    Add(FModel.Animator);
    Add(FModel.KeyProcessor);
    Add(FModel.MouseProcessor);
    Add(FModel.SoundKeeper);
    Add(FCoreModel.Renderer);
  end;

  InitDelegateCollector;
end;

procedure TSoModelWrapper.ExecuteKeyDown(ASender: TObject; Key: Word; KeyChar: Char; Shift: TShiftState);
begin
  FModel.ExecuteKeyDown(ASender, Key, KeyChar, Shift);
end;

procedure TSoModelWrapper.ExecuteKeyUp(ASender: TObject; Key: Word; KeyChar: Char; Shift: TShiftState);
begin
  FModel.ExecuteKeyUp(ASender, Key, KeyChar, Shift);
end;

procedure TSoModelWrapper.ExecuteMouseDown(ASender: TObject; AEventArgs: TMouseEventArgs);
begin
  FModel.ExecuteMouseDown(ASender, AEventArgs);
end;

procedure TSoModelWrapper.ExecuteMouseMove(ASender: TObject; AEventArgs: TMouseMoveEventArgs);
begin
  FModel.ExecuteMouseMove(ASender, AEventArgs);
end;

procedure TSoModelWrapper.ExecuteMouseUp(ASender: TObject; AEventArgs: TMouseEventArgs);
begin
  FModel.ExecuteMouseUp(ASender, AEventArgs);
end;

procedure TSoModelWrapper.ExecuteOnTick;
begin
  FModel.ExecuteOnTick;
  FCoreModel.ExecuteOnTick;
end;

procedure TSoModelWrapper.InitDelegateCollector;
var
  vOperator: ISoOperator;
begin
  FRegisterDelegate := TSoDelegateCollector.Create;

  for vOperator in FOperators do
    vOperator.VisitByDelegateCollector(FRegisterDelegate);
end;

end.
