// В примере рассмотрен вариант с созданием наслденика TEngine

unit uDemoEngine;

interface

uses
  uEngine2D;

type
  TDemoEngine = class(TEngine2D)
  private
    procedure DoWork;
    procedure DoWorkGame;
  public
    constructor Create; override;
  const
    CGameRun = 1; // Игра идет
    CGameEnded = 2; // Ждем клика для создания новой игры
  end;

implementation

{ TDemoEngine }

constructor TDemoEngine.Create;
begin
  inherited;
  Status := CGameEnded;
  EngineThread.WorkProcedure := Self.DoWork;
end;

procedure TDemoEngine.DoWork;
begin
  case Status of
    CGameRun: doWorkGame;
  end;
end;

procedure TDemoEngine.DoWorkGame;
begin
  Repaint;
end;

end.
