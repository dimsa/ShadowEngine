unit uSoSound;

interface

uses
  uSoBasePart, uGameAudioManager, FMX.Media,
  uSoObject, System.Threading, System.SysUtils;

type

 TSoSound = class(TSoBasePart)
  private
    FMedia: TMedia;
    FVolume: Integer;
    FLoaded: Boolean;
    FLoop: Boolean;
    FLoopTask: ITask;
    procedure SetVolume(const Value: Integer);
    procedure SetLoop(const Value: Boolean);

 protected
   procedure OnMediaEnded(ASender: TObject);
 public
    procedure Play; virtual;
    procedure Stop; virtual;
    procedure Pause; virtual;
    property Volume: Integer read FVolume write SetVolume;
    property Loop: Boolean read FLoop write SetLoop;
    constructor Create(const ASubject: TSoObject; const AFileName: string);
    destructor Destroy; override;
 end;

implementation

{ TSoSound }

constructor TSoSound.Create(const ASubject: TSoObject; const AFileName: string);
var
  vTask: ITask;
begin
  inherited Create(ASubject);
  FLoaded := False;
  FLoop := False;

  vTask := TTask.Create (procedure ()
  begin
    try
      FMedia := TMediaCodecManager.CreateFromFile(AFileName);

      FLoopTask := TTask.Create (procedure ()
      begin
        Sleep(FMedia.Duration - FMedia.CurrentTime);
        OnMediaEnded(Self);
      end);

      FLoaded := True;
    except

    end;
  end);
  vTask.Start;



end;

destructor TSoSound.Destroy;
begin
  FLoopTask := nil;
  inherited;
end;

procedure TSoSound.OnMediaEnded(ASender: TObject);
begin
  if FLoop then
  begin
    FMedia.CurrentTime := 0;
    FMedia.Play;
  end;
end;

procedure TSoSound.Pause;
begin
  if not FLoaded then
    Exit;
  FLoopTask.Cancel;
  FMedia.Stop;
end;

procedure TSoSound.Play;
begin
  if not FLoaded then
    Exit;
  FMedia.Play;
  FLoopTask.Start;
end;

procedure TSoSound.SetLoop(const Value: Boolean);
begin
  FLoop := Value;
end;

procedure TSoSound.SetVolume(const Value: Integer);
begin
  if not FLoaded then
    Exit;
  FMedia.Volume := (Value mod 100) / 100;
  FVolume := Value;
end;

procedure TSoSound.Stop;
begin
  if not FLoaded then
    Exit;
  FLoopTask.Cancel;
  FMedia.Stop;
  FMedia.CurrentTime := 0;
end;

end.
