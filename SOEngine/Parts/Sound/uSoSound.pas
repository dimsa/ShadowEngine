unit uSoSound;

interface

uses
  FMX.Media, FMX.Types,
  System.Threading, System.SysUtils, uSoBasePart, uSoObject;

type

 TSoSound = class(TSoBasePart)
  private
    FMedia: TMedia;
    FVolume: Integer;
    FLoaded: Boolean;
    FLoop: Boolean;
    FTimer: TTimer;
    FFileName: string;
    procedure SetVolume(const Value: Integer);
    procedure SetLoop(const Value: Boolean);
    procedure InitPlay;
    procedure FinishPlay;
 protected
   // Don't use it's manually. It for uSoSoundKeeper it call it after creation of TMediaPlayer
   // Otherwise you will got error about not supported format.
   procedure Load;
   procedure OnMediaEnded(ASender: TObject);
 public
    procedure Play; virtual;
    procedure Stop; virtual;
    procedure Pause; virtual;
    procedure Resume; virtual;
    property Volume: Integer read FVolume write SetVolume;
    property Loop: Boolean read FLoop write SetLoop;
    property FileName: string read FFileName;
    constructor Create(const ASubject: TSoObject; const AFileName: string);
    destructor Destroy; override;
 end;

implementation

{ TSoSound }

constructor TSoSound.Create(const ASubject: TSoObject; const AFileName: string);
begin
  inherited Create(ASubject);

  FLoaded := False;
  FLoop := False;
  FFileName := AFileName;
end;

destructor TSoSound.Destroy;
begin
  if Assigned(FTimer) then
  begin
    FTimer.Enabled := False;
    FTimer.Free;
  end;
  FMedia.Free;
  inherited;
end;

procedure TSoSound.FinishPlay;
begin
  FTimer.Enabled := False;
end;

procedure TSoSound.InitPlay;
begin
  FTimer.Interval := (FMedia.Duration - FMedia.CurrentTime) div 10000;
  FTimer.Enabled := True;
end;

procedure TSoSound.Load;
//var
//  vTask: ITask;
begin
{ TODO : Add async loading with OnLoaded Events }
//  vTask := TTask.Create (procedure ()
//  begin
    if FMedia <> nil then
    begin
      FMedia.DisposeOf;
      FMedia := nil;
    end;

    FMedia := TMediaCodecManager.CreateFromFile(FFileName);

    FTimer := TTimer.Create(nil);
    FTimer.Enabled := False;
    FTimer.OnTimer := OnMediaEnded;
    FLoaded := True;
//  end);
//  vTask.Start;
end;

procedure TSoSound.OnMediaEnded(ASender: TObject);
begin
  if FLoop then
    Play;
end;

procedure TSoSound.Pause;
begin
  if not FLoaded then
    Exit;

  FMedia.Stop;
  FinishPlay;
end;

procedure TSoSound.Play;
begin
  if not FLoaded then
    Exit;
  FMedia.CurrentTime := 0;
  FMedia.Play;
  InitPlay;
end;

procedure TSoSound.Resume;
begin
  if not FLoaded then
    Exit;

  FMedia.Play;
  InitPlay;
end;


procedure TSoSound.Stop;
begin
  if not FLoaded then
    Exit;

  FMedia.Stop;
  FMedia.CurrentTime := 0;
  FinishPlay;
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

end.

