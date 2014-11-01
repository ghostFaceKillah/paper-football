unit welcome_menu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  new_game_settings_code, game_window, game_state;

type

  { Twelcome_window }

  Twelcome_window = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    new_game_button: TButton;
    OpenDialog1: TOpenDialog;
    open_game: TButton;
    end_game: TButton;
    Label1: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure new_game_buttonClick(Sender: TObject);
    procedure end_gameClick(Sender: TObject);
    procedure open_gameClick(Sender: TObject);
  private
    open_file : TextFile;
    { private declarations }
  public
    { public declarations }
  end;

var
  welcome_window: Twelcome_window;

implementation

{$R *.lfm}

{ Twelcome_window }

procedure Twelcome_window.end_gameClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure Twelcome_window.open_gameClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    AssignFile(open_file, OpenDialog1.FileName);
    ReadGameFromFile(main.game, open_file);
    AssignFile(open_file, 'second_test.game');
    WriteGameToFile(main.game, open_file);
    main.Visible := True;
    main.FormActivate(welcome_window);
    welcome_window.Visible := False;
    while main.Visible do
      Application.ProcessMessages;
    welcome_window.Visible := True;
    welcome_window.BringToFront;
  end;
end;

procedure Twelcome_window.new_game_buttonClick(Sender: TObject);
begin
  new_game_settings.Visible := True;
  welcome_window.Visible := False;
  while new_game_settings.Visible do
    Application.ProcessMessages;
  if new_game_settings.success then begin
    main.init(new_game_settings.h, new_game_settings.w,
              new_game_settings.up, new_game_settings.down);

    main.Visible := True;
    welcome_window.Visible := False;
  end else begin
    welcome_window.Visible := True;
    welcome_window.BringToFront;
  end;
  while main.Visible do
    Application.ProcessMessages;
  welcome_window.Visible := True;
  welcome_window.BringToFront;
end;

procedure Twelcome_window.FormActivate(Sender: TObject);
begin
  with OpenDialog1 do begin
    InitialDir:= GetCurrentDir;
    Filter:='Pliki gry (*.game)|*.game';
  end;
end;

end.

