unit new_game_settings_code;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus;

type

  { Tnew_game_settings }

  Tnew_game_settings = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    up_player_choice: TComboBox;
    down_player_choice: TComboBox;
    height_input: TEdit;
    width_input: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { private declarations }
  public
    h, w : Integer;
    up , down : String;
    success : Boolean;
    { public declarations }
  end;

var
  new_game_settings: Tnew_game_settings;

implementation

{$R *.lfm}

{ Tnew_game_settings }

procedure Tnew_game_settings.Button1Click(Sender: TObject);
begin
  h := StrToInt(height_input.Text);
  w := StrToInt(width_input.Text);
  if (h mod 2 <> 0) then
    ShowMessage('Wysokość musi być liczbą parzystą.')
  else if (w mod 2 <> 0) then
    ShowMessage('Szerokość musi być liczbą parzystą.')
  else if (w < 0) or (w > 20) then
    ShowMessage('Szerokość nie może być ujemna i nie może być większa niż 20')
  else if (h < 0) or (h > 20) then
    ShowMessage('Wysokość nie może być ujemna i nie może być większa niż 20')
  else begin
    if  up_player_choice.Text = 'komputer' then
      up := 'ai'
    else
      up := 'human';
    if  down_player_choice.Text = 'komputer' then
      down := 'ai'
    else
      down := 'human';
    success := True;
    new_game_settings.Close;
  end;
end;

procedure Tnew_game_settings.Button2Click(Sender: TObject);
begin
  new_game_settings.Close;
end;

procedure Tnew_game_settings.FormActivate(Sender: TObject);
begin
  success := False;
end;


end.

