unit editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  GR32_Image, GR32, game_state, buffers;

type

  { Teditor }

  Teditor = class(TForm)
    back_to_game: TButton;
    up_player_type: TComboBox;
    down_player_type: TComboBox;
    Label1: TLabel;
    Label3: TLabel;
    nowEditing: TComboBox;
    nowPlaying: TComboBox;
    now_editing_label: TLabel;
    Label2: TLabel;
    edit_canvas: TPaintBox32;
    procedure back_to_gameClick(Sender: TObject);
    procedure down_player_typeChange(Sender: TObject);
    procedure edit_canvasMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormActivate(Sender: TObject);
    procedure drawPoint(x, y : Integer; c : TColor);
    procedure drawBall(x, y : Integer);
    procedure drawBuffer();
    procedure initColors();
    procedure nowEditingChange(Sender: TObject);
    procedure nowPlayingChange(Sender: TObject);
    procedure switch_entry(var entry : Integer);
    procedure up_player_typeChange(Sender: TObject);

  private
    { private declarations }
    editing_mode : String;
    BadRed, Background, Gray, HintGreen,
      Black, GoodBlue, TextHighlight : TColor;
    trans_x, trans_y : Integer;
  public
    game : GameState;
    { public declarations }
  end;

var
  editor_instance: Teditor;


implementation

{$R *.lfm}

{ Teditor }

procedure Teditor.edit_canvasMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  l: line_elem;
  i, j, n, m : Integer;
  dot_x, dot_y : Integer;
begin
  if editing_mode = 'diags' then begin
    n := game.h + 2;
    m := game.w + 2;
    for i := 0 to n-1 do
      for j := 0 to m-1 do begin
          extract_diag_line(i, j, game.diag[i*m + j], l);
          if (l.x1 + trans_x > x) and (l.x2 + trans_x < x) and
                 (l.y1 + trans_y < y) and (l.y2 + trans_y > y) then begin
            switch_entry(game.diag[i*m + j]);
          end;
      end;
  end else if editing_mode = 'adiags' then begin
    n := game.h + 2;
    m := game.w + 2;
    for i := 0 to n-1 do
      for j := 0 to m-1 do begin
          extract_adiag_line(i, j, game.adiag[i*m + j], l);
          if (l.x1 + trans_x < x) and (l.x2 + trans_x > x) and
                 (l.y1 + trans_y < y) and (l.y2 + trans_y > y) then begin
            switch_entry(game.adiag[i*m + j]);
          end;
      end;
  end else if editing_mode = 'vert' then begin
    n := game.h + 2;
    m := game.w + 1;
    for i := 0 to n-1 do
      for j := 0 to m-1 do begin
          extract_vert_line(i, j, game.vert[i*m + j], l);
          if (l.x1 + trans_x - 10 < x) and (l.x2 + trans_x + 10 > x) and
                 (l.y1 + trans_y < y) and (l.y2 + trans_y > y) then begin
            switch_entry(game.vert[i*m + j]);
          end;
      end;
  end else if editing_mode = 'hor' then begin
    n := game.h + 1;
    m := game.w + 2;
    for i := 0 to n-1 do
      for j := 0 to m-1 do begin
          extract_hor_line(i, j, game.hor[i*m + j], l);
          if (l.x1 + trans_x < x) and (l.x2 + trans_x > x) and
                 (l.y1 + trans_y - 10 < y) and (l.y2 + trans_y + 10 > y) then begin
            switch_entry(game.hor[i*m + j]);
          end;
      end;
  end else if editing_mode = 'dots' then begin
    n := game.h + 3;
    m := game.w + 1;
    for i := 0 to n-1 do
      for j := 0 to m-1 do begin
        dot_x := j*scale + scale - 2 + trans_x;
        dot_y := i*scale + scale + trans_y;
        if abs(x - dot_x) + abs(y - dot_y) < 10 then begin
          game.dots[i*m + j] := not(game.dots[i*m + j]);
          make_buffer(game);
          drawBuffer();
        end;
      end;
  end else if editing_mode = 'ball_pos' then begin
    n := game.h + 3;
    m := game.w + 1;
    for i := 0 to n-1 do
      for j := 0 to m-1 do begin
        dot_x := j*scale + scale - 2 + trans_x;
        dot_y := i*scale + scale + trans_y;
        if abs(x - dot_x) + abs(y - dot_y) < 10 then begin
          game.ball_x := j;
          game.ball_y := i;
          make_buffer(game);
          drawBuffer();
        end;
      end;
  end;
end;

procedure Teditor.switch_entry(var entry : Integer);
begin
  if (entry <> 0) then begin
    if (entry <> 3) then
    entry := 0;
  end else
    entry := 5;
  make_buffer(game);
  drawBuffer();
end;

procedure Teditor.up_player_typeChange(Sender: TObject);
begin
  case up_player_type.Caption of
     'człowiek' : game.player_up := 'human';
     'komputer' : game.player_up := 'ai';
   end;
end;


procedure Teditor.back_to_gameClick(Sender: TObject);
begin
  Close;
end;

procedure Teditor.down_player_typeChange(Sender: TObject);
begin
  case down_player_type.Caption of
     'człowiek' : game.player_down := 'human';
     'komputer' : game.player_down := 'ai';
   end;
end;

procedure Teditor.drawPoint(x, y : Integer; c : TColor);
  begin
    edit_canvas.Buffer.FillRectT(x + 1, y + 1, x + 4, y + 4, c);
  end;

procedure Teditor.drawBall(x, y : Integer);
  begin
    edit_canvas.Buffer.FillRectT(x, y, x + 5, y + 5, Black);
  end;

procedure Teditor.drawBuffer();
var
  i : Integer;
  draw_col : TColor;
begin
  edit_canvas.Buffer.Clear(Background);
  for i := 0 to line_buffer_end-1 do begin
    case line_buffer[i].what of
      1: draw_col := BadRed;
      2: draw_col := GoodBlue;
      3: draw_col := Black;
      4: draw_col := HintGreen;
      5: draw_col := Black;
    end;
    edit_canvas.Buffer.line(
      line_buffer[i].x1 + trans_x, line_buffer[i].y1 + trans_y,
      line_buffer[i].x2 + trans_x, line_buffer[i].y2 + trans_y,  
      draw_col);
  end;
  for i := 0 to dot_buffer_end-1 do begin
    if dot_buffer[i].what = 1 then
      drawPoint(dot_buffer[i].x + trans_x, dot_buffer[i].y + trans_y, Black)
    else if dot_buffer[i].what = 0 then
      drawPoint(dot_buffer[i].x + trans_x, dot_buffer[i].y + trans_y, Gray);
  end;
  drawBall((game.ball_x+1)*scale - 2 + trans_x, (game.ball_y+1)*scale + trans_y);
  edit_canvas.Repaint;
end;

procedure Teditor.initColors();
begin
  BadRed := Color32(255, 30, 30, 0);
  Background := Color32(255, 255, 255, 255);
  Black := Color32(0, 0, 0, 255);
  Gray := Color32(215, 215, 215, 255);
  HintGreen := Color32(21, 246, 43, 255);
  GoodBlue := Color32(40, 40, 255, 0);
  textHighlight := Color32(240, 255, 140, 0);
end;

procedure Teditor.nowEditingChange(Sender: TObject);
begin
  case nowEditing.Caption of
    'Przeciwprzekątne' : editing_mode := 'diags';
    'Przekątne' : editing_mode := 'adiags';
    'Pionowe' : editing_mode := 'vert';
    'Poziome' : editing_mode := 'hor';
    'Kropki' : editing_mode := 'dots';
    'Pozycja piłki' : editing_mode := 'ball_pos';
  end;
end;

procedure Teditor.nowPlayingChange(Sender: TObject);
begin
  case nowPlaying.Caption of
    'do góry' : game.up_turn := true;
    'na dół' : game.up_turn := false;
  end;
end;

procedure Teditor.FormActivate(Sender: TObject);
begin
  editing_mode := 'diags';
  nowEditing.Caption := 'Przeciwprzekątne';
  if game.up_turn then
    nowPlaying.Caption := 'do góry'
  else
    nowPlaying.Caption := 'na dół';
  if game.player_up = 'ai' then
    up_player_type.Caption := 'komputer'
  else
    up_player_type.Caption := 'człowiek';
  if game.player_down = 'ai' then
    down_player_type.Caption := 'komputer'
  else
    down_player_type.Caption := 'człowiek';

  make_buffer(game);
  trans_x := (edit_canvas.Width - (game.w + 2)*scale) div 2;
  trans_y := (edit_canvas.Height - (game.h + 4)*scale) div 2;
  initColors();
  drawBuffer();
end;

end.

