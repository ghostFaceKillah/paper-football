unit game_window;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  GR32_Image, GR32, game_state, player, buffers, game_history, types, editor;

type

  { Tmain }

  Tmain = class(TForm)
    goToEditor: TButton;
    getHint, saveGame, goToMainMenu,
      prevStick, nextStick, prevMove, nextMove, 
      giveHint: TButton;
      SaveDialog1: TSaveDialog;
    game_canvas: TPaintBox32;
    nowPlayingLabel: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure drawBuffer();
    procedure drawBall(x, y : Integer);
    procedure drawPoint(x, y : Integer; c : TColor);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure game_canvasMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure getHintClick(Sender: TObject);
    procedure goToEditorClick(Sender: TObject);
    procedure goToMainMenuClick(Sender: TObject);
    procedure initColors();
    function extractDirection(x, y: Integer) : Integer;
    procedure game_apply(move : Integer);
    procedure nextMoveClick(Sender: TObject);
    procedure nextStickClick(Sender: TObject);
    procedure prevMoveClick(Sender: TObject);
    procedure prevStickClick(Sender: TObject);
    procedure get_ai_move();
    procedure init(h,w : Integer; up, down : String);
    procedure saveGameClick(Sender: TObject);
  private
    save_file : TextFile;
    BadRed, Background, Gray, HintGreen,
      Black, GoodBlue, TextHighlight : TColor;
    trans_x, trans_y : Integer;
    history, current : moves;
    intercept : Boolean;
    hint_mode : Boolean;
    { private declarations }
  public
    game : GameState;
    { public declarations }
  end;

var
  main: Tmain;

implementation

{$R *.lfm}

{ Tmain }

procedure Tmain.initColors();
begin
  BadRed := Color32(255, 30, 30, 0);
  Background := Color32(255, 255, 255, 255);
  Black := Color32(0, 0, 0, 255);
  Gray := Color32(215, 215, 215, 255);
  HintGreen := Color32(21, 246, 43, 255);
  GoodBlue := Color32(40, 40, 255, 0);
  textHighlight := Color32(240, 255, 140, 0);
end;

procedure Tmain.drawPoint(x, y : Integer; c : TColor);
  begin
    game_canvas.Buffer.FillRectT(x + 1, y + 1, x + 4, y + 4, c);
  end;

procedure Tmain.drawBall(x, y : Integer);
  begin
    game_canvas.Buffer.FillRectT(x, y, x + 5, y + 5, Black);
  end;

procedure Tmain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Application.Terminate;
end;

procedure Tmain.FormCreate(Sender: TObject);
begin

end;

function Tmain.extractDirection(x, y: Integer) : Integer;
var
  a : Double;
  ball_x, ball_y, x_rel, y_rel : Integer;
  dir : Integer;
begin
  ball_y := (game.ball_y + 1)*scale + trans_y;
  ball_x := (game.ball_x + 1)*scale + trans_x;
  x_rel := x - ball_x;
  y_rel := ball_y - y;
  dir := -1;
  if abs(ball_y - y) + abs(x_rel) >= 10 then begin
    if (x_rel = 0) then begin
      if (y_rel > 0) then begin
        dir := 0;
      end else begin
        dir := 4;
      end;
    end else begin
      a := (y_rel) / (x_rel); // tan pi/8 = 0.4142, tan 3/8 = 2.4142
      if x_rel > 0 then begin
        if a < -2.412 then
          dir := 4
        else if a < -0.4142 then
          dir := 3
        else if a < 0.4142 then
          dir := 2
        else if a < 2.4142 then
          dir := 1
        else
          dir := 0;
      end else begin
        if a < -2.412 then
          dir := 0
        else if a < -0.4142 then
          dir := 7
        else if a < 0.4142 then
          dir := 6
        else if a < 2.4142 then
          dir := 5
        else
          dir := 4;
      end;
    end;
  end;
  Result := dir;
end;

procedure Tmain.game_apply(move : Integer);
var
    current_color: Integer;
    player_name, direction : String;
begin
  if (hint_mode) then
    current_color := 4
  else
    if (game.up_turn) then begin
      current_color := 1;
    end else begin
      current_color := 2;
  end;
  case isPossible(move, game.up_turn, game) of
    yes: begin
      applyStickAndDot(move, current_color, game);
      switchTurns(game);
      push_move(move, true, hint_mode, current, history);
    end;
    nonfinal: begin
      applyStickAndDot(move, current_color, game);
      push_move(move, false, hint_mode, current, history);
    end;
    no: begin
       //writeln('This move is impossible');
       // status label illegal move
    end;
    win: begin
      applyStickAndDot(move, current_color, game);
      push_move(move, true, hint_mode, current, history);
      game.finished := true;
      switchTurns(game);
      make_buffer(game);
      drawBuffer();
      if game.up_turn then begin
        direction := 'w dół';
        if game.player_up = 'ai' then
          player_name := 'komputer'
        else
          player_name := 'człowiek';
      end else begin
        direction:= 'do góry';
        if game.player_down = 'ai' then
          player_name := 'komputer'
        else
          player_name := 'człowiek';
        end;
        ShowMessage('Wygral ' + player_name + ' grając ' + direction);
      end;
    loss: begin
      applyStickAndDot(move, current_color, game);
      push_move(move, true, hint_mode, current, history);
      game.finished := true;
      switchTurns(game);
      make_buffer(game);
      drawBuffer();
      if game.up_turn then begin
        direction := 'do góry';
        if game.player_up = 'ai' then
          player_name := 'komputer'
        else
          player_name := 'człowiek';
      end else begin
        direction:= 'w dół';
        if game.player_down = 'ai' then
          player_name := 'komputer'
        else
          player_name := 'człowiek';
      end;
      ShowMessage('Wygrał ' + player_name + ' grając ' + direction + '! Gratulacje!');
    end;
  end;
  Application.ProcessMessages;
  make_buffer(game);
  drawBuffer();
end;

procedure Tmain.nextMoveClick(Sender: TObject);
begin
   hint_mode := false;
  if not(is_forward_empty(current, history)) then begin
    intercept := true;
    go_move_forward(current, history, game);
    make_buffer(game);
    drawBuffer();
  end;
end;

procedure Tmain.nextStickClick(Sender: TObject);
begin
  hint_mode := false;
  if not(is_forward_empty(current, history)) then begin
    intercept := true;
    go_stick_forward(current, history, game);
    make_buffer(game);
    drawBuffer();
  end;
end;

procedure Tmain.prevMoveClick(Sender: TObject);
begin
  hint_mode := false;
  if not(is_back_empty(current, history)) then begin
    intercept := true;
    go_move_back(current, history, game);
    make_buffer(game);
    drawBuffer();
  end;
end;

procedure Tmain.prevStickClick(Sender: TObject);
begin
  hint_mode := false;
  if not(is_back_empty(current, history)) then begin
    intercept := true;
    go_stick_back(current, history, game);
    make_buffer(game);
    drawBuffer();
  end;
end;

procedure Tmain.get_ai_move();
var
  move: Integer;
begin
  while (current_player_is_ai(game) and not(game.finished) and not(intercept)) do begin
    if game.up_turn then
      move := ai_up_player_decide(game)
    else
      move := ai_down_player_decide(game);
    game_apply(move);
  end;
end;

procedure Tmain.game_canvasMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  move : Integer;
begin
  intercept := false;
  if hint_mode then
    hint_mode := false;

  while current^.is_hint do begin
    go_stick_back(current, history, game);
    make_buffer(game);
    drawBuffer();
  end;

  
  if not(current_player_is_ai(game)) then begin
    move := extractDirection(x, y);
    game_apply(move);
  end;

  get_ai_move();
end;

procedure Tmain.getHintClick(Sender: TObject);
var
  move : Integer;
  up_played : Boolean;
begin
  if not(hint_mode) then begin
    up_played := game.up_turn;
    while ((up_played = game.up_turn) and not(game.finished)) do begin
      hint_mode := true;
      if game.up_turn then
        move := ai_up_player_decide(game)
      else
        move := ai_down_player_decide(game);
      game_apply(move);
    end;
  end;
end;

procedure Tmain.goToEditorClick(Sender: TObject);
begin
  editor_instance.game := game;
  main.Visible := False;
  editor_instance.Visible := True;
  editor_instance.BringToFront;
  while editor_instance.Visible do
    Application.ProcessMessages;
  game := editor_instance.game;
  editor_instance.Visible := False;
  main.Visible := True;
  main.BringToFront;
end;

procedure Tmain.goToMainMenuClick(Sender: TObject);
begin
  main.Visible := false;
end;

procedure Tmain.init(h,w : Integer; up, down : String);
begin
  initializeGame(up, down, game, h, w);
end;

procedure Tmain.saveGameClick(Sender: TObject);
begin
  if SaveDialog1.Execute then begin
    AssignFile(save_file, SaveDialog1.FileName);
    writeGameToFile(game, save_file);
  end;
end;

procedure Tmain.FormActivate(Sender: TObject);
begin
  hint_mode := false;
  init_history(current, history);
  make_buffer(game);
  with SaveDialog1 do begin
    InitialDir:= GetCurrentDir;
    Filter:='Pliki gry (*.game)|*.game';
  end;
  trans_x := (game_canvas.Width - (game.w + 2)*scale) div 2;
  trans_y := (game_canvas.Height - (game.h + 4)*scale) div 2;
  initColors();
  drawBuffer();
  get_ai_move();
end;

procedure Tmain.drawBuffer();
var
  i : Integer;
  draw_col : TColor;
  player_name, direction : String;
begin
  game_canvas.Buffer.Clear(Background);
  for i := 0 to line_buffer_end-1 do begin
    case line_buffer[i].what of
      1: draw_col := BadRed;
      2: draw_col := GoodBlue;
      3: draw_col := Black;
      4: draw_col := HintGreen;
      5: draw_col := Black;
    end;
    game_canvas.Buffer.line(
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
  if game.up_turn then begin
    direction := 'do góry';
    if game.player_up = 'ai' then
      player_name := 'komputer'
    else
      player_name := 'człowiek';
  end else begin
    direction:= 'w dół';
    if game.player_down = 'ai' then
      player_name := 'komputer'
    else
      player_name := 'człowiek';
  end;
  nowPlayingLabel.Caption := 'Ruch wykonuje ' + player_name +
                             ' w kierunku ' + direction;


  game_canvas.Repaint;
end;

end.

