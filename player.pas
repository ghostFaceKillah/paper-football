unit player;

interface

uses game_state;

function ai_up_player_decide(var game : GameState) : Integer;
function ai_down_player_decide(var game : GameState) : Integer;

implementation

const
  max_depth = 8;

function max(const a, b : Integer) : Integer;
  begin
    if (a > b) then
      max := a
    else
      max := b;
  end;

function min(const a, b : Integer) : Integer;
  begin
    if (a < b) then
      min := a
    else
      min := b;
  end;


function heur_fast(var game : GameState) : Integer;
  begin
    heur_fast := game.ball_y;
  end;

function alphabeta(const is_min : Boolean; alpha, beta : Integer;
                   var game : GameState; const h : Integer) : Integer;
  var
    i : Integer;
    cont : Boolean;
  begin
    i := 0;
    cont := True;
    if (h = max_depth) then
      alphabeta := heur_fast(game)
    else 
      if not(is_min) then begin
        while (i < 8) and cont do begin
          case isPossible(i, is_min, game) of
            yes: begin
              applyStickAndDot(i, 2, game);
              alpha := max(alpha, alphabeta(not(is_min), alpha, beta, game, h+1));
              deapplyStickAndDot(i, game);
            end;
            nonfinal: begin
              applyStickAndDot(i, 2, game);
              alpha := max(alpha, alphabeta(is_min, alpha, beta, game, h+1));
              deapplyStickAndDot(i, game);
            end;
            win: begin
              alpha := max(alpha, 2000);
            end;
            loss: begin
              alpha := max(alpha, -2000);
            end;
            no: begin
              alpha := max(alpha, -3000);
            end;
          end;
          inc(i);
          if (beta <= alpha) then
            cont := false;
          alphabeta := alpha;
        end;
      end else begin
        while (i < 8) and cont do begin
          case isPossible(i, is_min, game) of
            yes: begin
              applyStickAndDot(i, 1, game);
              beta := min(beta, alphabeta(not(is_min), alpha, beta, game, h+1));
              deapplyStickAndDot(i, game);
            end;
            nonfinal: begin
              applyStickAndDot(i, 1, game);
              beta := min(beta, alphabeta(is_min, alpha, beta, game, h+1));
              deapplyStickAndDot(i, game);
            end;
            win: begin
              beta := min(beta, -2000);
            end;
            loss: begin
              beta := min(beta, 2000);
            end;
            no: begin
              beta := min(beta, 3000);
            end;
          end;
          inc(i);
          if (beta <= alpha) then
            cont := false;
        end;
        alphabeta := beta;
      end;
  end;

function ai_up_player_decide(var game : GameState) : Integer;
  var
    moves : Array [0..7] of Integer;
    alpha, beta, i, min_val, min_i : Integer;
  begin
    alpha := -4000;
    beta := 4000;
    for i := 0 to 7 do begin
      case isPossible(i, true, game) of
        yes: begin
          applyStickAndDot(i, 1, game);
          moves[i] := alphabeta(false, alpha, beta, game, 0);
          deapplyStickAndDot(i, game);
        end;
        nonfinal: begin
          applyStickAndDot(i, 1, game);
          moves[i] := alphabeta(true, alpha, beta, game, 0);
          deapplyStickAndDot(i, game);
        end;
        win: begin
          moves[i] := -2000;
        end;
        loss: begin
          moves[i] := 2000;
        end;
        no: begin
          moves[i] := 3500;
        end;
      end;
    end;
    min_val := 4000;
    min_i := -1;
    for i := 0 to 7 do begin
      if (moves[i] < min_val) then begin
        min_val := moves[i];
        min_i := i;
      end;
    end;
    ai_up_player_decide := min_i;
  end;

function ai_down_player_decide(var game : GameState) : Integer;
  var
    moves : Array [0..7] of Integer;
    alpha, beta, i, max_val, max_i : Integer;
  begin
    alpha := -4000;
    beta := 4000;
    for i := 0 to 7 do begin
      case isPossible(i, false, game) of
        yes: begin
          applyStickAndDot(i, 2, game);
          moves[i] := alphabeta(true, alpha, beta, game, 0);
          deapplyStickAndDot(i, game);
        end;
        nonfinal: begin
          applyStickAndDot(i, 2, game);
          moves[i] := alphabeta(false, alpha, beta, game, 0);
          deapplyStickAndDot(i, game);
        end;
        win: begin
          moves[i] := 2000;
        end;
        loss: begin
          moves[i] := -2000;
        end;
        no: begin
          moves[i] := -3500;
        end;
      end;
    end;
    max_val := -4000;
    max_i := -1;
    for i := 0 to 7 do begin
      if (moves[i] > max_val) then begin
        max_val := moves[i];
        max_i := i;
      end;
    end;
    ai_down_player_decide := max_i;
  end;

end.
