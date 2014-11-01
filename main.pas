program pilka_temp_loop;

uses game_state, player, buffers, game_history;

var
  game : GameState;
  color : Integer;
  move : Integer;
  game_continue : Boolean;
  history, current : moves;

begin
  initializeGame(game, 10, 8);
  writeGameToFile(game);
  game_continue := true;
  init_history(current, history);
  while (game_continue) do begin
    if (game.p1_turn) then begin
      move := human_player_decide(game);
      color := 1;
    end else begin
      move := ai_down_player_decide(game);
      color := 2;
    end;
    case isPossible(move, game.p1_turn, game) of
      yes: begin
        applyStickAndDot(move, color, game);
        switchTurns(game);
        push_move(move, true, current, history);
      end;
      nonfinal: begin
        applyStickAndDot(move, color, game);
        push_move(move, false, current, history);
      end;
      no: begin
        writeln('This move is impossible');
      end;
      win: begin
        applyStickAndDot(move, color, game);
        push_move(move, true, current, history);
        game_continue := false;
        if (game.p1_turn) then
          writeln('Player 1 wins!')
        else
          writeln('Player 2 wins!');
      end;
      loss: begin
        applyStickAndDot(move, color, game);
        push_move(move, true, current, history);
        game_continue := false;
        if (game.p1_turn) then
          writeln('Player 2 wins!')
        else
          writeln('Player 1 wins!');
      end;
    end;
    make_buffer(game);
    writeGameToFile(game);
  end;
  print_history(history);

end.
