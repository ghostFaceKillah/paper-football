unit game_history;

interface

uses game_state;

type
  moves = ^move_elem;
  
  move_elem = record
    num : Integer;
    is_final : Boolean;
    is_hint : Boolean;
    next : moves;
    prev : moves;
  end;

procedure init_history(var current, history : moves);
procedure push_move(const i : Integer; const is_final, is_hint : Boolean; var current,
  history : moves);
procedure print_history(var history : moves);
procedure go_stick_back(var current, history : moves; var game : GameState);
procedure go_move_back(var current, history : moves; var game : GameState);
procedure go_stick_forward(var current, history : moves; var game : GameState);
procedure go_move_forward(var current, history : moves; var game : GameState);
function is_forward_empty(var current, history : moves): Boolean;
function is_back_empty(var current, history : moves): Boolean;

implementation

procedure init_history(var current, history : moves);
  begin
    new(history);
    history^.next := history;
    history^.prev := history;
    history^.is_hint := false;
    current := history;
  end;

function is_forward_empty(var current, history : moves): Boolean;
begin
  is_forward_empty := (current^.next = history);
end;

function is_back_empty(var current, history : moves): Boolean;
begin
  is_back_empty := (current = history);
end;

procedure push_move(const i : Integer; const is_final, is_hint : Boolean; var current,
  history : moves);
  var
    worker : moves;
  begin
    if (current^.next = history) then begin
      new(worker);
      worker^.next := history;
      history^.prev^.next := worker;
      worker^.prev := history^.prev;
      history^.prev := worker;
      worker^.is_final := is_final;
      worker^.num := i;
      worker^.is_hint := is_hint;
      current := worker;
    end else begin
      while (history^.prev <> current) do begin
        worker := history^.prev;
        history^.prev := history^.prev^.prev;
        dispose(worker);
      end;
      current^.next := history;
      push_move(i, is_final, is_hint, current, history);
    end;
  end;

procedure print_history(var history : moves);
  var
    worker : moves;
  begin
    worker := history^.next;
    while worker <> history do begin
      write(worker^.num,' ');
      if worker^.is_final then
        writeln();
      worker := worker^.next;
    end; 
  end;

procedure go_stick_back(var current, history : moves; var game : GameState);
  begin
    if current^.is_final then
      switchTurns(game);
    deapplyStickAndDot(current^.num, game);
    current := current^.prev;
  end;

procedure go_move_back(var current, history : moves; var game : GameState);
  begin
    if not(is_back_empty(current, history)) then
      go_stick_back(current, history, game);
    while (not(is_back_empty(current, history)) and not(current^.is_final)) do
      go_stick_back(current, history, game);
  end;

procedure go_stick_forward(var current, history : moves; var game : GameState);
  var
    who : Integer;
  begin
    if game.up_turn then
      who :=1
    else
      who :=2;
    current := current^.next;
    applyStickAndDot(current^.num, who, game);
    if current^.is_final then
      switchTurns(game);
  end;

procedure go_move_forward(var current, history : moves; var game : GameState);
  begin
    if not(is_forward_empty(current, history)) then
      go_stick_forward(current, history, game);
    while (not(current^.is_final) and not(is_forward_empty(current, history))) do
      go_stick_forward(current, history, game);
  end;

end.
