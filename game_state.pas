unit game_state;

interface

uses SysUtils;

const
  MAX_H = 40;
  MAX_W = 40;

type
  Option = (yes, nonfinal, no, win, loss);

  GameState = record
    player_up, player_down : String;
    up_turn : Boolean;
    finished : Boolean;
    h : Integer;
    w : Integer;
    ball_x : Integer;
    ball_y : Integer;
    dots  : Array [0..MAX_H*MAX_W] of Boolean ; // h+3, w+1
    vert  : Array [0..MAX_H*MAX_W] of Integer ; // h+2, w+1
    hor   : Array [0..MAX_H*MAX_W] of Integer ; // h+1, w+2
    diag  : Array [0..MAX_H*MAX_W] of Integer ; // h+2, w+2
    adiag : Array [0..MAX_H*MAX_W] of Integer ; // h+2, w+2
  end;

  procedure initializeGame(const p_up, p_down : String; var game : GameState;
                           n, m : Integer);
  procedure printArray(var A : Array of Boolean; n, m : Integer);
  function existsLine(const move_no, p_x, p_y : Integer; 
                    const game : GameState) : Boolean;
  function existsDot(const move_no : Integer; const g : GameState) : Boolean;
  function dotNeeded(dot_y, dot_x : Integer; const g : GameState) : Boolean;
  procedure applyStickAndDot(const move_no, who : Integer; var g : GameState);
  procedure deapplyStickAndDot(move_no : Integer; var g : GameState);
  function isPossible(move_no : Integer; isMin : Boolean; 
                      var game : GameState) : Option;
  function isWinningMove(isMin : Boolean; move_no : Integer; 
                         var g : GameState): Boolean;
  function isLosingMove(move_no : Integer; var g : GameState): Boolean;
  function goalDot(isMin : Boolean; const g : GameState) : Boolean;
  procedure writeGameToFile(const game : GameState; var f : TextFile);
  procedure switchTurns(var game : GameState);
  function current_player_is_ai(var game : GameState): Boolean;
  procedure readBooleanArrayFromFile(var A : Array of Boolean; n, m : Integer;
                           var f : TextFile);
  procedure readArrayFromFile(var A : Array of Integer; n, m : Integer; 
                              var f : TextFile);
  procedure readGameFromFile(var game : GameState; var f : TextFile);

implementation

procedure makeArray(var A : Array of Integer; n, m : Integer);
  var
    i, j : Integer;
  begin
    for i := 0 to n-1 do
      for j := 0 to m-1 do
        if ((i = 0) or (j = 0) or (i = n-1) or (j = m-1)) and
                      ( j <> (m - 1) div 2 ) and ( j <> m div 2) then
          a[i*m + j] := 3
        else
          a[i*m + j] := 0;
    for i := n*m to MAX_H*MAX_W do
      a[i] := 0;
  end;

procedure makeBoolArray(var A : Array of Boolean; n, m : Integer);
  var
    i, j : Integer;
  begin
    for i := 0 to n-1 do
      for j := 0 to m-1 do
        a[i*m + j] := ((i = 0) or (j = 0) or (i = n-1) or (j = m-1)) and
                      ( j <> (m - 1) div 2 ) and ( j <> m div 2);
    for i := n*m to MAX_H*MAX_W do
      a[i] := false;
  end;

procedure printArray(var A : Array of Boolean; n, m : Integer);
  var
    i, j : Integer;
  begin
    for i := 0 to n-1 do begin
      for j := 0 to m-1 do
        if a[i*m + j] then
          write(1 ,' ')
        else
          write(0 ,' ');
      writeln();
    end;
  end;

function current_player_is_ai(var game : GameState): Boolean;
begin
  current_player_is_ai := (((game.up_turn) and (game.player_up = 'ai')) or
                          (not(game.up_turn) and (game.player_down = 'ai')));
end;

procedure initializeGame(const p_up, p_down : String; var game : GameState;
                         n, m : Integer);
  // correct dots
  var
    j : Integer;
  begin
    game.player_up := p_up;
    game.player_down := p_down;
    game.up_turn := true;
    game.finished := false;
    game.ball_x := m div 2;
    game.ball_y := n div 2 + 1;
    game.h := n;
    game.w := m;
    makeBoolArray(game.dots, n+3, m+1);
    makeArray(game.vert, n+2, m+1);
    makeArray(game.hor , n+1, m+2);
    makeArray(game.diag, n+2, m+2);
    makeArray(game.adiag, n+2, m+2);
    game.dots[m div 2 + 1] := false;
    game.dots[m div 2 - 1] := false;
    game.dots[(m+1)*(n+2) + m div 2 + 1] := false;
    game.dots[(m+1)*(n+2) + m div 2 - 1] := false;
    game.dots[(m+1)*(n+2) div 2 + m div 2] := true;
    for j := 1 to m do begin
      game.dots[(m+1)*(n+1) + j] :=  (j <> m div 2);
      game.dots[m + 1 + j] := (j <> m div 2);
    end;
  end;

function existsLine(const move_no, p_x, p_y : Integer; 
                    const game : GameState) : Boolean;
  begin
    case move_no of
      0 : 
        existsLine := game.vert[(p_y-1)*(game.w+1) + p_x] <> 0;
      1 :
        existsLine := game.diag[(p_y-1)*(game.w+2) + p_x+1] <> 0;
      2 :
        existsLine :=  game.hor[(p_y-1)*(game.w+2) + p_x+1] <> 0;
      3 :
        existsLine := game.adiag[(p_y)*(game.w+2) + p_x+1] <> 0;
      4 :
        existsLine := game.vert[p_y*(game.w+1) + p_x] <> 0;
      5 :
        existsLine := game.diag[p_y*(game.w+2) + p_x] <> 0;
      6 :
        existsLine := game.hor[(p_y-1)*(game.w+2) + p_x] <> 0;
      7 :
        existsLine := game.adiag[(p_y-1)*(game.w+2) + p_x] <> 0;
    end;
  end;

function existsDot(const move_no : Integer; const g : GameState) : Boolean;
  begin
    case move_no of
      0 : 
        existsDot := g.dots[(g.ball_y-1)*(g.w+1) + g.ball_x];
      1 :
        existsDot := g.dots[(g.ball_y-1)*(g.w+1) + g.ball_x + 1];
      2 :
        existsDot := g.dots[(g.ball_y)*(g.w+1) + g.ball_x + 1];
      3 :
        existsDot := g.dots[(g.ball_y+1)*(g.w+1) + g.ball_x + 1];
      4 :
        existsDot := g.dots[(g.ball_y+1)*(g.w+1) + g.ball_x];
      5 :
        existsDot := g.dots[(g.ball_y+1)*(g.w+1) + g.ball_x-1];
      6 :
        existsDot := g.dots[(g.ball_y)*(g.w+1) + g.ball_x-1];
      7 :
        existsDot := g.dots[(g.ball_y-1)*(g.w+1) + g.ball_x-1];
    end;
  end;

function dotNeeded(dot_y, dot_x : Integer; const g : GameState) : Boolean;
  begin
    dotNeeded := 
    ((dot_x = g.w div 2 ) and (dot_y = 1 + g.h div 2 )) or
    (((dot_y-1)*(g.w+1) + dot_x >= 0) and
    (g.vert[(dot_y-1)*(g.w+1) + dot_x] <> 0)) or
    (((dot_y-1)*(g.w+2) + dot_x+1 >= 0) and
    (g.diag[(dot_y-1)*(g.w+2) + dot_x+1] <> 0)) or
    (((dot_y-1)*(g.w+2) + dot_x+1 >= 0) and
    (g.hor[(dot_y-1)*(g.w+2) + dot_x+1] <> 0)) or
    (((dot_y)*(g.w+2) + dot_x+1 >= 0) and
    (g.adiag[(dot_y)*(g.w+2) + dot_x+1] <> 0)) or
    ((dot_y*(g.w+1) + dot_x >= 0) and
    (g.vert[dot_y*(g.w+1) + dot_x] <> 0)) or
    ((dot_y*(g.w+2) + dot_x >= 0) and
    (g.diag[dot_y*(g.w+2) + dot_x] <> 0)) or
    (((dot_y-1)*(g.w+2) + dot_x >= 0) and
    (g.hor[(dot_y-1)*(g.w+2) + dot_x] <> 0)) or
    (((dot_y-1)*(g.w+2) + dot_x >= 0) and
    (g.adiag[(dot_y-1)*(g.w+2) + dot_x]  <> 0));
  end;

procedure applyStickAndDot(const move_no, who : Integer; var g : GameState);
  begin
    case move_no of
      0: begin
          g.vert[(g.ball_y-1)*(g.w+1) + g.ball_x] := who;
          g.dots[(g.ball_y-1)*(g.w+1) + g.ball_x] := true;
          dec(g.ball_y);
        end;
      1: begin
          g.diag[(g.ball_y-1)*(g.w+2) + g.ball_x + 1] := who;
          g.dots[(g.ball_y-1)*(g.w+1) + g.ball_x + 1] := true;
          dec(g.ball_y);
          inc(g.ball_x);
        end;
      2: begin
          g.hor[(g.ball_y-1)*(g.w+2) + g.ball_x + 1] := who;
          g.dots[(g.ball_y)*(g.w+1) + g.ball_x + 1] := true;
          inc(g.ball_x); 
        end;
      3: begin
          g.adiag[(g.ball_y)*(g.w+2) + g.ball_x+1] := who;
          g.dots[(g.ball_y+1)*(g.w+1) + g.ball_x + 1] := true;
          inc(g.ball_y); 
          inc(g.ball_x); 
        end;
      4: begin
          g.vert[g.ball_y*(g.w+1) + g.ball_x] := who;
          g.dots[(g.ball_y+1)*(g.w+1) + g.ball_x] := true;
          inc(g.ball_y); 
        end;
      5: begin
          g.diag[g.ball_y*(g.w+2) + g.ball_x] := who;
          g.dots[(g.ball_y+1)*(g.w+1) + g.ball_x-1] := true;
          inc(g.ball_y); 
          dec(g.ball_x); 
        end;
      6: begin
          g.hor[(g.ball_y-1)*(g.w+2) + g.ball_x] := who;
          g.dots[(g.ball_y)*(g.w+1) + g.ball_x-1] := true;
          dec(g.ball_x); 
        end;
      7: begin
          g.adiag[(g.ball_y-1)*(g.w+2) + g.ball_x] := who;
          g.dots[(g.ball_y-1)*(g.w+1) + g.ball_x-1] := true;
          dec(g.ball_x); 
          dec(g.ball_y); 
      end;
    end;
  end;


procedure deapplyStickAndDot(move_no : Integer; var g : GameState);
  begin
    g.finished := false;
    case move_no of
      0: begin
        inc(g.ball_y);
        g.vert[(g.ball_y-1)*(g.w+1) + g.ball_x] := 0;
        g.dots[(g.ball_y-1)*(g.w+1) + g.ball_x] := dotNeeded(g.ball_y - 1,
          g.ball_x, g);
      end;
      1: begin
        inc(g.ball_y); 
        dec(g.ball_x); 
        g.diag[(g.ball_y-1)*(g.w+2) + g.ball_x+1] := 0;
        g.dots[(g.ball_y-1)*(g.w+1) + g.ball_x + 1] := dotNeeded(g.ball_y-1,
          g.ball_x + 1, g);
      end;
      2: begin
        dec(g.ball_x); 
        g.hor[(g.ball_y-1)*(g.w+2) + g.ball_x+1] := 0;
        g.dots[(g.ball_y)*(g.w+1) + g.ball_x + 1] := dotNeeded(g.ball_y,
          g.ball_x + 1, g);
      end;
      3: begin
        dec(g.ball_x); 
        dec(g.ball_y); 
        g.adiag[(g.ball_y)*(g.w+2) + g.ball_x+1] := 0;
        g.dots[(g.ball_y+1)*(g.w+1) + g.ball_x + 1] := dotNeeded(g.ball_y + 1,
          g.ball_x + 1, g);
      end;
      4: begin
        dec(g.ball_y); 
        g.vert[g.ball_y*(g.w+1) + g.ball_x] := 0;
        g.dots[(g.ball_y+1)*(g.w+1) + g.ball_x] := dotNeeded(g.ball_y + 1,
          g.ball_x, g);
      end;
      5: begin
        dec(g.ball_y); 
        inc(g.ball_x);
        g.diag[g.ball_y*(g.w+2) + g.ball_x] := 0;
        g.dots[(g.ball_y+1)*(g.w+1) + g.ball_x-1] := dotNeeded(g.ball_y + 1,
          g.ball_x - 1, g);
      end;
      6: begin
        inc(g.ball_x);
        g.hor[(g.ball_y-1)*(g.w+2) + g.ball_x] := 0;
        g.dots[(g.ball_y)*(g.w+1) + g.ball_x-1] := dotNeeded(g.ball_y,
          g.ball_x-1, g);
      end;
      7: begin
        inc(g.ball_x);
        inc(g.ball_y);
        g.adiag[(g.ball_y-1)*(g.w+2) + g.ball_x] := 0;
        g.dots[(g.ball_y-1)*(g.w+1) + g.ball_x-1] := dotNeeded(g.ball_y-1,
          g.ball_x-1, g);
      end;
    end;
  end;

function goalDot(isMin : Boolean; const g : GameState) : Boolean;
  begin
    if isMin then
      goalDot := (g.ball_y = 0) and ((g.ball_x = (g.w div 2) - 1) or
                                         (g.ball_x = g.w div 2) or
                                         (g.ball_x = (g.w div 2) + 1))
    else 
      goalDot := (g.ball_y = g.h+2) and ((g.ball_x = (g.w div 2) - 1) or 
                                         (g.ball_x = g.w div 2) or
                                         (g.ball_x = (g.w div 2) + 1));
  end;

function isWinningMove(isMin : Boolean; move_no : Integer; 
                       var g : GameState): Boolean;
  begin
    applyStickAndDot(move_no, 3, g);
    isWinningMove := goalDot(isMin, g);
    deapplyStickAndDot(move_no, g);
  end;

function isLosingMove(move_no : Integer; var g : GameState): Boolean;
  begin
    applyStickAndDot(move_no, 3, g);
    isLosingMove := (g.vert[(g.ball_y-1)*(g.w+1) + g.ball_x] <> 0) and
      (g.diag[(g.ball_y-1)*(g.w+2) + g.ball_x+1] <> 0) and 
      (g.hor[(g.ball_y-1)*(g.w+2) + g.ball_x+1] <> 0) and 
      (g.adiag[(g.ball_y)*(g.w+2) + g.ball_x+1] <> 0) and 
      (g.vert[g.ball_y*(g.w+1) + g.ball_x] <> 0) and 
      (g.diag[g.ball_y*(g.w+2) + g.ball_x] <> 0) and 
      (g.hor[(g.ball_y-1)*(g.w+2) + g.ball_x] <> 0) and 
      (g.adiag[(g.ball_y-1)*(g.w+2) + g.ball_x] <> 0);
    deapplyStickAndDot(move_no, g);
  end;

function isPossible(move_no : Integer; isMin : Boolean; 
                    var game : GameState) : Option;
  begin
    if (existsLine(move_no, game.ball_x, game.ball_y, game)) then
      isPossible := no
    else 
      if (isWinningMove(not(isMin), move_no, game)) then
        isPossible := loss
      else if (isWinningMove(isMin, move_no, game)) then
        isPossible := win
      else if (isLosingMove(move_no, game)) then
        isPossible := loss
      else if (existsDot(move_no, game)) then
        isPossible := nonfinal
      else 
        isPossible := yes;
  end;

procedure writeBooleanArrayToFile(const A : Array of Boolean; n, m : Integer; 
                           var f : TextFile);
  var
    i, j : Integer;
  begin
    for i := 0 to n-1 do begin
      for j := 0 to m-1 do
        if a[i*m + j] then
          write(f, 1 ,' ')
        else
          write(f, 0 ,' ');
      writeln(f);
    end;
  end;

procedure ReadBooleanArrayFromFile(var A : Array of Boolean; n, m : Integer; 
                           var f : TextFile);
  var
    i, j, cur : Integer;
  begin
    for i := 0 to n-1 do begin
      for j := 0 to m-1 do begin
        read(f, cur);
        if cur = 1 then
          a[i*m + j] := true
        else
          a[i*m + j] := false;
      end;
    end;
  end;

procedure writeArrayToFile(const A : Array of Integer; n, m : Integer; 
                           var f : TextFile);
  var
    i, j : Integer;
  begin
    for i := 0 to n-1 do begin
      for j := 0 to m-1 do
          write(f, a[i*m + j], ' ');
      writeln(f);
    end;
  end;

procedure readArrayFromFile(var A : Array of Integer; n, m : Integer; 
                            var f : TextFile);
var
  i, j, k : Integer;
begin
  for i := 0 to n-1 do begin
    for j := 0 to m-1 do begin
        read(f, k);
        a[i*m + j] := k;
      end;
  end;
end;
  
  
procedure writeGameToFile(const game : GameState; var f : TextFile);
  begin
    Rewrite(f);
    writeln(f, 'player_up');
    writeln(f, game.player_up);
    writeln(f, 'player_down');
    writeln(f, game.player_down);
    writeln(f, 'h');
    writeln(f, game.h);
    writeln(f, 'w');
    writeln(f, game.w);
    writeln(f, 'up_turn');
    if game.up_turn then
      writeln(f, 1)
    else
      writeln(f, 0);
    writeln(f, 'finished');
    if game.finished then
      writeln(f, 1)
    else
      writeln(f, 0);
    writeln(f, 'ball_x');
    writeln(f, game.ball_x);
    writeln(f, 'ball_y');
    writeln(f, game.ball_y);
    writeln(f, 'dots');
    writeBooleanArrayToFile(game.dots, game.h+3, game.w+1, f);
    writeln(f, 'vert');
    writeArrayToFile(game.vert, game.h+2, game.w+1, f);
    writeln(f, 'hor');
    writeArrayToFile(game.hor, game.h+1, game.w+2, f);
    writeln(f, 'diag');
    writeArrayToFile(game.diag, game.h+2, game.w+2, f);
    writeln(f, 'adiag');
    writeArrayToFile(game.adiag, game.h+2, game.w+2, f);
    Close(f);
  end;

procedure readGameFromFile(var game : GameState; var f : TextFile);
  var
    i : Integer;
  begin
    Reset(f);
    readln(f);
    readln(f, game.player_up);
    readln(f);
    readln(f, game.player_down);
    readln(f);
    readln(f, game.h);
    readln(f);
    readln(f, game.w);
    readln(f);
    readln(f, i);
    game.up_turn := (i <> 0);
    readln(f);
    readln(f, i);
    game.finished := (i <> 0);
    readln(f);
    readln(f, game.ball_x);
    readln(f);
    readln(f, game.ball_y);
    readln(f);
    ReadBooleanArrayFromFile(game.dots, game.h+3, game.w+1, f);
    readln(f);
    readln(f);
    ReadArrayFromFile(game.vert, game.h+2, game.w+1, f);
    readln(f);
    readln(f);
    ReadArrayFromFile(game.hor, game.h+1, game.w+2, f);
    readln(f);
    readln(f);
    ReadArrayFromFile(game.diag, game.h+2, game.w+2, f);
    readln(f);
    readln(f);
    ReadArrayFromFile(game.adiag, game.h+2, game.w+2, f);
    Close(f);
  end;

procedure switchTurns(var game : GameState);
  begin
    game.up_turn := not(game.up_turn);
  end;

end.
