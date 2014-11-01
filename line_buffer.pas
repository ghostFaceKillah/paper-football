unit line_buffer;

interface

uses game_state;

const
  scale = 20;

type
  line_elem = record
    x1, x2, y1, y2, what : Integer;
  end;

var
  buffer : Array [0..6*MAX_H*MAX_W] of line_elem;
  buffer_end : Integer;

procedure make_buffer(const game : GameState);

implementation

procedure extract_vert_line(const i, j, col : Integer; var l :line_elem);
  begin
    l.x1 := j*scale + scale;
    l.y1 := i*scale + scale;
    l.x2 := j*scale + scale;
    l.y2 := (i+1)*scale + scale;
  end;

procedure extract_hor_line(const i, j, col : Integer;  var l :line_elem);
  begin
    l.x1 := (j-1)*scale + scale;
    l.y1 := (i+1)*scale + scale + 2;
    l.x2 := j*scale + scale;
    l.y2 := (i+1)*scale + scale + 2;
  end;

procedure extract_diag_line(const i, j, col : Integer;  var l :line_elem);
  begin
    l.x1 := j*scale + scale;
    l.y1 := i*scale + scale + 2;
    l.x2 := (j-1)*scale + scale;
    l.y2 := (i+1)*scale + scale + 2;
  end;

procedure extract_adiag_line(const i, j, col : Integer;  var l :line_elem);
  begin
    l.x1 := (j-1)*scale + scale;
    l.y1 := i*scale + scale + 2;
    l.x2 := j*scale + scale;
    l.y2 := (i+1)*scale + scale + 2;
  end;

procedure make_buffer(const game : GameState);
  var
    m, n : Integer;
    i, j : Integer;
  begin
    buffer_end := 0;
    n := game.h + 2;
    m := game.w + 1;
    // make vertical lines
    for i := 0 to n-1 do
      for j := 0 to m-1 do 
        if (game.vert[i*m + j] <> 0) then begin
          extract_vert_line(i, j, game.vert[i*m + j], buffer[buffer_end]);
          inc(buffer_end);
        end;

    n := game.h + 1;
    m := game.w + 2;
    for i := 0 to n-1 do
      for j := 0 to m-1 do 
        if (game.hor[i*m + j] <> 0) then begin
          extract_hor_line(i, j, game.hor[i*m + j], buffer[buffer_end]);
          inc(buffer_end);
        end;

    n := game.h + 2;
    m := game.w + 2;
    for i := 0 to n-1 do
      for j := 0 to m-1 do 
        if (game.diag[i*m + j] <> 0) then begin
          extract_diag_line(i, j, game.diag[i*m + j], buffer[buffer_end]);
          inc(buffer_end);
        end;

    n := game.h + 2;
    m := game.w + 2;
    for i := 0 to n-1 do
      for j := 0 to m-1 do 
        if (game.adiag[i*m + j] <> 0) then begin
          extract_adiag_line(i, j, game.adiag[i*m + j], buffer[buffer_end]);
          inc(buffer_end);
        end;
  end;

end.
