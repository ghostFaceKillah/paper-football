unit buffers;

interface

uses game_state;

const
  scale = 20;

type
  line_elem = record
    x1, x2, y1, y2, what : Integer;
  end;
  dot_elem = record
    x, y, what : Integer;
  end;

var
  line_buffer : Array [0..6*MAX_H*MAX_W] of line_elem;
  dot_buffer : Array [0..MAX_H*MAX_W] of dot_elem;
  line_buffer_end : Integer;
  dot_buffer_end : Integer;

procedure make_buffer(const game : GameState);
procedure printBuffers();
procedure extract_vert_line(const i, j, col : Integer; var l :line_elem);
procedure extract_hor_line(const i, j, col : Integer;  var l :line_elem);
procedure extract_diag_line(const i, j, col : Integer;  var l :line_elem);
procedure extract_adiag_line(const i, j, col : Integer;  var l :line_elem);
  
implementation

procedure extract_vert_line(const i, j, col : Integer; var l :line_elem);
  begin
    l.x1 := j*scale + scale;
    l.y1 := i*scale + scale;
    l.x2 := j*scale + scale;
    l.y2 := (i+1)*scale + scale;
    l.what := col;
  end;

procedure extract_hor_line(const i, j, col : Integer;  var l :line_elem);
  begin
    l.x1 := (j-1)*scale + scale;
    l.y1 := (i+1)*scale + scale + 2;
    l.x2 := j*scale + scale;
    l.y2 := (i+1)*scale + scale + 2;
    l.what := col;
  end;

procedure extract_diag_line(const i, j, col : Integer;  var l :line_elem);
  begin
    l.x1 := j*scale + scale;
    l.y1 := i*scale + scale + 2;
    l.x2 := (j-1)*scale + scale;
    l.y2 := (i+1)*scale + scale + 2;
    l.what := col;
  end;

procedure extract_adiag_line(const i, j, col : Integer;  var l :line_elem);
  begin
    l.x1 := (j-1)*scale + scale;
    l.y1 := i*scale + scale + 2;
    l.x2 := j*scale + scale;
    l.y2 := (i+1)*scale + scale + 2;
    l.what := col;
  end;

procedure make_buffer(const game : GameState);
  var
    m, n : Integer;
    i, j : Integer;
  begin
    line_buffer_end := 0;
    dot_buffer_end := 0;
    n := game.h + 2;
    m := game.w + 1;
    // make vertical lines
    for i := 0 to n-1 do
      for j := 0 to m-1 do 
        if (game.vert[i*m + j] <> 0) then begin
          extract_vert_line(i, j, game.vert[i*m + j], line_buffer[line_buffer_end]);
          inc(line_buffer_end);
        end;

    n := game.h + 1;
    m := game.w + 2;
    for i := 0 to n-1 do
      for j := 0 to m-1 do 
        if (game.hor[i*m + j] <> 0) then begin
          extract_hor_line(i, j, game.hor[i*m + j], line_buffer[line_buffer_end]);
          inc(line_buffer_end);
        end;

    n := game.h + 2;
    m := game.w + 2;
    for i := 0 to n-1 do
      for j := 0 to m-1 do 
        if (game.diag[i*m + j] <> 0) then begin
          extract_diag_line(i, j, game.diag[i*m + j],
            line_buffer[line_buffer_end]);
          inc(line_buffer_end);
        end;

    n := game.h + 2;
    m := game.w + 2;
    for i := 0 to n-1 do
      for j := 0 to m-1 do 
        if (game.adiag[i*m + j] <> 0) then begin
          extract_adiag_line(i, j, game.adiag[i*m + j],
            line_buffer[line_buffer_end]);
          inc(line_buffer_end);
        end;

    n := game.h + 3;
    m := game.w + 1;
    for i := 0 to n-1 do
      for j := 0 to m-1 do begin
        dot_buffer[dot_buffer_end].x := j*scale + scale - 2;
        dot_buffer[dot_buffer_end].y := i*scale + scale;
        if game.dots[i*m + j] then
          dot_buffer[dot_buffer_end].what := 1
        else
          dot_buffer[dot_buffer_end].what := 0;
        inc(dot_buffer_end);
      end;
  end;

procedure printBuffers();
var
  i : INteger;
begin
  writeln();
  writeln('lines');
  writeln();
  for i := 0 to line_buffer_end do begin
    writeln(
      line_buffer[i].x1, ' ',
      line_buffer[i].y1, ' ',
      line_buffer[i].x2, ' ',
      line_buffer[i].y2);
  end;
  writeln();
  writeln('dots');
  writeln();
  for i := 0 to dot_buffer_end do begin
    writeln(dot_buffer[i].x, ' ', dot_buffer[i].y);
  end;
end;

end.
