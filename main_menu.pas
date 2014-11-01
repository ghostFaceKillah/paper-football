unit main_menu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { Tmenu }

  Tmenu = class(TForm)
    newgame: TButton;
    open: TButton;
    edit: TButton;
    quit: TButton;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  menu: Tmenu;

implementation

{$R *.lfm}

end.

