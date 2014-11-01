program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, game_window, GR32_L, editor,
new_game_settings_code, welcome_menu
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(Twelcome_window, welcome_window);
  Application.CreateForm(Tmain, main);
  Application.CreateForm(Teditor, editor_instance);
  Application.CreateForm(Tnew_game_settings, new_game_settings);
  Application.Run;
end.

