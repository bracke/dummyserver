
with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Text_IO;

with AAA.Strings;
with CLIC.TTY;

with Commands;

procedure Dummyserver is

   package IO renames Ada.Text_IO;
   package TT renames CLIC.TTY;

begin
   Commands.Execute;

exception
   when E : others =>
      IO.New_Line;
      IO.Put_Line (TT.Error (Exception_Message (E)));
      IO.New_Line;

end Dummyserver;