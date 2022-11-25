
with GNAT.Sockets;      use GNAT.Sockets;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;

with Ada.Directories; use Ada.Directories;
with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Text_IO;

with AAA.Strings;
with CLIC.TTY;

with Configuration;
with Server;
with Version;

procedure Dummyserver is

   package IO renames Ada.Text_IO;
   package TT renames CLIC.TTY;

   Config      : Command_Line_Configuration;
   Q_Enabled   : aliased Boolean := False;
   Display_Version : aliased Boolean := False;
   Port_Option : aliased Integer := 0;
   Resources   : Configuration.Resource_List_Type.Map;
begin
   CLIC.TTY.Enable_Color (Force => False);

   Define_Switch
     (Config, Port_Option'Access, "-p:", Long_Switch => "--port:", Help => "Provide port number, default is 8080");

   Define_Switch
     (Config, Q_Enabled'Access, "-q", Long_Switch => "--quiet",
      Help => "Limit output to errors");

   Define_Switch
     (Config, Display_Version'Access, "-v", Long_Switch => "--version", Help => "Displays version and exits");

   Getopt (Config);

   if Display_Version then
      IO.New_Line;
      IO.Put_Line (Version.Title);
      IO.Put_Line (TT.Info (TT.Underline (Version.Link)));
      IO.New_Line;
      GNAT.OS_Lib.OS_Exit (0);
   end if;

   declare
      Filepath : constant String := Get_Argument;
   begin
      if Filepath'Length = 0 then
         IO.Put_Line
           (TT.Warn ("Please provide a configuration file as argument"));
         IO.Put_Line
           (TT.Info ("The configuration file must be in json format."));
         IO.New_Line;
         IO.Put_Line ("Example: " & TT.Terminal ("ds config.json"));
         IO.New_Line;
      else
         if Exists (Filepath) then
            Resources := Configuration.Read_Configuration (Filepath);
            if Port_Option = 0 then
               Port_Option := 8080;
            end if;
            Server.Start (Resources, Port_Type'Val (Port_Option), Q_Enabled);
         else
            IO.Put_Line (TT.Warn ("Configuration file not found"));
         end if;
      end if;
   end;
exception
   when GNAT.Command_Line.Exit_From_Command_Line =>
      IO.New_Line;
      IO.Put_Line
        (TT.Warn ("Please provide a configuration file as argument"));
      IO.Put_Line (TT.Info ("The configuration file must be in json format."));
      IO.New_Line;
      IO.Put_Line (TT.Terminal ("ds <file>"));
      IO.New_Line;

   when E : others =>
      IO.New_Line;
      IO.Put_Line (TT.Error (Exception_Message (E)));
      IO.New_Line;

end Dummyserver;