
with GNAT.Sockets;      use GNAT.Sockets;
with GNAT.Command_Line; use GNAT.Command_Line;

with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;

with Configuration;
with Server;

procedure Dummyserver is

   package IO renames Ada.Text_IO;

   Config      : Command_Line_Configuration;
   Q_Enabled   : aliased Boolean := False;
   Port_Option : aliased Integer := 0;
   Resources   : Configuration.Resource_List_Type.Map;
begin
   Define_Switch
     (Config,
     Q_Enabled'Access,
     "-q",
     Long_Switch => "--quiet",
      Help => "Limit output to errors");

   Define_Switch
     (Config,
     Port_Option'Access,
     "-p:",
     Long_Switch => "--port:",
      Help => "Provide port number, default is 8080");

   Getopt (Config);

   declare
      Filepath : constant String := Get_Argument;
   begin
      if Filepath'Length = 0 then
         IO.Put_Line ("Please provide a configuration file as argument");
         IO.Put_Line ("The configuration file must be in json format.");
         IO.New_Line;
         IO.Put_Line ("Example: ds config.json");
         IO.New_Line;
      else
         if Exists (Filepath) then
            Resources := Configuration.Read_Configuration (Filepath);
            if Port_Option = 0 then
               Port_Option := 8080;
            end if;
            Server.Start (Resources, Port_Type'Val (Port_Option), Q_Enabled);
         else
            IO.Put_Line ("Configuration file not found");
         end if;
      end if;
   end;
exception
   when GNAT.Command_Line.Exit_From_Command_Line =>
      IO.New_Line;
      IO.Put_Line ("Please provide a configuration file as argument");
      IO.Put_Line ("The configuration file must be in json format.");
      IO.New_Line;
      IO.Put_Line ("ds <file>");
      IO.New_Line;
end Dummyserver;