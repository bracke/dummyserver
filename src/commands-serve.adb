with Ada.Text_IO;
with Ada.Directories;
with Ada.Containers; use Ada.Containers;

with CLIC.Subcommand;
with CLIC.TTY;

with GNAT.Sockets; use GNAT.Sockets;

with Configuration;
with Server;

package body Commands.Serve is

   package IO renames Ada.Text_IO;
   package TT renames CLIC.TTY;
   package DIR renames Ada.Directories;
   package AA renames AAA.Strings;

   overriding
   procedure Execute (Cmd  : in out Instance;
                      Args :        AAA.Strings.Vector) is

      No_Argument : constant String := AA.Flatten (AA.Empty_Vector
         .Append (TT.Warn ("Please provide a configuration file as argument"))
         .Append (TT.Info ("The configuration file must be in json format."))
         .New_Line
         .Append ("Example: " & TT.Terminal ("ds serve config.json"))
         .New_Line);
   begin
      if Args.Length = 0 then
         IO.Put_Line (No_Argument);
      else
         declare
            Filepath : constant String := Args (1);
            Resources : Configuration.Resource_List_Type.Map;
         begin
            if Filepath'Length = 0 then
               IO.Put_Line (No_Argument);
            else
               if DIR.Exists (Filepath) then
                  Resources := Configuration.Read_Configuration (Filepath);
                  if Cmd.Port_Option = 0 then
                     Cmd.Port_Option := 8080;
                  end if;
                  Server.Start (Resources, Port_Type'Val (Cmd.Port_Option), Is_Quiet);
               else
                  IO.New_Line;
                  IO.Put_Line (TT.Warn ("Configuration file not found"));
                  IO.New_Line;
               end if;
            end if;
         end;
      end if;
   end Execute;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Instance;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
   begin
      Define_Switch
      (Config, Cmd.Port_Option'Access, "-p:", Long_Switch => "--port:",
         Help => "Provide port number, default is 8080");

   end Setup_Switches;

end Commands.Serve;