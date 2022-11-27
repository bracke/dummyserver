with Commands.Topics.Issues;
with Commands.Topics.Contribute;
with Commands.Serve;
with Commands.Base64;
with Commands.Stringify;

with CLIC.User_Input;

package body Commands is

   package IO renames Ada.Text_IO;
   package TT renames CLIC.TTY;

   Help_Switch : aliased Boolean := False;
   Q_Enabled   : aliased Boolean := False;

   No_Color : aliased Boolean := False;
   --  Force-disable color output

   No_TTY : aliased Boolean := False;
   --  Used to disable control characters in output

   Version_Only : aliased Boolean := False;
   --  Just display the current version and exit

   function Is_Quiet return Boolean is (Q_Enabled);

   -------------------------
   -- Set_Global_Switches --
   -------------------------

   procedure Set_Global_Switches
     (Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;

   begin
      Define_Switch
        (Config, Help_Switch'Access, "-h", "--help",
         "Display general or command-specific help");

      Define_Switch
        (Config, Q_Enabled'Access, "-q", Long_Switch => "--quiet",
         Help => "Limit output to errors");

      Define_Switch
        (Config, Version_Only'Access, "-v", Long_Switch => "--version",
         Help => "Displays version and exits");

      Define_Switch
        (Config, No_Color'Access, Long_Switch => "--no-color",
         Help => "Disables colors in output");

      Define_Switch
        (Config, No_TTY'Access, Long_Switch => "--no-tty",
         Help => "Disables control characters in output");

   end Set_Global_Switches;

   -------------
   -- Execute --
   -------------

   procedure Execute is
   begin
      Sub_Cmd.Parse_Global_Switches;

      if No_TTY then
         TT.Force_Disable_TTY;
      end if;

      if not No_Color and then not No_TTY then
         CLIC.TTY.Enable_Color (Force => False);
         --  This may still not enable color if TTY is detected to be incapable
         --  Simple_Logging.ASCII_Only := False;
         --  Also use a fancier busy spinner
      end if;

      if Version_Only then
         IO.New_Line;
         IO.Put_Line (Version.Title);
         IO.Put_Line (TT.Info (TT.URL (Version.Link)));
         IO.New_Line;
         return;
      end if;

      begin
         Sub_Cmd.Execute;
      exception
         when Child_Failed | Command_Failed | Wrong_Command_Arguments =>
            GNAT.OS_Lib.OS_Exit (1);
         when CLIC.User_Input.User_Interrupt =>
            GNAT.OS_Lib.OS_Exit (1);
      end;
   end Execute;
begin
   -- Commands --
   Sub_Cmd.Register ("Serve", new Serve.Instance);
   Sub_Cmd.Register ("General", new Sub_Cmd.Builtin_Help);
   Sub_Cmd.Register ("Tools", new Base64.Instance);
   Sub_Cmd.Register ("Tools", new Stringify.Instance);

   -- Help topics --
   Sub_Cmd.Register (new Topics.Issues.Topic);
   Sub_Cmd.Register (new Topics.Contribute.Topic);

end Commands;