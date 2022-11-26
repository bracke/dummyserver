with AAA.Strings;
private with GNAT.Strings;
with Version;

package Commands.Serve is

   type Instance
   is new CLIC.Subcommand.Command
   with private;

   overriding
   function Name (Cmd : Instance) return
      CLIC.Subcommand.Identifier is ("serve");

   overriding procedure Execute
   (Cmd : in out Instance; Args : AAA.Strings.Vector);

   overriding
   function Switch_Parsing (This : Instance)
                            return CLIC.Subcommand.Switch_Parsing_Kind
   is (CLIC.Subcommand.Parse_All);

   overriding
   function Long_Description
   (Cmd : Instance) return AAA.Strings.Vector is
   (AAA.Strings.Empty_Vector.Append
     ("Start the " & Version.Name)
     .New_Line
     );

   overriding procedure Setup_Switches
   (Cmd    : in out Instance;
    Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding function Short_Description (Cmd : Instance) return
      String is ("Start the " & Version.Name);

   overriding function Usage_Custom_Parameters (Cmd : Instance)
   return String is ("<path to json config file>");

private

   type Instance is new CLIC.Subcommand.Command with record
      Config_File : aliased GNAT.Strings.String_Access;
      Port_Option : aliased Integer := 8080;
   end record;

end Commands.Serve;