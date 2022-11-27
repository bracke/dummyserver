with AAA.Strings;
private with GNAT.Strings;

package Commands.Stringify is

   type Instance
   is new CLIC.Subcommand.Command
   with private;

   overriding
   function Name (Cmd : Instance) return
      CLIC.Subcommand.Identifier is ("stringify");

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
     ("Converts the contents of a text file into a single line string")
     .New_Line
   );

   overriding function Short_Description (Cmd : Instance) return String is ("Converts the contents of a text file into a single line string");

   overriding function Usage_Custom_Parameters (Cmd : Instance)
   return String is ("<path to text file>");

private

   type Instance is new CLIC.Subcommand.Command with null record;

end Commands.Stringify;