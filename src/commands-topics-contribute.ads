with AAA.Strings;
with CLIC.Subcommand;
with CLIC.TTY;
with Version;
package Commands.Topics.Contribute is

   package TT renames CLIC.TTY;

   type Topic is new CLIC.Subcommand.Help_Topic with null record;

   overriding
   function Name (This : Topic) return CLIC.Subcommand.Identifier
   is ("contribute");

   overriding
   function Title (This : Topic) return String
   is ("How to contribute to the " & Version.Name & " project.");

   overriding
   function Content (This : Topic) return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector.New_Line
   .Append
   ("Please consider contributing to this project. Not just for code, but also for adding to the documentation or fixing spelling mistakes.")
   .New_Line
   .Append ("Please create pull request on the " & Version.Name & " project's GitHub page: ")
   .Append (TT.URL (Version.Link))
   .New_Line
   .Append ("You can contribute to this project by creating pull request with your changes. You can use Git or GitHub for that. For small changes, the edit function in GitHub is probably the easiest option:")
   .New_Line
   .Append ("- View the project on GitHub.")
   .Append ("- Select the 'Code' tab.")
   .Append ("- Find the file you want to edit.")
   .Append ("- Click on the pen icon.")
   .Append ("- Do the changes You want.")
   .Append ("- Fill in the form in the bottom and click on 'Propose changes'.")
   .New_Line);

end Commands.Topics.Contribute;