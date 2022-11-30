with AAA.Strings;
with CLIC.Subcommand;
with CLIC.TTY;

package Commands.Topics.Issues is

   package TT renames CLIC.TTY;

   type Topic is new CLIC.Subcommand.Help_Topic with null record;

   overriding
   function Name (This : Topic) return CLIC.Subcommand.Identifier
   is ("issues");

   overriding
   function Title (This : Topic) return String
   is ("Reporting bugs and feature requests and other issues.");

   overriding
   function Content (This : Topic) return AAA.Strings.Vector is (AAA.Strings.Empty_Vector.New_Line
   .Append ("Think you've found a bug or have a new feature to suggest? Let me know!")
   .New_Line
   .Append ("Please create issues on the " & Version.Name & " project's GitHub page: ")
   .Append (TT.URL (Version.Link))
   .New_Line
   .Append ("The issue should contain:")
   .New_Line
   .Append ("- A description.")
   .Append ("- Any error message displayed.")
   .Append ("- The output in the terminal/console.")
   .Append ("- How ds was started - the full command.")
   .Append ("- The configuration file used.")
   .Append ("- The url used in the browser.")
   .New_Line
   .Append ("Providing these details will help enormously with finding the cause of the problems and thus make it much more likely that a fix can be found.")
   .New_Line
   );

end Commands.Topics.Issues;