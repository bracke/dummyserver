with CLIC.TTY;

package body Version is

   package TT renames CLIC.TTY;

   function Title return String is
      (TT.Bold (Name) & " (ds) Version " & TT.Emph (Current));

end Version;