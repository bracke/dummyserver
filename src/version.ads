package Version with Preelaborate
is
   Current : constant String := "1.1.0";
   Name    : constant String := "DummyServer";
   Link    : constant String := "https://github.com/bracke/dummyserver";
   Command : constant String := "ds";

   function Title return String;

end Version;