with Ada.Text_IO;
with Ada.Directories;
with Ada.Containers; use Ada.Containers;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with CLIC.Subcommand;
with CLIC.TTY;

package body Commands.Stringify is

   use Ada.Strings.Maps;
   use Ada.Strings.Maps.Constants;

   package IO renames Ada.Text_IO;
   package TT renames CLIC.TTY;
   package DIR renames Ada.Directories;
   package AA renames AAA.Strings;
   package SIO renames Ada.Streams.Stream_IO;

   Line_endings : constant Character_Set :=
      Control_Set and To_Set (ASCII.LF & ASCII.CR & Character'Val (0));

   Quotes : constant Character_Set := To_Set ("""" & "'");

   Whitespace : constant Character_Set := To_Set (" " & ASCII.HT);

   function Convert (Buffer : Ada.Streams.Stream_Element_Array; Remove_Whitespace : Boolean := False) return String is

      Result : Unbounded_String;
      Current_Character : Character;
      Useless_Characters : Character_Set := Line_endings;
   begin
      if Remove_Whitespace then
         Useless_Characters := Ada.Strings.Maps."or" (Useless_Characters, Whitespace);
      end  if;
      for aValue in Buffer'Range loop
         Current_Character := Character'Val (Buffer (aValue));
         if Is_In (Current_Character, Quotes) then

            Result := Result & "\""";

         elsif not Is_In (Current_Character, Useless_Characters) then

            Result := Result & Current_Character;

         end if;
      end loop;
      return To_String (Result);
   end Convert;

   overriding
   procedure Execute (Cmd  : in out Instance;
                      Args :        AAA.Strings.Vector) is

      No_Argument : constant String := AA.Flatten (AA.Empty_Vector
         .Append (TT.Warn ("Please provide the path to a text file as argument"))
         .New_Line
         .Append ("Example: " & TT.Terminal ("ds stringify response.json"))
         .New_Line);
   begin
      if Args.Length = 0 then
         IO.Put_Line (No_Argument);
      else
         declare
            Filepath : constant String := Args (1);
         begin
            if Filepath'Length = 0 then
               IO.Put_Line (No_Argument);
            else
               if DIR.Exists (Filepath) then
                  declare
                     File : SIO.File_Type;
                     Buffer : Ada.Streams.Stream_Element_Array (1 .. 4096);
                     Bytes_Used : Ada.Streams.Stream_Element_Count;
                  begin
                     SIO.Open (File, SIO.In_File, Filepath);
                     while not SIO.End_Of_File (File) loop
                        SIO.Read (File, Buffer, Bytes_Used);
                        declare
                           Raw : constant Ada.Streams.Stream_Element_Array := Buffer (1 .. Bytes_Used);
                           Encoded : constant String := Convert (Raw, Cmd.Remove_Whitespace);
                        begin
                           IO.Put (Encoded);
                        end;
                     end loop;
                     SIO.Close (File);
                  end;
               else
                  IO.Put_Line (TT.Warn ("File not found"));
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
      (Config, Cmd.Remove_Whitespace'Access, "-w", Long_Switch => "--nowhitespace",
         Help => "Removes blanks and tabs");

   end Setup_Switches;

end Commands.Stringify;