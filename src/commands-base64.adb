with Ada.Text_IO;
with Ada.Directories;
with Ada.Containers; use Ada.Containers;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;

with CLIC.Subcommand;
with CLIC.TTY;

with Base64.Standard; use Base64.Standard;

package body Commands.Base64 is

   package IO renames Ada.Text_IO;
   package TT renames CLIC.TTY;
   package DIR renames Ada.Directories;
   package AA renames AAA.Strings;
   package SIO renames Ada.Streams.Stream_IO;

   overriding
   procedure Execute (Cmd  : in out Instance;
                      Args :        AAA.Strings.Vector) is

      No_Argument : constant String := AA.Flatten (AA.Empty_Vector
         .Append (TT.Warn ("Please provide the path to a binary file as argument"))
         .New_Line
         .Append ("Example: " & TT.Terminal ("ds base64 qr.png"))
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
                           Encoded : constant String := Encode (Raw);
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

end Commands.Base64;