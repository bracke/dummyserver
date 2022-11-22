pragma Ada_2022;
with JSON.Parsers;
with JSON.Types;
with Ada.Text_IO;
with Ada.Strings.Unbounded.Hash_Case_Insensitive;

package body Configuration is

   package IO renames Ada.Text_IO;

   package Types is new JSON.Types (Long_Integer, Long_Float);
   package Parsers is new JSON.Parsers (Types);

   function To_U
     (Source : String) return Ada.Strings.Unbounded.Unbounded_String renames
     Ada.Strings.Unbounded.To_Unbounded_String;

   function String_Hash (X : Unbounded_String) return Hash_Type is
   begin
      return Ada.Strings.Unbounded.Hash_Case_Insensitive (X);
   end String_Hash;

   function Read_Configuration
     (File_Path : String) return Resource_List_Type.Map
   is

      Parser    : Parsers.Parser := Parsers.Create_From_File (File_Path);
      Value     : constant Types.JSON_Value := Parser.Parse;
      Resources : Configuration.Resource_List_Type.Map;
      use Types;
   begin
      if Value.Kind = Array_Kind and then Value.Length > 0 then
         for Element of Value loop
            if Element.Kind = Object_Kind and then
               Element.Contains ("name") then

               declare
                  A_Resource : Resource_Access_Type;
                  Content : Unbounded_String := Null_Unbounded_String;
                  Content_Type : Unbounded_String := To_U ("text/plain; charset=iso-8859-1");
               begin
                  if Element.Contains ("content") then
                     Content := To_U (Element ("content").Value);
                  end if;
                  if Element.Contains ("content") then
                     Content_Type := To_U (Element ("content-type").Value);
                  end if;

                  A_Resource :=
                     new Configuration.Resource'
                        (Name => To_U (Element ("name").Value),
                         Content => Content,
                         Content_Type => Content_Type
                         );

                  if not Resources.Contains (A_Resource.Name) then
                     Resources.Insert (A_Resource.Name, A_Resource);
                  else
                     IO.Put_Line
                     ("Duplicate resource definition: " &
                        Ada.Strings.Unbounded.To_String (A_Resource.Name));
                  end if;
               end;
            end if;
         end loop;
      else
         IO.Put_Line ("No resource definitions found");
      end if;
      return Resources;
   end Read_Configuration;

end Configuration;