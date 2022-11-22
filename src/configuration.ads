with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Configuration is

   type Resource is record
      Name : Unbounded_String;
      Content : Unbounded_String;
      Content_Type : Unbounded_String;
   end record;

   type Resource_Access_Type is access Resource;

   function String_Hash (X : Unbounded_String) return Hash_Type;

   package Resource_List_Type is new Ada.Containers.Indefinite_Hashed_Maps (
      Element_Type => Resource_Access_Type,
      Key_Type => Unbounded_String,
      Hash => String_Hash,
      equivalent_keys => "="
   );

   function Read_Configuration (File_Path : String)
      return Resource_List_Type.Map;

private
end Configuration;