pragma Ada_2022;
with Ada.IO_Exceptions, Ada.Text_IO, Ada.Strings.Unbounded, Ada.Strings.Fixed, Ada.Strings;
with GNAT.Sockets.Convenience;
with Black.Request, Black.Response;

package body Server is

   function To_U
     (Source : String) return Ada.Strings.Unbounded.Unbounded_String renames
     Ada.Strings.Unbounded.To_Unbounded_String;

   package IO renames Ada.Text_IO;
   use Configuration.Resource_List_Type;

   procedure Start
     (Resources : Configuration.Resource_List_Type.Map;
      Port      : Port_Type := 8080;
      Quiet     : Boolean := False)
   is

      Listener : Socket_Type;
      Message  : constant String :=
        "Serving on http://localhost:" &
        Ada.Strings.Fixed.Trim (Port'Image, Ada.Strings.Left);
   begin
      Listener := Convenience.Make_Server (Port => Port);
      IO.Put_Line (Message);

      loop
         declare
            Connection : Socket_Type;
            Client     : Sock_Addr_Type;
         begin
            Accept_Socket
              (Server => Listener, Socket => Connection, Address => Client);

            declare
               Request : constant Black.Request.Instance :=
                 Black.Request.Parse_HTTP (Stream (Connection));

               use Black.Response;

               A_Resource :
                 Configuration.Resource;
            begin
               if not Quiet then
                  IO.Put (Request.Resource & " -> ");
               end if;

               if Contains (Resources, To_U (Request.Resource)) then
                  A_Resource := Element (Resources, To_U (Request.Resource)).all;
                  Instance'Output
                    (Stream (Connection),
                     OK (Content_Type => Ada.Strings.Unbounded.To_String (A_Resource.Content_Type),
                        Data => Ada.Strings.Unbounded.To_String (A_Resource.Content)));

                  if not Quiet then
                     IO.Put_Line (Ada.Strings.Unbounded.To_String (A_Resource.Content));
                  end if;
               else
                  Instance'Output
                    (Stream (Connection), Not_Found (Resource => Request.Resource));

                  IO.Put_Line ("Not found");
               end if;
            end;

            Close_Socket (Socket => Connection);
         exception
            when Ada.IO_Exceptions.End_Error =>
               null;
         end;
      end loop;
   end Start;

end Server;