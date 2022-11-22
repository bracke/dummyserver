with GNAT.Sockets; use GNAT.Sockets;
with Configuration;

package Server is

   procedure Start
     (Resources : Configuration.Resource_List_Type.Map;
      Port : Port_Type := 8080;
      Quiet : Boolean := False);

private
end Server;