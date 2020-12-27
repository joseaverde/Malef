with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Malef.Wrapper;

procedure Test_2 is
   type Param_Type is
      record
         Name : Unbounded_String;
      end record;

   type Return_Type is null record;

   function Main (Params : Param_Type)
                  return Return_Type is
      Ret : Return_Type;
   begin

      Ada.Text_IO.Put_Line("Hello " & To_String(Params.Name));
      delay 1.0;

      return Ret;

   end Main;

   Ret : Return_Type;

   function Wrapped_Main is new Malef.Wrapper(Param_Type, Return_Type,
                                              Main'Access);

begin

   Ada.Text_IO.Put("Write your name: ");
   Ret := Wrapped_Main(Param_Type'(
                  Name => To_Unbounded_String(Ada.Text_IO.Get_Line)));

end Test_2;

