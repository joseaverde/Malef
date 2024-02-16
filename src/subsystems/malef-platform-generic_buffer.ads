with Ada.Streams;

generic
   Capacity : in Positive;
   Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
package Malef.Platform.Generic_Buffer is

   -- We can't use Ada.Text_IO to print the buffer, because for whatever reason
   -- printing UTF-8 with Ada.Text_IO outputs garbage if we compile with the
   -- `-gnatW8' flag (required for Wide_Wide_String s) on GNAT.
   --
   -- Also we can't use Ada.Wide_Wide_Text_IO, because it is slow as hell
   -- (maybe because it needs to transform the Unicode back to UTF-8).
   --
   -- So we had to find an native solution for this problem. Using the `write'
   -- system call is blazing fast. But it mey not be compatible with every
   -- operating sytem. Instead we use Streams, which yield a similar performace
   -- to the `write' sustem call, and they are in the standard library.
   --
   -- We need to keep a buffer to avoid writing the screen character by
   -- character (there are many single character calls). There is a buffer in
   -- the Ada.Text_IO.File_Type, but I'm not sure if the Stream package has it.
   -- I tested it on my laptop, and without a buffer, the code was 14 TIMES
   -- SLOWER. Therefore we are going to keep a buffer.
   --
   -- Also, as there will be only one buffer. We don't need to encapsulate it
   -- on a record and pass it everytime. Let's make it global and available
   -- for every function directly.

   procedure Flush with
      Inline => True;

   procedure Put (Item : in Character) with
      Inline => True;

   procedure Put (Item : in String) with
      Inline => True;

   procedure Wide_Wide_Put (Item : in Glyph) with
      Inline => True;

   procedure Wide_Wide_Put (Item : in Glyph_String) with
      Inline => True;

end Malef.Platform.Generic_Buffer;
