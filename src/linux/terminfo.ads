with Interfaces;

package Terminfo is

   TERMINFO_Env      : constant String := "TERMINFO";
   TERMINFO_DIRS_Env : constant String := "TERMINFO_DIRS";
   Dir_Separator     : constant Character := ':';
   HOME_Env          : constant String := "HOME";
   Default_Dir       : constant String := "/usr/share/terminfo";

   -- $TERMINFO
   -- $HOME/.terminfo
   -- $TERMINFO_DIRS  (separated by :), empty is /usr/share/terminfo
   -- /usr/share/terminfo

   type Term_Type is private;

   procedure Open (
      File : in     String;
      Term :    out Term_Type);

   function Search (
      Name : in String)
      return String;

private

   Magic_Number : constant := 8#0432#;

   type Header_Type is
      record
         Magic                  : Interfaces.Integer_16;
         Terminal_Names_Size    : Interfaces.Integer_16;
         Boolean_Count          : Interfaces.Integer_16;
         Integer_Count          : Interfaces.Integer_16;
         Offset_Count           : Interfaces.Integer_16;
         String_Table_Size      : Interfaces.Integer_16;
      end record;

   -- https://invisible-island.net/ncurses/man/term.5.html

   type Term_Type is null record;

end Terminfo;
