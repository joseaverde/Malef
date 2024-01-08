with Ada.Strings.Text_Buffers;

package Terminfo is

   -- Terminal information can be found in most POSIX-like OS's as a little
   -- database. When you open your terminal the $TERM environment variable is
   -- set to the name of the current terminal. For instance, `kitty' may put
   -- `xterm-kitty' or `xfce4-terminal' may set it to `xterm-256color'.
   --
   -- This database can be queried to see what signals can be emitted to obtain
   -- certain result. From colouring the terminal, to styles, to even
   -- synchornising the terminal so drawing isn't choppy. Obviously all
   -- terminals won't implement all features. But we can emulate them.
   --
   -- This package purpose is searching in the database for those files. And
   -- loading them.
   --
   -- The search order is as follows
   --  * If $TERMINFO is set, search there and stop the search.
   --  * Then search in $HOME/.terminfo
   --  * Then search in the directories from the $TERMINFO_DIRS variable.
   --    These are separated by colons (`:'). And if there is an empty string
   --    it search /usr/share/terminfo.
   --  * Finally it searches in /usr/share/terminfo
   --
   -- This package is for internal usage only. The API might change in between
   -- versions.

   -->> Constants <<--

   TERMINFO_Env      : constant String := "TERMINFO";
   TERMINFO_DIRS_Env : constant String := "TERMINFO_DIRS";
   TERM_Env          : constant String := "TERM";
   HOME_Env          : constant String := "HOME";
   Default_Dir       : constant String := "/usr/share/terminfo";
   Default_Terminal  : constant String := "dummy";
   Dir_Separator     : constant Character := ':';

   --<<----------->>--
   -->> Term_Type <<--
   --<<----------->>--

   type Term_Type is private with
      Put_Image                 => Put_Image,
      Default_Initial_Condition => False;
   -- This type holds information about a terminal. It shouldn't be
   -- uninitialised.

   procedure Open (
      Term :    out Term_Type;
      Path : in     String);
   -- This procedure takes the path to a database entry and loads the data into
   -- the terminal.

   procedure Put_Image (
      Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in     Term_Type);

   --<<-------------------->>--
   -->> Terminfo Searching <<--
   --<<-------------------->>--

   function Search (
      Name : in String)
      return String;
   -- This function searches for a terminal with name `Name' on the filesystem.
   -- If it finds it, it returns its complete path, if not, it returns an
   -- empty String.

   function Search_Current return String;
   -- This function searches for the current terminal in the database. If it
   -- finds it it returns the path to the file, otherwise it returns an empty
   -- String.

private

   -- https://invisible-island.net/ncurses/man/term.5.html

   type Term_Type is null record;

end Terminfo;
