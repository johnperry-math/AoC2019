-- Advent of Code 2019, Day 13
--
-- John Perry
--
-- Intcode again
--
-- part 1: determine the number of block tiles on screen when game exits
--
-- part 2: play the game, win, report score
--
-- you probably want to automate it, because there are a lot of tiles

pragma Ada_2020;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Intcode;

procedure Main is

   -- SECTION
   -- input-related

   F: File_Type; -- input file

   Testing: constant Boolean := False;

   Filename: constant String
         := (
             if Testing then "/Users/user/common/Ada/AoC2019/Day13/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day13/input.txt"
            );

   Iterations_Part1: constant := ( if Testing then 10 else 1000 );

   -- SECTION
   -- tile information

   type Tile_Types is ( Empty, Wall, Block, Paddle, Ball );

   -- SECTION
   -- Intcode computer

   Output_Length: constant := 6400;

   package Arcade is new Intcode
   (
    Max_Location => 10000,
    Input_From_Command_Line => False,
    Input_Size => 1,
    Output_To_Command_Line => False,
    Output_Size => Output_Length
   );

   -- SECTION
   -- miscellaneous material for part 2

   Score: Integer;

   -- SUBSECTION
   -- information on the playfield, derived from part 1

   Field_Width: constant := 38;
   Field_Height: constant := 21;
   type Playfield is array( 1 .. Field_Width, 1 .. Field_Height ) of Tile_Types;

   procedure Put(P: Playfield) is
   -- show the playfield on the screen
   begin

      for Y in P'Range(2) loop
         for X in P'Range(1) loop

            Put( (
                 case P(X,Y) is
                 when Empty => ' ',
                 when Wall => '|',
                 when Block => 'X',
                 when Paddle => '_',
                 when Ball => 'o'
            ) );

         end loop;
         New_Line;
      end loop;

   end Put;

   procedure Copy_Playfield_To(P: in out Playfield) is
   -- copies data from arcade output into the playfield
   -- this loops from start of output until it encounters (0, 0, 0)

      I: Natural := 1;
      -- location in output

      X, Y, Value: Integer;
      -- data read at position I, I + 1, I + 2 of output

   begin

      loop

         -- read the data
         X := Integer(Arcade.Report_Output(I));
         Y := Integer(Arcade.Report_Output(I+1));
         Value := Integer(Arcade.Report_Output(I+2));

         -- termination condition
         exit when X = 0 and then Y = 0 and then Value = 0;

         -- store data
         if X /= -1 then
            P(X + 1,Y + 1) := Tile_Types'Val(Value);
         elsif X = -1 and Y = 0 then
            Score := Value;
         end if;

         I := I + 3;

      end loop;

   end Copy_Playfield_To;

   Last_Playfield: Playfield;
   -- the last playfield Arcade reported

   type Object_Location is record
      X, Y: Natural;
   end record;

   function Find(P: Playfield; Thing: Tile_Types) return Object_Location is
   -- finds Thing in Playfield and reports location;
   -- if Thing not found, reports ( 0, 0 )

   begin

      for Y in P'Range(2) loop
         for X in P'Range(1) loop
            if P(X,Y) = Thing then return ( X, Y ); end if;
         end loop;
      end loop;

      return ( 0, 0 );

   end Find;

   Show_Game: Boolean := False;
   -- if True, shows the game

begin

   Arcade.Read_Program(Filename);

   -- SECTION
   -- part 1: determine the number of wall tiles

   Arcade.Run_Program;

   declare Result: Natural := 0;
   begin

      for I in 1 .. Output_Length / 3 loop
         if Arcade.Report_Output(3*(I-1) + 3) = 2 then
            Result := Result + 1;
         end if;
      end loop;

      Put_Line("there are" & Result'Image & " wall tiles");

   end;

   -- optionally display

   if Show_Game then
      Copy_Playfield_To(Last_Playfield);
      Put(Last_Playfield);
   end if;

   -- SECTION
   -- part 2: beat the game, report the score

   -- the next line makes the game play (specified in puzzle)
   Arcade.Memory(0) := 2;

   loop

      -- reset the buffer and restart the program
      Arcade.Reset_Output;
      Arcade.Run_Program;

      -- obtain the playfield data and optionally show it
      Copy_Playfield_To(Last_Playfield);
      if Show_Game then
         Put(Last_Playfield);
         Put_Line("score:" & Score'Image);
      end if;

      -- the game should continue until the Arcade no longer awaits input
      exit when not Arcade.Is_Suspended_For_Input;

      -- determine correct direction to move joystick

      declare

         -- locations of ball and paddle
         Ball_Loc: Object_Location := Find(Last_Playfield, Ball);
         Paddle_Loc: Object_Location := Find(Last_Playfield, Paddle);

         -- direction we'll want to move the joystick, and an input buffer
         Direction: Integer;
         Joystick_Input: Arcade.Input_Type;

      begin

         -- determine direction
         Direction := Ball_Loc.X - Paddle_Loc.X;
         Direction := ( if @ = 0 then 0 else abs(@) / @ );
         if Show_Game then Put("direction?"); Put_Line(Direction'Image); end if;

         -- inform the arcade
         Joystick_Input(1) := Long_Long_Integer(Direction);
         Arcade.Supply_Input(Joystick_Input);

      end;

   end loop;

   -- finally report
   Put_Line("final score:" & Score'Image);

end Main;
