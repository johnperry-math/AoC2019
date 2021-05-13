-- Advent of Code 2019, Day 19
--
-- John Perry
--
-- Intcode again
--
-- part 1: determine where tractor beam is active in a 50x50 grid
--
-- part 2: determine where Santa's 100x100 ship might fit into tractor beam
--
-- Shockingly, I got part 1 on the first try, and part 2 on the second.
-- In fact, I really had part 2 on the first try;
-- I just reported the wrong data.

pragma Ada_2020;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Intcode;

procedure Main is

   -- SECTION
   -- input-related

   Filename: constant String
         := "/Users/user/common/Ada/AoC2019/Day19/input.txt";

   Verbose: Boolean := False;

   -- SECTION
   -- Intcode computer

   Output_Length: constant := 1;

   package Droid is new Intcode
     -- drone droid
   (
    Max_Location => 10000,
    Output_To_Command_Line => False,
    Output_Size => Output_Length,
    Input_From_Command_Line => False,
    Input_Size => 2
   );

   -- SECTION
   -- variables and data types for part 1

   -- I thought I would need the 50x50 grid for part 2,
   -- but it turns out I didn't.
   -- Doesn't hurt to keep it hanging around.

   Grid_Width: constant := 50;
   Grid_Length: constant := 50;

   type Grid_Type is
         array( 0 .. Grid_Width - 1 , 0 ..  Grid_Length - 1 ) of Boolean;
   In_Beam: Grid_Type := ( others => ( others => False ) );

   -- SECTION
   -- subprograms useful to both parts

   function Affected_Point(Row, Col: Natural) return Boolean is
      -- returns True iff position ( Row, Col ) is affected by tractor beam

      K: constant := Droid.Output_Type'First;
      Coords: Droid.Input_Type
            := ( Long_Long_Integer(Row), Long_Long_Integer(Col) );

   begin

      Droid.Supply_Input(Coords);
      Droid.Run_Program;
      return Droid.Report_Output(K) = 1;

   end Affected_Point;

begin

   -- setup

   Droid.Read_Program(Filename);

   -- part 1: determine the number of affected points
   --
   -- as a bonus, I also print out the 50x50 grid

   declare Number_Of_Points_Affected: Natural := 0;
   begin

      for I in In_Beam'Range(1) loop
         for J in In_Beam'Range(2) loop

            if Affected_Point(I, J) then
               In_Beam(I,J) := True;
               Number_Of_Points_Affected := Number_Of_Points_Affected + 1;
            end if;

         end loop;
      end loop;

      Put_Line(Number_Of_Points_Affected'Image & " points are affected");

   end;

   -- print the grid

   for I in In_Beam'Range(1) loop
      for J in In_Beam'Range(2) loop
         Put( ( if In_Beam(I,J) then '#' else '.' ) );
      end loop;
      New_Line;
   end loop;

   New_Line;

   -- part 2: find the closest 100x100 block
   -- that is fully within the tractor beam

   -- my strategy was to move down the grid, one row at a time,
   -- moving across the grid as well,
   -- in such as way as to remain on the beam's "left" edge,
   -- until I find a 100x100 block fully within the beam
   --
   -- the one mistake I made was to report the resulting position (I,J);
   -- that's actually the BOTTOM left corner of the block,
   -- whereas I need the TOP left corner of the block,
   -- but that's easy enough to find
   --
   -- there are better approaches, and I started to sketch one out where I'd
   -- figure the position mathematically, but this turns out to be reasonably
   -- quick

   declare

      Size_Desired: constant := 100;

      Row: Natural := 3 * Size_Desired; -- row of the grid that we're searching
                                        -- this always moves down
                                        -- no point in starting too close,
                                        -- and in fact would break my approach
      Col: Natural := 0; -- column of the grid that we're searching
                         -- this always moves right

   begin

      Find_Santa: loop

         -- move right until we're in the tractor beam
         while not Affected_Point(Row,Col) loop Col := Col + 1; end loop;

         -- check if the square is large enough; if so, leave
         exit Find_Santa when Affected_Point(Row - Size_Desired + 1, Col)
               and then
                     Affected_Point(
                                    Row - Size_Desired + 1,
                                    Col + Size_Desired - 1
                                   );

         -- square is not large enough, so move to next row
         Row := Row + 1;

      end loop Find_Santa;

      -- report

      Put_Line(
               "square large enough at" & Natural'Image(Row - Size_Desired + 1)
               & "," & Col'Image
              );
      Put("result is "); Put((Row - Size_Desired + 1) * 10000 + Col, 0);
      New_Line;

   end;

end Main;
