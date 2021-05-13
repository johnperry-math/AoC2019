-- Advent of Code 2019, Day 21
--
-- John Perry
--
-- Intcode again
--
-- part 1: program a springdroid to jump over holes in the hull so that it can
-- count the damage
--
-- part 2: same as part 1, but we RUN instead of WALK, and we now have more
-- sensors
--
-- I had to modify the Intcode program to accept ASCII input, both from the
-- command line and from the input buffer, because I didn't like the idea of
-- converting text to LONG_LONG_INTEGER the way I did on Day 17
--
-- With that out of the way, it was a relatively simple matter of determining
-- the correct sequence of springdroid commands that would fit into 15 lines.
-- Part 1 is no problem; part 2 requires a little Boolean algebra. (VERY little)
--
-- With the Intcode modified to accept ASCII, I entered the "springtext" on the
-- command line. Solutions appear in the files solution_to_part?.txt

pragma Ada_2020;

with Intcode;

procedure Main is

   -- SECTION
   -- input-related

   Filename : constant String
      := "/Users/user/common/Ada/AoC2019/Day21/input.txt";

   -- SECTION
   -- Intcode computer

   Output_Length : constant := 1;

   package Droid is new Intcode
   -- drone droid
      (
       Max_Location            => 10000,
       Output_To_Command_Line  => True,
       Input_From_Command_Line => True,
       Input_Size              => 133,
       ASCII_Mode              => True
      );

begin

   Droid.Read_Program (Filename);
   Droid.Run_Program;

end Main;
