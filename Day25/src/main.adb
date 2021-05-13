-- Advent of Code 2019, Day 23
--
-- John Perry
--
-- Intcode again
--
-- part 1: send an Intcode droid exploring Santa's ship. it turns out that you
-- end up playing an old-style Text Adventure Game (TAG).
--
-- part 2: nothing serious to do
--
-- the TAG's commands are simple:
-- directions to move, take, drop, inv. several traps were fairly obvious
-- (molten lava, infinite loop); several were less so (electromagnet, photons).
-- i guess there's an inside joke of some sort about a grue (i look it up now
-- and find that it shows up in a game called Zork). the main puzzle involves
-- getting past a security sensor; the droid has to carry the correct weight.
-- in various rooms (there are 18 in all) you find items to weigh the droid down.
-- some experimentation (at most 256) will get you past security, and you
-- obtain the passcode.
--
-- see solution.txt for the correct solution to my case

pragma Ada_2020;

with Ada.Text_IO; use Ada.Text_IO;

with Intcode;

procedure Main is

   -- SECTION
   -- input-related

   Filename : constant String
      := "/Users/user/common/Ada/AoC2019/Day25/input.txt";

   -- SECTION
   -- Intcode computer

   package Droid is new Intcode
   -- drone droid
      (
       Max_Location            => 10000,
       Output_To_Command_Line  => True,
       Input_From_Command_Line => True,
       Input_Size              => 15,
       ASCII_Mode              => True
      );

begin

   -- the difficulty here lies not in the programming but in the program ;-)

   Droid.Read_Program (Filename);
   Droid.Run_Program;

end Main;
