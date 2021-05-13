-- Advent of Code 2019, Day 9
--
-- John Perry
--
-- part 1: complete the Intcode computer with relative mode addressing
-- and a command to adjust the relative base; test with input 1
--
-- part 2: run with input 2

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
             if Testing then "/Users/user/common/Ada/AoC2019/Day9/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day9/input.txt"
            );

   package Boost is new Intcode
         (
          Max_Location            => 10000,
          Input_Size              => 2,
          Output_Size             => 1,
          Input_From_Command_Line => True,
          Output_To_Command_Line  => True
         );

begin

   -- parts 1 and 2

   Boost.Read_Program(Filename);
   Boost.Run_Program;

end Main;
