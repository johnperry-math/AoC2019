-- Advent of Code 2019, Day 2
--
-- John Perry
--
-- Tyranny of the Rocket Equation
--
-- part 1: run an intcode program
--
-- part 2: find program parameters that produce a particular result
--

pragma Ada_2020;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Intcode;

procedure Main_Alternate is

   -- SECTION
   -- input-related

   F: File_Type; -- input file

   Testing: constant Boolean := False;

   Filename: constant String
         := (
             if Testing then "/Users/user/common/Ada/AoC2019/Day2/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day2/input.txt"
            );

   package Machine is new Intcode;

begin

   -- read the program and set up memory

   Machine.Read_Program(Filename);

   -- SECTION
   -- part 1: replace position 1 with 12, replace position 2 with 2,
   -- run program, report value of position 0

   Machine.Memory(1) := 12; Machine.Memory(2) := 2;
   Put("at position 0 we have ");
   Put_Line(Machine.Run_Program_Report_Location'Image);

   -- SECTION
   -- part 2: determine what pair of values in positions 1 and 2
   -- produce the output

   Noun_Loop:
   for Noun in 0 .. 100 loop
      for Verb in 0 .. 100 loop
         Machine.Memory(1) := Long_Long_Integer(Noun);
         Machine.Memory(2) := Long_Long_Integer(Verb);
         if Machine.Run_Program_Report_Location = 19690720 then
            Put("noun: "); Put(Noun, 0); Put("; verb: "); Put(Verb, 0);
            New_Line;
            Put("100 * noun + verb: "); Put(100 * Noun + Verb, 0); New_Line;
            exit Noun_Loop;
         end if;
      end loop;
   end loop Noun_Loop;

end Main_Alternate;
