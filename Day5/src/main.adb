-- Advent of Code 2019, Day 5
--
-- John Perry
--
-- part 1: enhance Intcode interpreter with new opcodes, run it to test
--
-- part 2: rerun it with a different value

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
             if Testing then "/Users/user/common/Ada/AoC2019/Day5/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day5/input.txt"
            );

   package Intcode_10000 is new Intcode ( Max_Location => 10000 );


begin

   -- read the program and set up memory

   Open(F, In_File, Filename);

   declare
      Address: Intcode_10000.Memory_Location := 0;
      Value: Integer;
      C: Character;
   begin
      while not End_Of_File(F) loop
         Get(F, Value);
         if not End_Of_File(F) then Get(F, C); end if;
         Intcode_10000.Memory(Address) := Long_Long_Integer(Value);
         Address := Address + 1;
      end loop;
   end;

   Close(F);

   -- SECTION
   -- parts 1 and 2 use the same input file,
   -- but require different input from user

   Intcode_10000.Run_Program;

end Main;
