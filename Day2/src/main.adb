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

procedure Main is

   -- SECTION
   -- input-related

   F: File_Type; -- input file

   Testing: constant Boolean := False;

   Filename: constant String
         := (
             if Testing then "/Users/user/common/Ada/AoC2019/Day2/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day2/input.txt"
            );

   -- SECTION
   -- handling Intcode

   -- opcodes

   Add: constant := 1;
   Mul: constant := 2;
   Hlt: constant := 99;

   -- memory

   subtype Memory_Location is Natural range 0 .. 200;
   type Storage is array ( Memory_Location ) of Integer;
   Memory: Storage := ( others => Hlt );

   -- performing opcodes

   procedure Perform_Opcode(
         Opcode, Pos_1, Pos_2, Pos_3: Integer; Memory: in out Storage
   )
   -- interpret Opcode
   -- with source in Pos_1 and Pos_2 and destination in Pos_3
   is
   begin
      case Opcode is
      when Add => Memory(Pos_3) := Memory(Pos_2) + Memory(Pos_1);
      when Mul => Memory(Pos_3) := Memory(Pos_2) * Memory(Pos_1);
      when Hlt => null;
      when others => raise Data_Error with "bad opcode";
      end case;
   end Perform_Opcode;

   function Run_Program return Integer is
   -- run the program in memory
   -- we copy it, so that we can reuse the original later

      Program_Counter: Memory_Location := 0;
      Scratch_Memory: Storage := Memory;

   begin

      while Scratch_Memory(Program_Counter) /= 99 loop
         Perform_Opcode(
               Scratch_Memory(Program_Counter + 0), Scratch_Memory(Program_Counter + 1),
               Scratch_Memory(Program_Counter + 2), Scratch_Memory(Program_Counter + 3),
               Scratch_Memory
         );
         Program_Counter := Program_Counter + 4;
      end loop;

      return Scratch_Memory(0);

   end Run_Program;

begin

   -- read the program and set up memory

   Open(F, In_File, Filename);

   declare
      Address: Memory_Location := 0;
      Value: Integer;
      C: Character;
   begin
      while not End_Of_File(F) loop
         Get(F, Value);
         if not End_Of_File(F) then Get(F, C); end if;
         Memory(Address) := Value;
         Address := Address + 1;
      end loop;
   end;

   Close(F);

   -- SECTION
   -- part 1: replace position 1 with 12, replace position 2 with 2,
   -- run program, report value of position 0

   Memory(1) := 12; Memory(2) := 2;
   Put("at position 0 we have "); Put(Run_Program, 0); New_Line;

   -- SECTION
   -- part 2: determine what pair of values in positions 1 and 2
   -- produce the output

   Noun_Loop:
   for Noun in 0 .. 100 loop
      for Verb in 0 .. 100 loop
         Memory(1) := Noun; Memory(2) := Verb;
         if Run_Program = 19690720 then
            Put("noun: "); Put(Noun, 0); Put("; verb: "); Put(Verb, 0);
            New_Line;
            Put("100 * noun + verb: "); Put(100 * Noun + Verb, 0); New_Line;
            exit Noun_Loop;
         end if;
      end loop;
   end loop Noun_Loop;

end Main;
