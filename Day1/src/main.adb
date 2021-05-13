-- Advent of Code 2019, Day 1
--
-- John Perry
--
-- Tyranny of the Rocket Equation
--
-- part 1: apply a simple formula to input sequence
--
-- part 2: compound the formula on itself
--

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
             if Testing then "/Users/user/common/Ada/AoC2019/Day1/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day1/input.txt"
            );

   -- SECTION
   -- computing fuel requirements

   function Estimate_Fuel(Mass: Natural) return Natural is
   -- returns an estimate of the fuel mass necessary for Mass
   ( ( Mass - Mass mod 3 ) / 3 -2 );

begin

   -- SECTION
   -- parts 1 and 2 together

   Open(F, In_File, Filename);

   declare

      Value, Partial_Sum, Intermediate_Sum: Natural;
      Sum, Total_Sum: Natural := 0;

   begin

      while not End_Of_File(F) loop

         Get(F, Value);

         Partial_Sum := Estimate_Fuel(Value);
         Sum := Sum + Partial_Sum;

         -- now accumulate the fuel for the fuel
         -- only need to do this for Partial_Sum > 8;
         -- otherwise estimate is 0 or negtive

         Intermediate_Sum := Partial_Sum;

         while Partial_Sum > 8 loop
            Partial_Sum := Estimate_Fuel(Partial_Sum);
            Intermediate_Sum := Intermediate_Sum + Partial_Sum;
         end loop;

         Total_Sum := Total_Sum + Intermediate_Sum;

      end loop;

      Put("sum of fuel requirements: "); Put(Sum, 0); New_Line;
      Put("total sum of fuel requirements: "); Put(Total_Sum, 0); New_Line;

   end;

   Close(F);

end Main;
