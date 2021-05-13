-- Advent of Code 2019, Day 17
--
-- John Perry
--
-- Intcode again
--
-- part 1: determine intersections of scaffolds and report sum of alignment
-- parameters
--
-- part 2: program the vacuum droid to visit all the scaffolding,
-- report amount of space dust collected
--
-- part 1 was pretty easy
--
-- my issues with part 2 were due in large part to (a) misreading the input
-- format, and (b) the seemingly random placement of the output in part 2
--
-- there's also an issue with how my input & output interact;
-- the intcode produces output and expects input, but the output seems not
-- to be formatted correctly, and prompts seem to appear AFTER it reads
-- the desired input
--
-- the problem is probably that i don't have any way of specifiying
-- input length, so that my code will read invalid input just so long as
-- the input position has not advanced beyond the last buffer element
--
-- the program below works, but ideally i'd rework it so that this issue
-- is resolved; alas, I probably won't

pragma Ada_2020;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Containers.Ordered_Maps;

with Intcode;

procedure Main is

   -- SECTION
   -- input-related

   F: File_Type; -- input file

   Testing: constant Boolean := False;

   Filename: constant String
         := (
             if Testing then "/Users/user/common/Ada/AoC2019/Day17/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day17/input.txt"
            );

   -- SECTION
   -- debugging

   Verbose: Boolean := False;

   -- SECTION
   -- Intcode computer

   Output_Length: constant := 2198;
   -- determined from first running the program once
   -- in fact it should probably be larger, and this is probably related
   -- to my problems I was having with the input/output routines

   package Droid is new Intcode
     -- vacuum droid
   (
    Max_Location => 10000,
    Output_To_Command_Line => False,
    Output_Size => Output_Length,
    Input_From_Command_Line => False,
    Input_Size => 100
   );

   -- SECTION
   -- data, structures, and subprograms for part 1

   Num_Rows, Num_Cols: Positive := 1;
   -- dimensions of the map

   Scaffold: constant Long_Long_Integer := Character'Pos('#');
   -- used to find intersections

   function Thing_At( Map: Droid.Output_Type; I, J: Positive )
                     return Long_Long_Integer
   -- the map is read in as a 1-dimensional array,
   -- since we don't know the dimensions beforehand;
   -- thus, this translates 2-dimensional position we want
   -- to 1-dimensional position we need
   is ( Map( ( I - 1 ) * ( Num_Cols + 1 ) + ( J - 1 ) + 1 ) );

   function Intersection_At( Map: Droid.Output_Type; I, J: Positive)
                            return Boolean
   -- test whether there is an intersection at position (I,J)
   is (
       Thing_At(Map, I, J) = Scaffold and then
       Thing_At(Map, I-1, J) = Scaffold and then
       Thing_At(Map, I+1, J) = Scaffold and then
       Thing_At(Map, I, J+1) = Scaffold and then
       Thing_At(Map, I, J-1) = Scaffold
      );

begin

   -- initial run

   Droid.Read_Program(Filename);
   Droid.Run_Program;

   -- part 1

   declare

      Droid_Output: Droid.Output_Type := Droid.Report_Output;
      Alignment_Parameter: Natural := 0;

   begin

      -- determine dimensions while printing map
      -- (I'm curious)

      Put_Line("map:");

      for I in Droid_Output'Range loop

         Put( Character'Val( Droid_Output(I) ) );

         if Droid_Output(I) = 10 and then Num_Cols = 1 then
            -- the end-of-line character is not a valid column!
            Num_Cols := I - 1;
         end if;

      end loop;

      -- i could count the number of rows, but why?
      Num_Rows := Droid_Output'Last / ( Num_Cols + 1);
      Put_Line("num rows: " & Num_Rows'Image);
      Put_Line("num cols: " & Num_Cols'Image);

      -- find scaffolding intersections

      for I in 2 .. Num_Rows - 1 loop
         for J in 2 .. Num_Cols - 1 loop
            if Intersection_At( Droid_Output, I, J) then
               Alignment_Parameter := @ + ( I - 1 ) * ( J - 1 );
            end if;
         end loop;
      end loop;

      Put_Line("alignment parameter: " & Alignment_Parameter'Image);

   end;

   -- part 2

   declare

      -- derived by hand; it wasn't too hard
      -- first i noticed Function_A appeared several times,
      -- then Function_B, though at first I had the wrong length,
      -- and finally Function_C, which corrected Function_B

      Movement_Function: String := "A,C,C,A,B,A,B,A,B,C";
      Function_A: String := "R,6,R,6,R,8,L,10,L,4";
      Function_B: String := "L,4,L,12,R,6,L,10";
      Function_C: String := "R,6,L,10,R,8";
      Video_Feed: String := "n";
      Input_String: String
        := Movement_Function & "-" & Function_A & "-"
        & Function_B & "-" & Function_C & "-"
        & Video_Feed & "-";

      -- the assignment below may have been just a little too clever

      Input_Array: Droid.Input_Type
        := (
            (
            for I in Droid.Input_Type'Range =>
              (
               if I <= Input_String'Last and then Input_String(I) /= '-' then
                  Character'Pos(Input_String(I))
               else 10
              )
           )
           );

      Dust_Particles_Cleaned: Natural := 0; -- result of part 2

   begin

      -- uncomment if you want to see the input string in its glory
      --  Put_Line("input: " & Input_String);

      Droid.Memory(0) := 2; -- sets it to wake up & follow directions
      Droid.Reset_Output;
      Droid.Supply_Input(Input_Array);
      Droid.Run_Program;

      for C of Droid.Report_Output loop
         Dust_Particles_Cleaned := Natural'Max(@,Natural(C));
      end loop;

      Put_Line("dust particles cleaned:" & Dust_Particles_Cleaned'Image);

   end;

end Main;
