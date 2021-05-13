-- Advent of Code 2019, Day 16
--
-- John Perry
--
-- part 1: apply Flawed Frequency Transmission to transform an input sequence
-- and after 100 phases report first 8 digits
--
-- part 2: figure out the message, which is an ungodly number of digits
-- into an ungodly long signal, which itself has to be processed 100 times...
-- but even an efficient processor won't do the trick because it's so ungodly
-- long that you need to analyze the procedure and the input
-- to figure out how to solve it, which requires you to ignore what you did
-- in part 1 -- not to improve, but to ignore it
--
-- this needs a larger stack: ulimit -S -s 131072 on bash was more than enough

pragma Ada_2020;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is

   -- SECTION
   -- input-related

   F: File_Type; -- input file

   Testing: constant Boolean := False;

   Filename: constant String
         := (
             if Testing then "/Users/user/common/Ada/AoC2019/Day16/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day16/input.txt"
            );

   -- SECTION
   -- debugging

   Verbose: Boolean := False;

   -- SECTION
   -- data types

   Which_Test: constant := 5;

   Data_Length: constant
     -- length of input and output data (I examined the file to determine this)
     := (
         if not Testing then 650
         else (
           case Which_Test is
              when 1 => 8,
              when others => 32
          )
        );

   subtype Data_Value is Natural range 0 .. 9;
   -- what's allowed in the data

   subtype Data_Range is Positive range 1 .. Data_Length;
   -- range of indices for the data

   type Data_Sequence is array( Positive range <> ) of Data_Value;
   -- data structure

   Input_Data: Data_Sequence(Data_Range);

   procedure Put(Seq: Data_Sequence) is
      -- displays Seq in a row (very long if Seq is)
   begin
      for S of Seq loop Put(S, 0); end loop;
   end Put;

   type Mod4 is mod 4;
   -- the pattern has 4 values & we cycle through them

   Base_Pattern: constant array( Mod4'Range ) of Integer := ( 0, 1, 0, -1 );

   -- SECTION
   -- subprograms for part 1

   procedure Transform(
                       Input: Data_Sequence;
                       Output: out Data_Sequence
                      )

     -- applies offset & amplified pattern to Input, deriving Output;
     -- this is a naive implementation
     --
     -- from the puzzle:
     -- "[R]epeat each value in the pattern a number of times equal to
     -- the position in the output list being considered.
     -- Repeat once for the first element, twice for the second element,
     -- three times for the third element, and so on.
     -- "When applying the pattern, skip the very first value exactly once.
     -- (In other words, offset the whole pattern left by one.)
     -- So, for the second element of the output list,
     -- the actual pattern used would be:
     -- 0, 1, 1, 0, 0, -1, -1, 0, 0, 1, 1, 0, 0, -1, -1, ...."

   is

      Pattern_Value: Mod4;
      -- where we are in the pattern

      J: Natural;
      -- where we are in the repetition 1 .. I of Pattern_Value

      Sum: Integer;
      -- the value of the next output digit

   begin

      -- loop through Input & deposit in Output
      for Position in Input'Range loop

         -- initialize
         J := 1;
         Sum := 0;
         Pattern_Value := 0;

         -- loop through Input AGAIN
         for I in Input'Range loop

            -- move forward in the repetition, restart if need be
            -- doing this here helps us offset the pattern

            J := J + 1;
            if J > Position then
               J := 1;
               Pattern_Value := Pattern_Value + 1;
            end if;

            Sum := Sum + Input(I) * Base_Pattern(Pattern_Value);

         end loop;

         Output(Position) := abs(Sum) mod 10;

      end loop;

   end Transform;

   procedure Alternate_Transform(
                                 Input: Data_Sequence;
                                 Output: out Data_Sequence
                                )

     -- this is a more efficient approach to the transform;
     -- I noticed that we are only ever adding & subtracting values in the list,
     -- and I went about "jumping" through the list to add them
     --
     -- I was kind of proud of myself for figuring this out;
     -- however, it does not evade the fundamental problem which makes part 2
     -- intractable without insight into the result (as opposed to the problem):
     -- we have a nested loop on a list of size 650 * 10_000,
     -- with each loop going through the entire range,
     -- which is rather impractical

   is

      Pos: Positive;
      -- position where we start a run

      Sum: Integer := 0;
      -- value of output

   begin

      -- loop on every input element
      for I in Input'Range loop

         -- initialization; to handle the offset, start at position I
         Pos := I;
         Sum := 0;

         -- jump through to take care of the addition
         while Pos + I <= Input'Last loop
            for J in 1 .. I loop
               Sum := Sum + Input(Pos + J - 1);
            end loop;
            Pos := Pos + 4 * I;
         end loop;

         -- can jump no further; wrap up the remaining terms
         for J in Pos .. Positive'Min(Pos + I - 1, Input'Last) loop
            Sum := Sum + Input(J);
         end loop;

         -- jump through to take care of the subtraction
         Pos := 3 * I;
         while Pos + I <= Input'Last loop
            for J in 1 .. I loop
               Sum := Sum - Input(Pos + J - 1);
               --  Put_Line("subtracting" & Input(Pos + J - 1)'image);
            end loop;
            Pos := Pos + 4 * I;
         end loop;

         -- can jump no further; wrap up the remaining terms
         for J in Pos .. Positive'Min(Pos + I - 1, Input'Last) loop
            Sum := Sum - Input(J);
         end loop;

         -- finish adding; record
         Output(I) := abs(Sum) mod 10;

      end loop;

   end Alternate_Transform;

   procedure Third_Approach_From_Reddit(
                                        Input: Data_Sequence;
                                        Output: out Data_Sequence;
                                        Start: Positive
                                       )

     -- as indicated by the name, I found this approach in the reddit discussion
     --
     -- it's clever, and I had thought of something like this,
     -- but I keep attacking it from the front instead of from the end
     --
     -- essentially, the solution is problem-dependent.
     -- the message Start is chosen past halfway, so when you apply the pattern,
     -- the repetition of the pattern means you always add the digits
     -- from there on
     --
     -- this means you can work backward: the last value of the output
     -- is always the last value of the input, and from then on you just
     -- add

   is

      Last: Positive := Output'Last;

   begin

      Output(Last) := Input(Last);
      for I in 1 .. Last - Start loop
         Output(Last - I) := ( Input(Last - I) + Output(Last - I + 1) ) mod 10;
      end loop;

   end Third_Approach_From_Reddit;

begin

   -- read the input

   if Testing then

      -- various inputs supplied by the problem, which are helpful for part 1
      -- but not really for part 2

      Input_Data := (
                     case Which_Test is
                        when 1 => ( 1, 2, 3, 4, 5, 6, 7, 8 ),
                        when 2 => (
                                   8, 0, 8, 7, 1, 2, 2, 4, 5, 8, 5, 9, 1, 4, 5,
                                   4, 6, 6, 1, 9, 0, 8, 3, 2, 1, 8, 6, 4, 5, 5,
                                   9, 5
                                  ),
                        when 3 => (
                                   1, 9, 6, 1, 7, 8, 0, 4, 2, 0, 7, 2, 0, 2, 2,
                                   0, 9, 1, 4, 4, 9, 1, 6, 0, 4, 4, 1, 8, 9, 9,
                                   1, 7
                                  ),
                        when 4 => (
                                   6, 9, 3, 1, 7, 1, 6, 3, 4, 9, 2, 9, 4, 8, 6,
                                   0, 6, 3, 3, 5, 9, 9, 5, 9, 2, 4, 3, 1, 9, 8,
                                   7, 3
                                  ),
                        when 5 => (
                                   0, 3, 0, 3, 6, 7, 3, 2, 5, 7, 7, 2, 1, 2, 9,
                                   4, 4, 0, 6, 3, 4, 9, 1, 5, 6, 5, 4, 7, 4, 6,
                                   6, 4
                                  ),
                        when others => raise Data_Error
                          with "invalid test: " & Which_Test'Image
                    );

   else

      Open(F, In_File, Filename);

      for I in Data_Range loop
         declare C: Character;
         begin
            Get(F, C);
            Input_Data(I) := Character'Pos(C) - Character'Pos('0');
         end;
      end loop;

      Close(F);

   end if;

   -- part 1

   declare

      New_Input: Data_Sequence := Input_Data;
      Output_Data: Data_Sequence(Input_Data'Range);

   begin

      -- testing first approach
      for I in 1 .. 100 loop
         Transform(New_Input, Output_Data);
         New_Input := Output_Data;
         if Verbose then Put(New_Input); New_Line; end if;
      end loop;
      for I in 1 .. 8 loop
         Put(New_Input(I), 0);
      end loop;
      New_Line;

      -- testing alternate approach (it's faster!)
      New_Input := Input_Data;
      for I in 1 .. 100 loop
         Alternate_Transform(New_input, Output_Data);
         New_Input := Output_Data;
         if Verbose then Put(New_Input); New_Line; end if;
      end loop;
      for I in 1 .. 8 loop
         Put(New_Input(I), 0);
      end loop;
      New_Line;
   end;

   -- part 2

   declare

      subtype Real_Signal_Range
        -- the "real" signal is just a copy of the received signal
        is Positive range 1 .. Input_Data'Length  * 10_000;

      Real_Signal, Tmp_Signal: Data_Sequence(Real_Signal_Range);

      Offset: Natural := 0;
      -- where in the real signal the message appears

   begin

      -- copy signal 10000 times to obtain "real" signal
      for I in 1 .. 10_000 loop
         for J in Input_Data'Range loop
            Real_Signal( ( I - 1 ) * Input_Data'Length + J ) := Input_Data(J);
         end loop;
      end loop;

      -- determine offset
      for I in 1 .. 7 loop
         Offset := Offset * 10 + Input_Data(I);
      end loop;
      Put_Line("offset is" & Offset'Image);

      -- DO NOT UNCOMMENT THESE LINES; IT TAKES FOR EVER (POSSIBLY LONGER)
      -- repeat part 1 on real signal
      --  for I in 1 .. 100 loop
      --     Put_Line("transformation" & I'Image);
      --     Alternate_Transform(Real_Signal, Tmp_Signal);
      --     Real_Signal := Tmp_Signal;
      --  end loop;

      -- apply the trick to determine the message
      for I in 1 .. 100 loop
         Third_Approach_From_Reddit(Real_Signal, Tmp_Signal, Offset);
         Real_Signal := Tmp_Signal;
      end loop;

      -- report message
      for I in 1 .. 8 loop
         Put(Real_Signal(Offset + I), 0);
      end loop;
      New_Line;

   end;

end Main;
