-- Advent of Code 2019, Day 5
--
-- John Perry
--
-- part 1: determine the maxmimal value produced by a chained sequence of
-- phased amplifiers
--
-- part 2: same, but now with a feedback loop

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
             if Testing then "/Users/user/common/Ada/AoC2019/Day7/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day7/input.txt"
            );

   function Digit(N: Natural; D: Natural) return Natural is
   -- returns the Dth digit of N
   ( ( (N - N mod 10 ** D ) - (N - N mod 10 ** (D + 1) ) ) / 10 ** D );

begin

   -- part 1

   declare

      Max_Value: Long_Long_Integer := 0;
      Location_Of_Max: Natural := 0;

      package Amplifier is new Intcode
            (
             Max_Location => 1000,
             Input_Size   => 2,
             Output_Size  => 1,
             Input_From_Command_Line => False,
             Output_To_Command_Line => False
            );
      Input_Array: Amplifier.Input_Type := ( others => 0 );

   begin

      Amplifier.Read_Program(Filename);

      -- test all possibilities
      for I in 1234 .. 43210 loop

         -- the digits are to be distinct

         if (
             for all J in 0 .. 4
             => Digit(I,J) in 0 .. 4
                and then
                not (
                     for some K in 0 .. 4 => J /= K and Digit(I,J) = Digit(I,K)
                    )
            )
         then

            if Testing then Put_Line("testing " & I'Image); end if;

            Input_Array := ( Long_Long_Integer(Digit(I, 4)), 0);

            Amplifier.Supply_Input( Input_Array );
            Amplifier.Run_Program;
            Input_Array := ( Long_Long_Integer(Digit(I, 3)), Amplifier.Report_Output(1) );

            Amplifier.Supply_Input( Input_Array );
            Amplifier.Run_Program;
            Input_Array := ( Long_Long_Integer(Digit(I, 2)), Amplifier.Report_Output(1) );

            Amplifier.Supply_Input( Input_Array );
            Amplifier.Run_Program;
            Input_Array := ( Long_Long_Integer(Digit(I, 1)), Amplifier.Report_Output(1) );

            Amplifier.Supply_Input( Input_Array );
            Amplifier.Run_Program;
            Input_Array := ( Long_Long_Integer(Digit(I, 0)), Amplifier.Report_Output(1) );

            Amplifier.Supply_Input( Input_Array );
            Amplifier.Run_Program;

            if Amplifier.Report_Output(1) >Long_Long_Integer( Max_Value ) then
               Max_Value := Amplifier.Report_Output(1);
               Location_Of_Max := I;
               Put_Line("reassigned max at " & I'Image & " to " & Max_Value'Image);
            end if;

         end if;

      end loop;

      Put_Line("max value is " & Max_Value'Image & " at " & Location_Of_Max'Image);

   end;

   -- part 2

   declare

      Max_Value: Long_Long_Integer := 0;
      Location_Of_Max: Natural := 0;

      -- this time there are 5 amplifiers which feed on each other,
      -- and which have to remember previous state, so I can't just
      -- repeat the previous amplifier

      package Amplifier_1 is new Intcode
            (
             Max_Location => 1000,
             Input_Size   => 2,
             Output_Size  => 1,
             Input_From_Command_Line => False,
             Output_To_Command_Line => False
            );

      package Amplifier_2 is new Intcode
            (
             Max_Location => 1000,
             Input_Size   => 2,
             Output_Size  => 1,
             Input_From_Command_Line => False,
             Output_To_Command_Line => False
            );

      package Amplifier_3 is new Intcode
            (
             Max_Location => 1000,
             Input_Size   => 2,
             Output_Size  => 1,
             Input_From_Command_Line => False,
             Output_To_Command_Line => False
            );

      package Amplifier_4 is new Intcode
            (
             Max_Location => 1000,
             Input_Size   => 2,
             Output_Size  => 1,
             Input_From_Command_Line => False,
             Output_To_Command_Line => False
            );

      package Amplifier_5 is new Intcode
            (
             Max_Location => 1000,
             Input_Size   => 2,
             Output_Size  => 1,
             Input_From_Command_Line => False,
             Output_To_Command_Line => False
            );

      -- inputs for the amplifiers
      Input1: Amplifier_1.Input_Type := ( others => 0 );
      Input2: Amplifier_2.Input_Type := ( others => 0 );
      Input3: Amplifier_3.Input_Type := ( others => 0 );
      Input4: Amplifier_4.Input_Type := ( others => 0 );
      Input5: Amplifier_5.Input_Type := ( others => 0 );
      Input_Location: Positive := Amplifier_1.Input_Type'First;

   begin

      Amplifier_1.Read_Program(Filename);
      Amplifier_2.Read_Program(Filename);
      Amplifier_3.Read_Program(Filename);
      Amplifier_4.Read_Program(Filename);
      Amplifier_5.Read_Program(Filename);

      -- test all possibilities
      for I in 56789 .. 98765 loop

         -- reset Input_Location for each new phase combination
         Input_Location := Amplifier_1.Input_Type'First;

         if Testing then Put_Line("testing " & I'Image); end if;

         -- makes ure the phase combination has distinct digits
         if (
             for all J in 0 .. 4
             => Digit(I,J) in 5 .. 9
                and then
                not (
                     for some K in 0 .. 4 => J /= K and Digit(I,J) = Digit(I,K)
                    )
            )
         then

            Feedback_Loop:
            loop

               -- in each case we modify the input if the amplider was suspended
               -- to obtain input from previous

               Input1 := ( Long_Long_Integer(Digit(I, 4)), 0);
               if Amplifier_1.Is_Suspended_For_Input then
                  Input1(Amplifier_1.Input_Type'Last)
                        := Amplifier_5.Report_Output(1);
               end if;

               -- once we've figured the input, we run the amplifier, and
               -- use its output to set up the input for the next one in line

               Amplifier_1.Supply_Input( Input1, Input_Location );
               Amplifier_1.Run_Program;
               Input2 := ( Long_Long_Integer(Digit(I, 3)), Amplifier_1.Report_Output(1) );
               if Amplifier_2.Is_Suspended_For_Input then
                  Input2(Amplifier_2.Input_Type'Last)
                        := Amplifier_1.Report_Output(1);
               end if;

               Amplifier_2.Supply_Input( Input2, Input_Location );
               Amplifier_2.Run_Program;
               Input3 := ( Long_Long_Integer(Digit(I, 2)), Amplifier_2.Report_Output(1) );
               if Amplifier_3.Is_Suspended_For_Input then
                  Input3(Amplifier_3.Input_Type'Last)
                        := Amplifier_2.Report_Output(1);
               end if;

               Amplifier_3.Supply_Input( Input3, Input_Location );
               Amplifier_3.Run_Program;
               Input4 := ( Long_Long_Integer(Digit(I, 1)), Amplifier_3.Report_Output(1) );
               if Amplifier_4.Is_Suspended_For_Input then
                  Input4(Amplifier_4.Input_Type'Last)
                        := Amplifier_3.Report_Output(1);
               end if;

               Amplifier_4.Supply_Input( Input4, Input_Location );
               Amplifier_4.Run_Program;
               Input5 := ( Long_Long_Integer(Digit(I, 0)), Amplifier_4.Report_Output(1) );
               if Amplifier_5.Is_Suspended_For_Input then
                  Input5(Amplifier_5.Input_Type'Last)
                        := Amplifier_4.Report_Output(1);
               end if;

               Amplifier_5.Supply_Input( Input5, Input_Location );
               Amplifier_5.Run_Program;

               -- when Amplifier_5 is done running, all the amplifiers
               -- are done running (I checked the Intcode) so we exit

               exit Feedback_Loop when not Amplifier_5.Is_Suspended_For_Input;

               Input_Location := Amplifier_1.Input_Type'Last;

            end loop Feedback_Loop;

            if Amplifier_5.Report_Output(1) > Max_Value then
               Max_Value := Amplifier_5.Report_Output(1);
               Location_Of_Max := I;
               Put_Line("reassigned max at " & I'Image & " to " & Max_Value'Image);
            end if;

         end if;

      end loop;

      Put_Line("max value is " & Max_Value'Image & " at " & Location_Of_Max'Image);

   end;

end Main;
