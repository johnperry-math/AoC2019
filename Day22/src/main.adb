-- Advent of Code 2019, Day 22
--
-- John Perry
--
-- shuffling "space cards" (finally neither maze nor intcode)
--
-- part 1: apply different techniques to shuffle some cards
--
-- part 2: unshuffle after shuffling an ungodly number of times
--
-- note to self: be sure to read the question correctly; I was briefly stuck on
-- part 1 because I thought it wanted "the card in position 2019" instead of
-- "the position of card 2019"
--
-- the moment I saw the large numbers on part 2, I despaired, and immediately
-- went to the internet to see what people were saying about it. I probably
-- shouldn't have, because it turns out that the solution involves mathematics
-- that I teach and like. the trouble is, I first looked at the problem and
-- started thinking "Euler's Theorem should make this go faster" but of course
-- I thought about it backwards.
--
-- after reading some of the discussion, I concluded that I should compute
-- a linear function to describe the input, raise that to a power, then somehow
-- "unsolve" it. but, raising it to a power was still absurd -- until I realized
-- (see I didn't read TOO much detail!) that the power is repeating composition
-- of functions, NOT multiplications of polynomials.
--
-- it's still quite challenging after that! essentially what I do is set up a
-- function f = a x + b where f is the position of the item in position x after
-- we apply the input shuffles. then i raise it to the given power, using fast
-- exponentiation (graduate school was good for something! -- some people online
-- call this squaring). this gives us a function g = c x + d which tells us the
-- position of the item in position x after we repeat f that aforementionedly
-- ungodly number of times.
--
-- finally we compute x = c^(-1) ( g - d ), which tells us the initial position
-- of the card that at the end is in position g.
--
-- this required me to go into Big_Integer. i read that some people solved it
-- without going that far, but i'm pretty happy with what's here, especially
-- as i worked it out without too much assistance, and once i actually got it
-- working correctly i gotit on the first try.
--
-- besides, it's not another evil maze!

pragma Ada_2020;

-- Ada libraries

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

procedure Main is

   -- SECTION
   -- which input to process, examples or actual data

   Testing : constant Boolean := False;
   type Valid_Examples is new Positive range 1 .. 4;
   Which_Test : constant := 1;

   -- SECTION
   -- input-related

   F : File_Type; -- input file

   Filename : constant String
      := (
          if Testing then (
             if Which_Test = 1
             then "/Users/user/common/Ada/AoC2019/Day22/example.txt"
             elsif Which_Test = 2
             then "/Users/user/common/Ada/AoC2019/Day22/example2.txt"
             elsif Which_Test = 3
             then "/Users/user/common/Ada/AoC2019/Day22/example3.txt"
             elsif Which_Test = 4
             then "/Users/user/common/Ada/AoC2019/Day22/example4.txt"
             else "/Users/user/common/Ada/AoC2019/Day22/example.txt"
            )
          else "/Users/user/common/Ada/AoC2019/Day22/input.txt"
         );

   function Parameter (S : String) return Integer is
   -- so long as there is an integer at the end of S, this returns that integer
   -- if no such integer appears at the end of S, you're in deep trouble

      Result: Integer := 0;
      Pos : Natural := S'First;
      Is_Negative: Boolean := False;

   begin

      -- find the integer's start
      while S (Pos) /= '-' and not Is_Digit (S (Pos)) loop
         Pos := Pos + 1;
      end loop;

      -- if negative, remember this
      if S (Pos) = '-' then
         Is_Negative := True;
         Pos := Pos + 1;
      end if;

      -- read the integer
      while Pos <= S'Last loop
         Result := Result * 10;
         Result := Result + Character'Pos (S (Pos)) - Character'Pos ('0');
         Pos := Pos + 1;
      end loop;

      -- make negative if need be
      if Is_Negative then Result := -Result; end if;

      return Result;

   end Parameter;

   -- SECTION
   -- debugging

   Verbose  : Boolean := False;

   -- SECTION
   -- deck-related

   Num_Cards : constant := ( if Testing then 10 else 10007 ); -- part 1
   Num_Cards_2 : constant := 119315717514047; -- part 2

   -- using mod types was a nice boon once I got them working, but at least gnat
   -- doesn't work well when you add negatives: you get what looks like
   -- 2^n - a instead of m - a (m is modulus, a is value subtracting, n is
   -- number of bits required to store modulus)
   --
   -- also gnat limits the size of a mod type to 32 bits

   type Card_Range is mod Num_Cards;
   type Deck_Array is array ( Card_Range ) of Card_Range;

   -- initially I used a buffered deck in part 1, but while working on part 2
   -- I found that I could use a completely different approach. I decided to
   -- keep the buffered deck for the sake of it

   type Alternate_Deck is mod 2;
   type Buffered_Deck is array ( Alternate_Deck ) of Deck_Array;

   type Deck_Type is record
      Cards  : Buffered_Deck
         := ( 0 => ( for I in Card_Range => I ), 1 => ( others => 0 ) );
      -- we use a buffered deck to make it easier to perform the shuffles;
      -- just copy from the active Deck_Array to the inactive one
      Active : Alternate_Deck := 0;
      -- changes after every shuffle, so that the newest deck is always Active
   end record;

   -- SECTION
   -- shuffle-related

   -- SUBSECTION
   -- subprogarms used when performing part 1 on a buffered deck

   procedure Deal_Into_New_Stack (Deck : in out Deck_Type) is
   -- essentially reverses the deck
      Source renames Deck.Cards (Deck.Active);
      Target renames Deck.Cards (Deck.Active + 1);
   begin
      for I in Card_Range'Range loop
         Target (I) := Source (Card_Range'Last - I);
      end loop;
      Deck.Active := @ + 1;
   end Deal_Into_New_Stack;

   procedure Cut_Number (N : Integer; Deck: in out Deck_Type) is
   -- if N is positive, removes the first N cards from the top and places them
   -- on the bottom; this is equivalent to moving each card N + 1 positions
   --
   -- if N is negative, removes the last |N| cards from the bottom and places
   -- them on the top; this is equivalent to moving each card M - |N| + 1
   -- positions, where M is the number of cards total

      Source renames Deck.Cards (Deck.Active);
      Target renames Deck.Cards (Deck.Active + 1);
      Shift: Integer := N;

   begin

      if Shift < 0 then Shift := Integer ( Card_Range'Last ) + Shift + 1; end if;
      for Each in Card_Range'Range loop
         Target (Each) := Source ( Each + Card_Range(Shift) );
      end loop;
      Deck.Active := @ + 1;

   end Cut_Number;

   function Modular_Inverse (A : Long_Long_Integer; B : Long_Long_Integer)
                             return Long_Long_Integer
   is
   -- if A > B, computes the inverse of B modulo A;
   -- if A < B, computes the inverse of A modulo B
   --
   -- this relies on a variant of the Extended Euclidean Algorithm, found in
   -- most number theory textbooks. the iteration requires one to remember some
   -- past computations

      Result      : Long_Long_Integer := 1;
      Old_Result  : Long_Long_Integer := 0;
      Next_Result : Long_Long_Integer;

      M           : Long_Long_Integer := Long_Long_Integer'Max (A, B);
      N           : Long_Long_Integer := Long_Long_Integer'Min (A, B);
      Modulus     : Long_Long_Integer := M;

      Q, R        : Long_Long_Integer; -- quotient, remainder

   begin

      if Verbose then Put_Line (Old_Result'Image); end if;

      loop

         Q := M / N; R := M mod N;

         if Verbose then
            Put_Line (Result'Image & M'Image & N'Image & Q'Image & R'Image);
         end if;

         M := N; N := R;
         Next_Result := ( Old_Result - Q * Result ) mod Modulus;
         Old_Result := Result;
         Result := Next_Result;

         exit when R = 0;

      end loop;

      return Old_Result;

   end Modular_Inverse;

   procedure Deal_With_Increment (N : Positive; Deck : in out Deck_Type) is
   -- places the first card in the first position, the second card N positions
   -- later, the third card N positions after that, etc.
   --
   -- this is equivalent to placing the card in position i in position i * N,
   -- and can be programmed that way if desired, but
   -- to determine the correct destination when we know the current position,
   -- use the modular inverse

      Source renames Deck.Cards (Deck.Active);
      Target renames Deck.Cards (Deck.Active + 1);

      M : Card_Range
         := Card_Range ( Modular_Inverse (
                         Long_Long_Integer (N),
                         Long_Long_Integer (Card_Range'Last) + 1
                        ) );

   begin

      for Each in Card_Range'Range loop
         Target (Each) := Source (Each * M);
      end loop;
      Deck.Active := @ + 1;

   end Deal_With_Increment;

   My_Deck: Deck_Type; -- where we perform all the operations for part 1

   -- SUBSECTION
   -- types and subprograms for performing part 1 without a full deck

   type Shuffle_Types is ( Backward, Increment, Cut);

   function New_Position (
                          Start   : Integer;
                          Modulus : Integer;
                          Shuffle : Shuffle_Types;
                          Offset  : Integer
                         ) return Integer
   is
   -- returns the position of the card at position Start after the given Shuffle
   -- with stated Offset
      (
        case Shuffle is
            when Backward => Modulus - Start - 1,
            when Increment => Start * Offset mod Modulus,
            when Cut => (
                           if Offset > 0 then Start + Modulus - Offset
                           else Start + abs (Offset)
                          )
       );

   Card_Pos  : Integer := 2019;

   -- SECTION
   -- material for part 2
   -- doing part 1 with a buffered deck is mainly a waste of time
   -- as far as part 2 is concerned

   -- SUBSECTION
   -- linear functions
   -- m x + b (modulo Num_Cards_2) is the position of the card that started in
   -- position x

   type Linear_Function is record
   -- represents the function M x + B
      M, B: Long_Long_Integer;
   end record;

   Composed_Function : Linear_Function := ( 1, 0 );
   -- this will be the function that determines the destination position
   -- of a card in in position X after performing all the shuffles in part 2;
   -- begins as the identity function
   Composed_Function_Part_1 : Linear_Function := ( 1, 0 );
   -- same as Composed_Function, but this is used to verify the result for pt 1
   Inverse_Of_Composed : Linear_Function;
   -- inverse function for part 2

   procedure Adjust_Function (
                     F       : in out Linear_Function;
                     Modulus : Long_Long_Integer;
                     Shuffle : Shuffle_Types;
                     Offset  : Integer
                             )
   is
   -- replace F with the resulting function which comes of performing the
   -- indicated Shuffle with Offset on the linear function F, where Modulus
   -- indicates the number of cards

   begin

      case Shuffle is

         when Backward =>
            F.M := Modulus - F.M;
            F.B := Modulus - F.B - 1;

         when Increment =>
            F.M := ( F.M * Long_Long_Integer( Offset ) ) mod Modulus;
            F.B := ( F.B * Long_Long_Integer( Offset ) ) mod Modulus;

         when Cut =>
            if Offset >= 0 then
               F.B := ( F.B + Modulus - Long_Long_Integer ( Offset ) ) mod Modulus;
            else
               F.B := ( F.B + Long_Long_Integer( abs(Offset) ) ) mod Modulus;
            end if;

      end case;

   end Adjust_Function;

   -- SUBSECTION
   -- converting to / from Big_Integers
   --
   -- I needed Big_Integers to perform fast exponentiation; I understand from
   -- some of the reading I did that this is not strictly necessary and maybe
   -- I will go back and revise it one day but I doubt it

   package Big_Int_Conversion is new Signed_Conversions
      (
       Int => Long_Long_Integer
      );
   function To_BI (Arg : Long_Long_Integer) return Big_Integer
                      renames Big_Int_Conversion.To_Big_Integer;
   function From_BI ( Arg : Big_Integer ) return Long_Long_Integer
                     renames Big_Int_Conversion.From_Big_Integer;

   procedure Fast_Exponentiation (
                                  F       : in out Linear_Function;
                                  Power   : Long_Long_Integer;
                                  Modulus : Long_Long_Integer
                                 )
   is
   -- determines the function that results from applying F, Power times,
   -- modulo the Modulus
   --
   -- for instance, consider f^5 when f(x) = 3 x + 7, modulo 10.
   -- here, the power indicates repeated composition rather than repeated
   -- multiplication; thus,

   --    - f^2(x) = f ( f(x) ) = 3 ( 3 x + 7 ) + 7 = 9 x + 8
   --    - f^3(x) = f^2 ( f(x) ) = 9 ( 3 x + 7 ) + 8 = 7 x + 1
   --    - f^4(x) = f^2 ( f^2(x) ) = 9 ( 9 x + 8 ) + 8 = x
   --    - f^5(x) = f^3 ( f^2(x) ) = 7 ( 9 x + 8 ) + 1 = 3 x + 7
   --             = f^4 ( f(x) ) = 3x + 7
   --
   -- notice that applying the power 4 times gives the identity; this is on
   -- account of Euler's Theorem, which I did not use because it did not seem
   -- advantageous given the powers involved
   --
   -- notice also that we can combine powers in different ways to obtain the
   -- same result. this leads to the idea of fast exponentiation by repeated
   -- squaring: compute squares of F, write Power as a sum of powers of 2,
   -- and whenever the coefficient of that power (the bit!) is 1,
   -- multiply the result by the current square

      Divided_Power : Big_Integer := To_BI (Power);
      -- we store the power here and "shift right" (divide by 2) on each pass

      Big_Mod       : Big_Integer := To_BI (Modulus);

      Square_M      : Big_Integer := To_BI (F.M);
      Square_B      : Big_Integer := To_BI (F.B);

      Result_M      : Big_Integer := 1;
      Result_B      : Big_Integer := 0;

   begin

      while Divided_Power /= 0 loop

         if Divided_Power mod 2 = 1 then
            -- this square matters, so apply it to the result
            Divided_Power := Divided_Power - 1;
            Result_M := ( Result_M * Square_M ) mod Big_Mod;
            Result_B := ( Result_B * Square_M + Square_B ) mod Big_Mod;
         end if;

         -- adjust the power and compute the next square of f
         Divided_Power := Divided_Power / 2;
         Square_B := ( Square_M * Square_B + Square_B ) mod Big_Mod;
         Square_M := ( Square_M * Square_M ) mod Big_Mod;

      end loop;

      -- store in f
      F.M := From_BI (Result_M);
      F.B := From_BI (Result_B);

   end Fast_Exponentiation;

begin

   -- part 1

   Open (F, In_File, Filename);

   while not End_Of_File (F) loop

      declare Technique : String := Get_Line (F);
      begin

         -- visual inspection of the input shows that it's sane: we're looking
         -- at "deal with ...", "deal in...", and "cut ..." so reading is
         -- relatively simple
         --
         -- I originally worked with a buffered deck, but while exploring part 2
         -- I realized that I could compute the position of a desired card while
         -- passing through this, which would provide a nice validation of what
         -- I was thinking at the time, and I could also determine the shuffle
         -- function, which I could also use to validate my ideas
         --
         -- however part 2 has a different modulus so in this version
         -- Composed_Function and Adjust_Function don't work as a validation
         --
         -- it would not be hard to add another Adjust_Function on a
         -- Part_1_Function with modulus Num_Cards to show what I did before...
         -- but I won't do that now

         if Technique (Technique'First .. Technique'First + 3) = "deal" then

            if Technique (Technique'First + 5) = 'i' then

               -- deal into new stack
               Deal_Into_New_Stack (My_Deck);
               Card_Pos := New_Position (Card_Pos, Num_Cards, Backward, 0);
               Adjust_Function(Composed_Function, Num_Cards_2, Backward, 0);
               Adjust_Function(Composed_Function_Part_1, Num_Cards, Backward, 0);

            else

               -- deal with increment
               declare Value: Integer := Parameter(Technique);
               begin
                  Deal_With_Increment (Parameter (Technique), My_Deck);
                  Card_Pos := New_Position (Card_Pos, Num_Cards, Increment, Value);
                  Adjust_Function (Composed_Function, Num_Cards_2, Increment, Value);
                  Adjust_Function(Composed_Function_Part_1, Num_Cards, Increment, Value);
               end;

            end if;

         else

            -- cut
            declare Value: Integer := Parameter(Technique);
            begin
               Cut_Number (Parameter (Technique), My_Deck);
               Card_Pos := New_Position (Card_Pos, Num_Cards, Cut, Value);
               Adjust_Function(Composed_Function, Num_Cards_2, Cut, Value);
               Adjust_Function(Composed_Function_Part_1, Num_Cards, Cut, Value);
            end;

         end if;

      end;

   end loop;

   Close(F);

   -- report part 1

   if Testing then

      -- the tests are all on small decks (10 cards) so we can just print them
      -- out and check against the examples given

      for I in Card_Range loop
         Put (Natural (My_Deck.Cards (My_Deck.Active) (I)), 0);
         Put (' ');
      end loop;
      New_Line;

   else

      -- first approach was to find 2019 in the deck
      -- inefficient, but it did the job!

      declare
         I : Card_Range := 0;
         Deck renames My_Deck.Cards(My_Deck.Active);
      begin

         while (not Testing) and then Deck (I) /= 2019 loop
            I := I + 1;
         end loop;
         Put (Natural ( i ), 0);
         New_Line;

      end;

      Put_Line (Card_Pos'Image);

      Put_Line ( Long_Long_Integer'Image(
                 (
                 Long_Long_Integer(2019) * Composed_Function_Part_1.M
                 + Composed_Function_Part_1.B
                ) mod Long_Long_Integer( Num_Cards )
                ) );

   end if;

   -- part 2
   -- determine the inverse of Composed_Function,
   -- then use that to calculate the original position of the card that is
   -- shuffled to position 2020
   --
   -- the formula for this is given above in the introduction

   declare

      Normalizer : Long_Long_Integer
         := Modular_Inverse ( Num_Cards_2, Composed_Function.M );
      -- inverse of m

      Temp       : Big_Integer
         := To_BI (Normalizer) * ( To_BI (Num_Cards_2)
                                   - To_BI (Composed_Function.B) )
      mod To_BI (Num_Cards_2);
      -- we need Big_Integers here to prevent an overflow

   begin
      Inverse_Of_Composed := ( M => Normalizer, B => From_BI( Temp ) );
   end;

   -- might be interesting to someone (it was useful to me)
   Put_Line (
             "function is" & Long_Long_Integer'Image (Composed_Function.M)
             & " x +" & Long_Long_Integer'Image (Composed_Function.B)
            );
   Put_Line (
             "inverse is" & Long_Long_Integer'Image (Inverse_Of_Composed.M)
             & " x +" & Long_Long_Integer'Image (Inverse_Of_Composed.B)
            );

   -- finally we do the work!
   Fast_Exponentiation (Inverse_Of_Composed, 101741582076661, Num_Cards_2);
   declare Card_In_2020 : Big_Integer := 2020;
   begin
      Card_In_2020 := ( @ * To_BI (Inverse_Of_Composed.M)
         + To_BI (Inverse_Of_Composed.B) ) mod To_BI(Num_Cards_2);
      Put_Line(Card_In_2020'Image & " is in position 2020");
   end;

end Main;
