-- Advent of Code 2019, Day 4
--
-- John Perry
--
-- Secure Container
--
-- part 1: determine how many numbers in a certain range
-- satisfy certain requirements
--
-- part 2: same, with slightly more stringent requirements
--

pragma Ada_2020;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Main is

   -- we'll take advantage of this Ada feature
   subtype Password_Range is Positive range 367479 .. 893698;

   function Is_Six_Digit(N: Password_Range) return Boolean is
   -- this should always be true, frankly, but whatever
   ( 99999 < N and N < 1000000 );


   function Digit(N: Password_Range; D: Natural) return Natural is
   -- returns the Dth digit of N
   ( ( (N - N mod 10 ** D ) - (N - N mod 10 ** (D + 1) ) ) / 10 ** D );

   function Has_Repeated_Digit(N: Password_Range) return Boolean is
   -- returns true iff at least one of N's digits repeats
   ( for some I in 0 .. 5 => Digit(N, I) = Digit(N, I + 1) );

   function Has_Doubled_Digit(N: Password_Range) return Boolean is
   -- returns true iff at least one of N's digits is repeated once but not twice
   (
         ( Digit(N, 0) = Digit(N, 1) and then Digit(N, 0) /= Digit(N, 2) )
         or else
         (
          for some I in 1 .. 4 =>
                ( Digit(N, I) = Digit(N, I + 1)
                      and then Digit(N, I) /= Digit(N, I - 1)
                      and then Digit(N, I) /= Digit(N, I + 2)
                     )
              )
          or else
          ( Digit(N, 4) /= Digit(N, 5) and then Digit(N, 5) = Digit(N, 6) )
   );

   function Digits_Never_Decrease(N: Password_Range) return Boolean is
   -- ensure the digits never decrease
   ( for all I in 0 .. 5 => Digit(N, I) >= Digit(N, I + 1) );

begin

   -- some test cases
--     Put(Boolean'Image(Has_Doubled_Digit(444422))); New_Line;
--     Put(Boolean'Image(Digits_Never_Decrease(367770))); New_Line;
--     Put(Boolean'Image(Has_Repeated_Digit(456789))); New_Line;
--     Put(Boolean'Image(Has_Repeated_Digit(368802))); New_Line;
--     Put(Boolean'Image(Digits_Never_Decrease(368802))); New_Line;
--     Put(Boolean'Image(Digits_Never_Decrease(368899))); New_Line;

   -- part 1
   Put_Line("Part 1:");
   declare Number_Of_Valid_Passwords: Natural := 0;
   begin
      for I in Password_Range loop
         if Is_Six_Digit(I)
               and Has_Repeated_Digit(I)
               and Digits_Never_Decrease(I)
         then
            Put(I, 0); New_Line;
            Number_Of_Valid_Passwords := Number_Of_Valid_Passwords + 1;
         end if;
      end loop;
      Put(Number_Of_Valid_Passwords, 0); Put(" valid passwords"); New_Line;
   end;

   -- part 2
   Put_Line("Part 2:");
   declare Number_Of_Valid_Passwords: Natural := 0;
   begin
      for I in Password_Range loop
         if Is_Six_Digit(I)
               and Has_Doubled_Digit(I)
               and Digits_Never_Decrease(I)
         then
            Put(I, 0); New_Line;
            Number_Of_Valid_Passwords := Number_Of_Valid_Passwords + 1;
         end if;
      end loop;
      Put(Number_Of_Valid_Passwords, 0); Put(" valid passwords"); New_Line;
   end;

end Main;
