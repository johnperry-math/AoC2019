-- Advent of Code 2019, Day 10
--
-- John Perry
--
-- part 1: detect which location of an asteroid is best to monitor the others
--
-- part 2: destroy 200 asteroids from that base, report modified location
-- of 200th

pragma Ada_2020;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Containers.Ordered_Sets;

procedure Main is

   -- SECTION
   -- input-related

   F: File_Type; -- input file

   Testing: constant Boolean := False;

   Filename: constant String
         := (
             if Testing then "/Users/user/common/Ada/AoC2019/Day10/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day10/input.txt"
            );

   -- SECTION
   -- asteroid field data types and information

   type Field_Element is ( Asteroid, Empty, Destroyed );

   type Asteroid_Field_Array
         is array ( Natural range <>, Natural range <> )
         of Field_Element;

   Field_Dimension: Natural;
   -- this should be the number of rows & columns in the asteroid field

   function Relatively_Prime(A,B: Integer) return Boolean is
   -- returns True iff gcd(A,B) = 1
      M: Natural := Integer'Max(abs(A), abs(B));
      N: Natural := Integer'Min(abs(A), abs(B));
      Q, R: Natural;
   begin
      while N /= 0 loop
         Q := M / N;
         R := M mod N;
         M := N;
         N := R;
      end loop;
      return M = 1;
   end;

   Last_Found_Row, Last_Found_Col: Natural;
   -- these will record the last row and column in which we find an asteroid

   function Found_Asteroid(
                           Field: Asteroid_Field_Array;
                           I,J: Natural;
                           K,L: Integer
                          )
   return Boolean is
   -- returns True iff an asteroid is found in Field,
   -- looking from row I, column J, in the angle determined by adding
   -- multiples of K to I and the same multiples of L to J, AND
   -- Relatively_Prime(K, L) is True (so K = L = 0 is also out)
   --
   -- (the use of Relatively_Prime helps make sure a loop will check every
   -- angle at most once)
   --
   -- also records in Last_Found_Row and Last_Found_Col
   -- the last row & col where this asteroid was found

      Multiple: Positive := 1;
      -- the multiple of K (resp. L) we add to I (resp. J)
      Searching: Boolean := True;
      -- whether we are still searching for an asteroid

   begin

      if ( K /= 0 or L /= 0 ) and then Relatively_Prime(K, L) then

         Searching := True;

         while Searching
               and I + Multiple * K in Field'Range(1)
               and J + Multiple * L in Field'Range(2)
         loop

            if Field(I + Multiple * K, J + Multiple * L) = Asteroid then
               Searching := False;
               Last_Found_Row := I + Multiple * K;
               Last_Found_Col := J + Multiple * L;
            end if;

            Multiple := Multiple + 1;

         end loop;

      end if;

      return not Searching;

   end Found_Asteroid;

begin

   -- get dimension

   Open(F, In_File, Filename);
   declare S: String := Get_Line(F);
   begin
      Field_Dimension := S'Length;
   end;
   Close(F);

   -- SECTION
   -- read asteroid data

   declare Asteroid_Field: Asteroid_Field_Array(
                                                0 .. Field_Dimension - 1,
                                                0 .. Field_Dimension - 1
                                               );
   begin

      Open(F, In_File, Filename);

      for I in Asteroid_Field'Range(1) loop
         for J in Asteroid_Field'Range(2) loop
            declare C: Character;
            begin
               Get(F, C);
               Asteroid_Field(I,J) := ( if C = '.' then Empty else Asteroid );
            end;
         end loop;
      end loop;

      Close(F);

      -- SUBSECTION
      -- part 1: find best location

      declare

         -- types to record locations of good asteroids

         subtype Field_Range is Natural range
               Asteroid_Field'First(1) .. Asteroid_Field'Last(1);

         type Location_Type is record
            Row, Col: Field_Range;
         end record;

         -- values to record locations of good asteroids

         Max_Seen, Seen_By_This: Natural := 0;

         Location_Of_Max_Seen: Location_Type;

      begin

         -- for each position
         Vertical:
         for I in Field_Range loop

            Horizontal:
            for J in Field_Range loop

               if Asteroid_Field(I,J) = Asteroid then

                  Seen_By_This := 0;

                  -- for each slope
                  Slope_X:
                  for K in -Field_Range'Last .. Field_Range'Last loop

                     Slope_Y:
                     for L in -Field_Range'Last .. Field_Range'Last loop

                        if Found_Asteroid(Asteroid_Field,I,J,K,L) then
                           Seen_By_This := Seen_By_This + 1;
                        end if;

                     end loop Slope_Y;

                  end loop Slope_X;

                  -- if this position sees more than all previous positions,
                  -- make a note of it

                  if Seen_By_This > Max_Seen then
                     Max_Seen := Seen_By_This;
                     Location_Of_Max_Seen := ( Row => I, Col => J );
                  end if;

               end if;

            end loop Horizontal;

         end loop Vertical;

         -- report

         Put_Line(
                  "location of best-positioned asteriod is "
                  & Location_Of_Max_Seen.Col'Image & ", "
                  & Location_Of_Max_Seen.Row'Image
                 );
         Put_Line("it detects " & Max_Seen'Image & " asteroids");

         -- SUBSECTION
         -- part 2

         declare

            -- in this case it's important to consider each angle in correct
            -- order, so we set up an ordered set for the angles in each
            -- quadrant

            type Angle is record
               -- angle of view and/or of lasert
               Dx, Dy: Integer;
            end record;

            function "<"(First, Second: Angle) return Boolean is
               -- orders angles in clockwise order
                  ( First.Dx * Second.Dy > First.Dy * Second.Dx );

            package Angle_Sets is new Ada.Containers.Ordered_Sets
                  (
                   Element_Type => Angle
                  );

            New_Angles: Angle_Sets.Set;

            -- make sure we don't exceed our mandate

            Number_Destroyed: Natural := 0;
            function Finished return Boolean is ( Number_Destroyed = 200 );

            -- convenience (REALLY nice convenience)

            Base renames Location_Of_Max_Seen;

         begin

            Global_Loop:
            while not Finished loop

               Quadrant_Loop:
               for Quad in 1 .. 4 loop

                  Put_Line("quadrant " & Quad'Image);

                  -- I got a little carried away with case expressions
                  -- in this part

                  -- quadrantal angles
                  if Found_Asteroid(
                                    Asteroid_Field,
                                    Base.Row, Base.Col,
                                    K => (
                                          case Quad is
                                             when 1 => -1, when 2 => 0,
                                             when 3 =>  1, when 4 => 0
                                         ),
                                    L => (
                                          case Quad is
                                             when 1 => 0, when 2 => 1,
                                             when 3 => 0, when 4 => -1
                                         )
                                   )
                  then

                     Number_Destroyed := Number_Destroyed + 1;
                     Asteroid_Field(Last_Found_Row, Last_Found_Col) := Destroyed;
                     Put_Line(
                              Number_Destroyed'Image
                              & " destroyed asteroid at "
                              & Last_Found_Col'Image & ", "
                              & Last_Found_Row'Image
                             );
                  end if;

                  -- in-quadrant angles

                  New_Angles.Clear; -- eliminate old angles

                  if not Finished then -- in other words, skip this if you are!

                     Through_Rows:
                     for I in 1 .. Field_Range'Last loop

                        Through_Cols:
                        for J in 1 .. Field_Range'Last loop

                           if Relatively_Prime(I,J) then
                              New_Angles.Insert( (
                                                 Dx => (
                                                        case Quad is
                                                        when 1 | 2 => I,
                                                        when 3 | 4 => -I
                                                       ),
                                                 Dy => (
                                                        case Quad is
                                                        when 1 | 4 => -J,
                                                        when 2 | 3 => J
                                                       )
                                                ) );
                           end if;

                        end loop Through_Cols;

                     end loop Through_Rows;

                     for Angle of New_Angles loop

                        if Found_Asteroid(
                                          Asteroid_Field,
                                          Base.Row, Base.Col,
                                          Angle.Dy, Angle.Dx
                                         )
                        then

                           Number_Destroyed := Number_Destroyed + 1;
                           Asteroid_Field(Last_Found_Row, Last_Found_Col)
                                 := Destroyed;
                           Put_Line(
                                    Number_Destroyed'Image
                                    & " destroyed asteroid at "
                                    & Last_Found_Col'Image & ", "
                                    & Last_Found_Row'Image
                                   );

                        end if;

                        exit when Finished;

                     end loop;

                  end if;

                  exit when Finished;

               end loop Quadrant_Loop;

            end loop Global_Loop;

            Put_Line(
                     "200th asteroid destroyed at " & Last_Found_Col'Image
                     & ", " & Last_Found_Row'Image
                    );
            Put("the magic number is ");
            Put(Last_Found_Col * 100 + Last_Found_Row, 0); New_Line;

         end;

      end;

   end;

end Main;
