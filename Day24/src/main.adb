-- Advent of Code 2019, Day 24
--
-- John Perry
--
-- yay, a variant on the game of life (again no maze or intcode)
--
-- part 1: find the first pattern in a small game-of-life that repeats twice
--
-- part 2: hey, the Plutonians did this, so it's recursive, and that can be fun
--
-- I didn't get this on the first try, but I did eventually get it, and I even
-- enjoyed myself doing it.

pragma Ada_2020;

-- Ada libraries

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Ordered_Sets;

procedure Main is

   -- SECTION
   -- input-related

   -- SUBSECTION
   -- which input to process, examples or actual data

   Testing : constant Boolean := False;
   type Valid_Examples is new Positive range 1 .. 4;
   Which_Test : constant := 1;

   -- SUBSECTION
   -- input file

   F : File_Type; -- input file

   Filename : constant String
      := (
          if Testing then (
             if Which_Test = 1
             then "/Users/user/common/Ada/AoC2019/Day24/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day24/input.txt"
            )
          else "/Users/user/common/Ada/AoC2019/Day24/input.txt"
         );

   -- SECTION
   -- setup for part 1

   package Integer_Sets is new Ada.Containers.Ordered_Sets
      (
       Element_Type => Natural
      );

   Biodiversity_Ratings : Integer_Sets.Set;
   -- we need to report the second rating that appears,
   -- so we store past ratings here

   type Eris_Feature is ( Bug, Empty, Recursion );
   -- in part 1, an Eris location can be a Bug or Empty
   -- in part 2, we add Recursion

   Min_C, Min_R         : constant := 1;
   -- minimum valid positions of the grid
   Max_C, Max_R         : constant := 5;
   -- maximum valid positions of the grid
   subtype Row_Range is Integer range Min_R - 1 .. Max_R + 1;
   -- we extend the array beyond min and max to make the checking easier
   subtype Col_Range is Integer range Min_C - 1 .. Max_C + 1;
   -- we extend the array beyond min and max to make the checking easier
   type Eris_Grid_Array is array ( Row_Range, Col_Range ) of Eris_Feature;
   -- type of the data

   Eris_Grid            : Eris_Grid_Array := ( others => ( others => Empty ) );
   -- data for part 1

   procedure Show_Grid (Grid : Eris_Grid_Array) is
      -- prints the grid, which I used for debugging
   begin
      for R in Row_Range loop
         for C in Col_Range loop
            Put ( ( if Grid (R, C) = Bug then '#' else '.' ) );
         end loop;
         New_Line;
      end loop;
   end Show_Grid;

   function Neighbor_Count (R : Row_Range; C : Col_Range) return Natural is
   -- counts the neighbors of grid location (R, C)
   -- this is why i chose to make the array a little larger:
   -- we don't need any complicated if statements to make sure we're in bounds;
   -- just check up, left, right, down
   -- since the "invalid" locations are always empty, they don't affect the
   -- final result
      (
        0 + ( if Eris_Grid (R - 1, C) = Bug then 1 else 0 )
        + ( if Eris_Grid (R, C - 1) = Bug then 1 else 0 )
        + ( if Eris_Grid (R, C + 1) = Bug then 1 else 0 )
        + ( if Eris_Grid (R + 1, C) = Bug then 1 else 0 )
       );

   procedure Minute_Passes is
   -- iterate one minute according to the rules

      Num_Nbrs : Natural;
      -- number of neighbors, evaluated at each position
      New_Grid : Eris_Grid_Array := ( others => ( others => Empty ) );
      -- temporary storage for result

   begin

      -- loop through all positions; count the neighbors; apply the rules

      for R in Min_R .. Max_R loop
         for C in Min_C .. Max_C loop

            Num_Nbrs := Neighbor_Count (R, C);

            if Eris_Grid (R, C) = Bug then
               if Num_Nbrs = 1 then New_Grid (R, C) := Bug; end if;
            else
               if Num_Nbrs = 1 or Num_Nbrs = 2 then
                  New_Grid (R, C) := Bug;
               end if;
            end if;

         end loop;
      end loop;

      -- i initially forgot this in both parts one and two,
      -- DESPITE telling myself both times, "don't forget to do this!"
      Eris_Grid := New_Grid;

   end Minute_Passes;

   function Current_Rating return Natural is
   -- returns the rating of the current grid, by assigning a unique power of 2
   -- to each position

      Grid renames Eris_Grid;

      Value  : array ( Row_Range, Col_Range ) of Natural
      -- i was dreading the complicated if statements necessary to do this,
      -- then remembered that arrays are cool
         := (
             ( others => 0 ),
             ( 0, 1, 2, 4, 8, 16, 0 ),
             ( 0, 32, 64, 128, 256, 512, 0 ),
             ( 0, 1024, 2048, 4096, 8192, 16384, 0 ),
             ( 0, 2 ** 15, 2 ** 16, 2 ** 17, 2 ** 18, 2 ** 19, 0 ),
             ( 0, 2 ** 20, 2 ** 21, 2 ** 22, 2 ** 23, 2 ** 24, 0 ),
             ( others => 0 )
            );

      Result : Natural := 0;

   begin

      -- straightforward, thanks to the Value array
      for R in Row_Range loop
         for C in Col_Range loop
            Result := Result + ( if Grid (R, C) = Bug then Value (R, C) else 0 );
         end loop;
      end loop;
      return Result;

   end Current_Rating;

   -- SECTION
   -- data types and subprograms for part 2

   Number_Of_Minutes    : constant := ( if Testing then 10 else 200 );
   -- how long to iterate the game

   Depth_Constant       : constant := Number_Of_Minutes / 2;
   -- can only expand to a new level every 2 minutes
   Min_Depth            : constant
      := ( if Testing then -5 else -Depth_Constant );
   -- minimum depth of the recursion
   Max_Depth            : constant := ( if Testing then 5 else Depth_Constant );
   -- maximum depth of the recursion
   subtype Recursive_Range is Integer range Min_Depth - 1 .. Max_Depth + 1;
   -- range of depths in grid: as before, we add extra depth
   -- to make the checking easier
   type Larger_Grid_Array is array ( Recursive_Range ) of Eris_Grid_Array;
   -- type of the data for part 2
   Larger_Grid          : Larger_Grid_Array
      := ( others => ( others => ( others => Empty ) ) );
   -- data for part 2

   function Score (D : Integer; R : Row_Range; C : Col_Range) return Natural is
   -- determine the score of a position on the grid:
   -- returns 1 if there's a bug; otherwise returns 0
      ( if Larger_Grid (D) (R, C) = Bug then 1 else 0 );

   function Neighbor_Count_Recursive
      (Depth : Integer; R : Row_Range; C : Col_Range)
       return Natural
   -- returns the number of neighbors that the cell in row R, column C in the
   -- grid at depth D has; the game's rules mean that the only normal positions
   -- occur in the "inner" corners (i,j) where i,j are 2 or 4; for all the
   -- other positions, one has to look at at least one cell at lower or higher
   -- depth
   --
   -- this could be turned into a functional programming-style function, but
   -- I haven't yet figured out Ada's 'Reduce, or perhaps AdaCore hasn't
   -- implemented it yet
   is

      Result : Natural;

   begin

      -- handle the literal corner cases first,
      -- then the non-corner outer rows and columns,
      -- then the innermost rows and columns

      if R = Min_R and then C = Min_C then -- upper left corner
         Result := Score (Depth - 1, 2, 3)
            + Score (Depth - 1, 3, 2)
            + Score (Depth, R, C + 1)
            + Score (Depth, R + 1, C);

      elsif R = Min_R and then C = Max_C then -- upper right corner
         Result := Score (Depth - 1, 2, 3)
            + Score (Depth - 1, 3, 4)
            + Score (Depth, R, C - 1)
            + Score (Depth, R + 1, C);

      elsif R = Max_R and then C = Min_C then -- lower left corner
         Result := Score (Depth - 1, 4, 3)
            + Score (Depth - 1, 3, 2)
            + Score (Depth, R, C + 1)
            + Score (Depth, R - 1, C);

      elsif R = Max_R and then C = Max_C then -- lower right corner
         Result := Score (Depth - 1, 4, 3)
            + Score (Depth - 1, 3, 4)
            + Score (Depth, R, C - 1)
            + Score (Depth, R - 1, C);

      elsif ( R = 2 and then C = 2 ) or else ( R = 2 and then C = 4 )
         or else ( R = 4 and then C = 2 ) or else ( R = 4 and then C = 4 )
      then -- inner corners
         Result := Score (Depth, R - 1, C)
            + Score (Depth, R + 1, C)
            + Score (Depth, R, C - 1)
            + Score (Depth, R, C + 1);

      elsif R = Min_R then -- tile on top row: recurse upward
         Result := Score (Depth - 1, 2, 3)
            + Score (Depth, R, C - 1)
            + Score (Depth, R, C + 1)
            + Score (Depth, R + 1, C);

      elsif R = Max_R then -- tile on bottom row: recurse downward
         Result := Score (Depth - 1, 4, 3)
            + Score (Depth, R, C - 1)
            + Score (Depth, R, C + 1)
            + Score (Depth, R - 1, C);

      elsif C = Min_C then -- tiles on leftmost row: recurse leftward
         Result := Score (Depth - 1, 3, 2)
            + Score (Depth, R, C + 1)
            + Score (Depth, R - 1, C)
            + Score (Depth, R + 1, C);

      elsif C = Max_C then -- tiles on rightmost row: recurse rightward
         Result := Score (Depth - 1, 3, 4)
            + Score (Depth, R, C - 1)
            + Score (Depth, R - 1, C)
            + Score (Depth, R + 1, C);

      elsif R = 2 then -- recurse inwards and down
         Result := Score (Depth, R, C - 1)
            + Score (Depth, R, C + 1)
            + Score (Depth, R - 1, C);
         for I in Min_C .. Max_C loop
            Result := Result + Score (Depth + 1, Min_R, I);
         end loop;

      elsif R = 4 then -- recurse inwards and up
         Result := Score (Depth, R, C - 1)
            + Score (Depth, R, C + 1)
            + Score (Depth, R + 1, C);
         for I in Min_C .. Max_C loop
            Result := Result + Score (Depth + 1, Max_R, I);
         end loop;

      elsif C = 2 then -- recurse inwards and right
         Result := Score (Depth, R, C - 1)
            + Score (Depth, R + 1, C)
            + Score (Depth, R - 1, C);
         for I in Min_R .. Max_R loop
            Result := Result + Score (Depth + 1, I, Min_C);
         end loop;

      elsif C = 4 then -- recurse inwards and left
         Result := Score (Depth, R, C + 1)
            + Score (Depth, R + 1, C)
            + Score (Depth, R - 1, C);
         for I in Min_R .. Max_R loop
            Result := Result + Score (Depth + 1, I, Max_C);
         end loop;

      elsif R = 3 and then C = 3 then -- innermost cell is the recursion
         Result := 0;

      else
         raise Data_Error
            with "case unconsidered: (" & R'Image & C'Image & " )";
         -- this actually helped, because I had forgotten position (3, 3)
         -- (despite telling myself to do otherwise)

      end if;

      return Result;

   end Neighbor_Count_Recursive;

   procedure Minute_Passes_Recursively is
   -- iterate one minute according to the rules of part 2

      Num_Nbrs : Natural;
      -- number of neighbors, evaluated at each position

      New_Grid : Larger_Grid_Array
         := ( others => ( others => ( others => Empty ) ) );
      -- temporary storage for new grid

   begin

      for D in Min_Depth .. Max_Depth loop -- through depths

         for R in Min_R .. Max_R loop -- through rows
            for C in Min_C .. Max_C loop -- through columns


               if (R = 3 and then C = 3)
                  or else R = Min_R - 1 or else R = Max_R + 1
                  or else C = Min_C - 1 or else C = Max_C + 1
               then

                  -- denote the positions that correspond to grids of higher
                  -- or lower depth
                  New_Grid (D) (R, C) := Recursion;

               else

                  -- figure out the neighbors according to the recursive system,
                  -- then update the grid according to the new score
                  Num_Nbrs := Neighbor_Count_Recursive (D, R, C);
                  if Larger_Grid (D) (R, C) = Bug then
                     if Num_Nbrs = 1 then New_Grid (D) ( R, C ) := Bug; end if;
                  else
                     if Num_Nbrs = 1 or Num_Nbrs = 2 then
                        New_Grid (D) ( R, C ) := Bug;
                     end if;
                  end if;

               end if;

            end loop;
         end loop;

      end loop;

      -- i initially forgot this in both parts one and two,
      -- DESPITE telling myself both times, "don't forget to do this!"
      Larger_Grid := New_Grid;

   end Minute_Passes_Recursively;

   procedure Show_Recursive_Grid
      (Min_D : Integer := Min_Depth; Max_D : Integer := Max_Depth)
   is
   -- shows the grids from depth Min_D to depth Max_D
   begin
      for I in Min_D .. Max_D loop
         Put_Line ("Depth" & I'Image & ":");
         Show_Grid (Larger_Grid (I));
         New_Line;
      end loop;
   end Show_Recursive_Grid;

begin

   -- SECTION
   -- process input file

   Open (F, In_File, Filename);

   declare R : Row_Range := Min_R;
   begin

      while not End_Of_File (F) loop

         declare Line : String := Get_Line (F);
         begin

            -- Eris_Grid's positions initialize Empty,
            -- so we only have to change positions that have a Bug
            for C in 1 .. 5 loop
               if Line (Line'First + C - 1) = '#' then
                  Eris_Grid (R, C) := Bug;
               end if;
            end loop;

         end;

         R := R + 1;

      end loop;

   end;

   -- initialize Larger_Grid before we monkey with Eris_Grid
   Larger_Grid (0) := Eris_Grid;
   Larger_Grid (0) (3, 3) := Recursion;
   for I in Col_Range loop
      Larger_Grid (0) (0, I) := Recursion;
      Larger_Grid (0) (6, I) := Recursion;
   end loop;
   for I in 1 .. Max_R loop
      Larger_Grid (0) (I, 0) := Recursion;
      Larger_Grid (0) (I, 6) := Recursion;
   end loop;


   -- SECTION
   -- part 1

   loop

      declare Rating : Natural := Current_Rating; -- rating after each minute
      begin

         Show_Grid (Eris_Grid); -- nice to look at; useful for debugging
         Put_Line (Rating'Image);

         if Biodiversity_Ratings.Contains (Rating) then
            Put_Line ("Biodiversity rating is" & Rating'Image);
            exit;
         else
            Biodiversity_Ratings.Insert (Rating);
            Minute_Passes;
         end if;

      end;

   end loop;

   New_Line; Put_Line ("--------"); New_Line;

   -- SECTION
   -- part 2

   -- let the bugs do their thing
   for I in 1 .. Number_Of_Minutes loop
      Minute_Passes_Recursively;
   end loop;

   if Testing then Show_Recursive_Grid; end if;

   -- now count them
   declare Number_Of_Bugs : Natural := 0;
   begin

      for D in Min_Depth .. Max_Depth loop

         for R in Min_R .. Max_R loop
            for C in Min_C .. Max_C loop

               if Larger_Grid (D) (R, C) = Bug then
                  Number_Of_Bugs := Number_Of_Bugs + 1;
               end if;

            end loop;
         end loop;

      end loop;

      Put_Line (
                "there are" & Number_Of_Bugs'Image & " bugs after "
                & Number_Of_Minutes'Image & " minutes"
               );

   end;

end Main;
