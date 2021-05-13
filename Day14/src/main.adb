-- Advent of Code 2019, Day 14
--
-- John Perry
--
-- Space stoichiometry
--
-- part 1: determine how much ORE you need to create 1 FUEL
--
-- part 2: determine how much FUEL you can create with 1 trillion ORE
--
-- both parts were a bit tough, to a large extent because you have to take
-- into account how to use leftover minerals.
-- i came up with a solution to part 2 that I think will work and I think is not
-- entirely unclever, but I don't know, because 10 1/2 passed
-- before it completed, and by that time I had devised
-- a better solution, thanks in large part to pr0g's rust-based solution
-- on GitHub.
--
-- my original version generates 1 unit of fuel at a time,
-- waiting to see at what quantity of fuel all the minerals are exhausted,
-- at which point it divides the 1 trillion by that amount of fuel generated,
-- then computes how much it can compute with what's left.
-- whether this works in reasonable time apparently depends very much
-- on your luck at drawing good numbers.
--
-- the new version, which is not actually a copy of pr0g's version,
-- but is certainly inspired by it, iteratively calculates
-- how much ore is necessary to produce a given amount of fuel,
-- subtract that from the amount of ore available, then repeat.
-- the amount of fuel generatedis the quotient of the amount of ore left
-- and the amount of ore necessary to generate 1 fuel.
--
-- even at this point I didn't have it quite right; it was only when I realized
-- that even if there isn't enouhg ore to generate 1 fuel, there can still be
-- enough ore AND minerals left to generate 1 fuel.
--
-- as an aside, a lot of solutions I saw online are actually wrong in general!
-- I suppose they simply got lucky with how the system worked on their formulas?

pragma Ada_2020;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Containers.Indefinite_Ordered_Maps;

with Ada.Strings.Fixed;

procedure Main is

   -- SECTION
   -- input-related

   F: File_Type; -- input file

   Testing: constant Boolean := False;

   Filename: constant String
         := (
             if Testing then "/Users/user/common/Ada/AoC2019/Day14/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day14/input.txt"
            );

   -- SECTION
   -- data structures

   subtype Mineral is String( 1 .. 6 );
   -- what's in a name? a mineral by any other name would smell just as bitter

   -- a couple of names we'll need often
   Fuel_String: constant Mineral := "FUEL  ";
   Ore_String: constant Mineral := "ORE   ";

   type Ingredient is record
   -- an ingredient needed to make another ingredient
      Name: Mineral := ( others => ' ');
      Amount: Positive;
   end record;

   type Requirements is array( Positive range <> ) of Ingredient;
   -- the requirements for a recipe

   type Reaction ( Num_Reqs: Positive ) is record
      Resulting_Amount: Positive;
      Reqs: Requirements( 1 .. Num_Reqs );
   end record;

   package Reaction_Maps is new Ada.Containers.Indefinite_Ordered_Maps
   -- how to store the recipes
   (
    Key_Type => Mineral,
    Element_Type => Reaction
   );

   Recipe_Book: Reaction_Maps.Map;
   -- actual storage for the recipes

   -- SECTION
   -- subprograms for I/O

   procedure Read_Ingredient(
                             S: String; Pos: in out Positive;
                             Put_In: in out Reaction;
                             Entry_Number: Positive
                            )
   is
   -- reads from S, starting at Pos, an ingredient,
   -- as well as the associated quantity,
   -- and stores it in Put_In.Reqs(Entry_Number)

      Ing_Num: Positive := 1;
      -- ingredient number

   begin

      -- get quantity consumed
      Get(S(Pos .. S'Last), Put_In.Reqs(Entry_Number).Amount, Pos);
      Pos := Pos + 2;

      -- get ingredient
      while S(Pos) in 'A' .. 'Z' loop
         Put_In.Reqs(Entry_Number).Name(Ing_Num) := S(Pos);
         Pos := Pos + 1;
         Ing_Num := Ing_Num + 1;
      end loop;

   end Read_Ingredient;

   procedure Read_Reaction(S: String) is
   -- reads and stores in Recipe_Book the formula defined by S

      Pos: Positive := S'First;
      -- where we are in the string

      Number: Positive := Ada.Strings.Fixed.Count(S, ",") + 1;
      -- which ingredient we're reading

      New_Reaction: Reaction(Number);
      -- the formula itself

      Name: Mineral := ( others => ' ' );
      -- temporary storage for the ingredient name

      I: Positive := 1;
      -- where in the Name we are assigning

   begin

      -- read and store each ingredient, one at a time
      -- the data is comma-separated
      Number := 1;
      loop
         Read_Ingredient(S, Pos, New_Reaction, Number);
         exit when S(Pos) /= ',';
         Pos := Pos + 2;
         Number := Number + 1;
      end loop;

      -- advance past "=> "
      Pos := Pos + 4;

      -- read quantity produced
      Get(S(Pos .. S'Last), New_Reaction.Resulting_Amount, Pos);

      -- read the name
      Pos := Pos + 2;
      while Pos <= S'Last loop
         Name(I) := S(Pos);
         Pos := Pos + 1;
         I := I + 1;
      end loop;

      Recipe_Book.Insert(Name, New_Reaction);

   end Read_Reaction;

   package Available_Package is new Ada.Containers.Indefinite_Ordered_Maps
   -- how we track how much is available of each memory
   (
    Key_Type => Mineral,
    Element_Type => Long_Long_Integer
   );

   Mineral_Available: Available_Package.Map;
   -- how much is available of each type

   Ore_Needed: Long_Long_Integer := 0;
   -- how much ore is needed to build the requested amount of fuel

   procedure Produce_Mineral(
                             Name: Mineral; Amount_Needed: Long_Long_Integer;
                             Verbose: Boolean := False
                            )
   is
   -- DO NOT PASS ORE TO THIS
   --
   -- produces everything except ORE; records how much ORE is needed;
   -- updates Mineral_Available after producing Amount_Needed,
   -- but not consuming it
   --
   -- to see how this work, set Verbose to True

      Recipe: Reaction_Maps.Reference_Type renames Recipe_Book(Name);
      -- the Recipe for Name

      Scale: Long_Long_Integer
      -- how much to scale each quantity
            := Amount_Needed / Long_Long_Integer(Recipe.Resulting_Amount)
                  + (
                     if Amount_Needed
                           mod Long_Long_Integer(Recipe.Resulting_Amount) = 0
                     then 0
                     else 1
                    );

      Tmp_Lli: Long_Long_Integer;
      -- used solely for I/O

   begin

      -- there's no need to do any work if we already have the material
      if Mineral_Available(Name) >= Amount_Needed then

         if Verbose then
            Put_Line("already have " & Amount_Needed'Image & " of " & Name);
         end if;

      else

         if Verbose then
            Put_Line("producing " & Amount_Needed'Image & " of " & Name);
         end if;

         -- to produce the material, produce each of its required ingredients
         for I in 1 .. Recipe.Num_Reqs loop

            declare

               -- some conveniences
               Entry_Name renames Recipe.Reqs(I).Name;
               Entry_Amount_Needed: Long_Long_Integer
                     := Long_Long_Integer(Recipe.Reqs(I).Amount) * Scale;

               Additional_Need: Long_Long_Integer
               -- amount of the ingredient that we need to produce
               -- for this material
                     := (
                         if Entry_Amount_Needed > Mineral_Available(Entry_Name)
                         then
                            Entry_Amount_Needed - Mineral_Available(Entry_Name)
                         else
                            0
                        );

            begin

               if Verbose then
                  Put_Line(
                           Amount_Needed'Image & " " &
                                 Name & " requires" & Entry_Amount_Needed'Image & " of "
                           & Entry_Name
                          );
               end if;

               if Entry_Name = Ore_String then -- ore has no ingredients

                  Ore_Needed := Ore_Needed + Additional_Need;

               else

                  if Verbose then
                     Put_Line(
                              "still need" & Additional_Need'Image
                              & " of " & Entry_Name
                             );
                  end if;

                  declare

                     Ingredient_Definition: Reaction
                           renames Recipe_Book.Element(Entry_Name);

                     -- figure out how many time we need to run the nanofactory
                     -- to produce this ingredient

                     Reactions_Needed: Long_Long_Integer
                           := Additional_Need
                                 / Long_Long_Integer(
                                                     Ingredient_Definition
                                                     .Resulting_Amount
                                                    )
                           + (
                              if Additional_Need
                                    mod Long_Long_Integer(
                                          Ingredient_Definition.Resulting_Amount
                                   )
                              /= 0
                              then 1 else 0
                             );

                     Ingredient_Produced: Long_Long_Integer
                     -- how much we will produce, given the number of reactions
                           := Long_Long_Integer(
                                                Ingredient_Definition
                                                .Resulting_Amount
                                               )
                                 * Reactions_Needed;

                  begin

                     if Verbose then
                        Tmp_Lli := Mineral_Available(Entry_Name);
                        Put_Line("we had" & Tmp_Lli'Image & " " & Entry_Name);
                        Put_Line("to produce " & Amount_Needed'Image & " of " & Name
                                 & " required" & Additional_Need'Image & " units of "
                                 & Entry_Name & ", so"
                                 & Reactions_Needed'Image & " reactions");
                     end if;

                     -- produce it
                     Produce_Mineral(Entry_Name, Ingredient_Produced);

                     if Verbose then
                        Tmp_Lli := Mineral_Available(Entry_Name);
                        Put_Line("from" & Tmp_Lli'Image & " " & Entry_Name
                                 & " we take" & Entry_Amount_Needed'Image
                                );
                     end if;

                     -- now consume it
                     Mineral_Available(Entry_Name)
                           := Mineral_Available(Entry_Name)
                                 - Entry_Amount_Needed;

                     if Verbose then
                        Tmp_Lli := Mineral_Available(Entry_Name);
                        Put_Line("now have" & Tmp_Lli'Image & " left of " & Entry_Name);
                     end if;

                  end;

               end if;

            end;

         end loop;

         -- record the production
         Mineral_Available(Name)
               := Mineral_Available(Name)
                     + Long_Long_Integer(Recipe.Resulting_Amount) * Scale;

         if Verbose then
            Tmp_Lli := Mineral_Available(Name);
            Put_Line(
                     "after production have" & Tmp_Lli'Image & " " & Name
                    );
         end if;

      end if;

   end Produce_Mineral;

   -- material needed for part 2

   Ore_Needed_For_One: Long_Long_Integer;
   -- the amount of ore needed to produce one fuel
   Ore_Available: Long_Long_Integer := 0;
   -- the amount of ore we have available

begin

   -- SECTION
   -- read all reactions

   Open(F, In_File, Filename);

   while not End_Of_File(F) loop
      Read_Reaction(Get_Line(F));
   end loop;

   Close(F);

   -- SECTION
   -- part 1

   -- SUBSECTION
   -- set up the amount of each mineral available (0)

   declare C: Reaction_Maps.Cursor := Recipe_Book.First;
   begin
      while Reaction_Maps."/="(C, Reaction_Maps.No_Element) loop
         Mineral_Available.Insert(Reaction_Maps.Key(C), 0);
         Reaction_Maps.Next(C);
      end loop;
   end;
   Mineral_Available.Insert(Ore_String, 0);

   -- SUBSECTION
   -- produce 1 fuel, report how much ore is needed

   Produce_Mineral(Fuel_String, 1);
   Mineral_Available(Fuel_String) := Mineral_Available(Fuel_String) - 1;
   Put_Line("we need" & Ore_Needed'Image & " ORE for FUEL");

   -- SECTION
   -- part 2

   -- SUBSECTION
   -- reset the data, remember how many are needed for 1 fuel

   Ore_Needed_For_One := Ore_Needed;
   Ore_Needed := 0;

   declare
      package MAP renames Available_Package;
      C: MAP.Cursor := Mineral_Available.First;
   begin
      while MAP."/="(C, MAP.No_Element) loop
         -- uncomment if you'd like a report (it's interesting)
         -- Put_Line(
         --          MAP.Key(C)'Image & " has"
         --          & MAP.Element(C)'Image & " remaining"
         --         );
         Mineral_Available.Replace_Element(C, 0);
         MAP.Next(C);
      end loop;
   end;
   New_Line;

   -- SUBSECTION
   -- determine how much fuel can be produced from 1 trillion ore

   Put_Line("from 1 trillion ore...");

   declare

      -- obvious from meaning
      Ore_Available: Long_Long_Integer := 1_000_000_000_000;
      Fuel_Produced: Long_Long_Integer := 0;
      Ore_Used: Long_Long_Integer := 0;

      -- how much fuel we produce on a given turn
      New_Fuel: Long_Long_Integer;

   begin

      loop

         -- produce as much of the fuel as we think we can do with what's left
         New_Fuel := Ore_Available / Ore_Needed_For_One;
         if New_Fuel = 0 then New_Fuel := 1; end if;
         Produce_Mineral(Fuel_String, New_Fuel);

         -- set fuel aside
         Mineral_Available(Fuel_String) := 0;
         -- uncomment if you'd like a report (it's interesting)
         -- Put_Line("need" & Ore_Needed'Image & " to produce" & New_Fuel'Image);
         -- Put_Line("have" & Ore_Available'Image);

         -- termination condition: when we can't produce what we need
         exit when Ore_Available < Ore_Needed;

         -- we can produce New_Fuel, so record it and report
         Ore_Available := Ore_Available - Ore_Needed;
         Ore_Used := Ore_Used + Ore_Needed;
         Ore_Needed := 0;
         Fuel_Produced := Fuel_Produced + New_Fuel;
         -- uncomment if you'd like a report (it's interesting)
         -- Put_Line(Fuel_Produced'Image & " fuel produced");
         -- Put_Line(Ore_Available'Image & " ore left");

      end loop;

      -- uncomment if you'd like a report (it's interesting)
      -- Put_Line("ran out of ore");

      -- SUBSECTION
      -- report answer

      -- uncomment the next line if you want to see how much of each mineral
      -- is left (it's interesting)
      -- for I of Mineral_Available loop Put(I'Image & ","); end loop; New_Line;

      Put_Line(
               "...we produce" &  Fuel_Produced'Image
               & " units of fuel!"
              );
      Put_Line("we used" & Ore_Used'Image & " units of ore");

   end;

end Main;
