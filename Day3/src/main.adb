-- Advent of Code 2019, Day 3
--
-- John Perry
--
-- Day 3: Crossed Wires
--
-- part 1: find where two wires cross... closest to origin
--
-- part 2: find where two wires cross... with shortest distance traveled
--

pragma Ada_2020;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;

procedure Main is

   -- SECTION
   -- input-related

   F: File_Type; -- input file

   Testing: constant Boolean := False;

   Filename: constant String
         := (
             if Testing then "/Users/user/common/Ada/AoC2019/Day3/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day3/input.txt"
            );

   -- SECTION
   -- our geometry

   type Location is record
      X, Y: Integer := 0;
   end record;

   -- functions to move from a location, returning a new location

   function Right_From(Pos: Location; Distance: Natural) return Location is
   ( ( Pos.X + Distance, Pos.Y ) );

   function Left_From(Pos: Location; Distance: Natural) return Location is
   ( ( Pos.X - Distance, Pos.Y ) );

   function Up_From(Pos: Location; Distance: Natural) return Location is
   ( ( Pos.X, Pos.Y + Distance ) );

   function Down_From(Pos: Location; Distance: Natural) return Location is
   ( ( Pos.X, Pos.Y - Distance ) );

   -- the distance between two points

   function Manhattan_Distance(A, B: Location) return Natural is
   ( abs(A.X - B.X) + abs(A.Y - B.Y) );

   -- setup for ordered sets and maps

   function "<"(A, B: Location) return Boolean is
   ( A.X < B.X or else ( A.X = B.X and then A.Y < B.Y ) );

   function "="(A, B: Location) return Boolean is
   ( A.X = B.X and then A.Y = B.Y );

   package Location_Sets is new Ada.Containers.Ordered_Sets
   (
    Element_Type => Location
   );

   package Location_Maps_To_Delay is new Ada.Containers.Ordered_Maps
   (
    Key_Type => Location,
    Element_Type => Natural
   );

   -- SECTION
   -- navigating according to instructions

   type Directions is ( Up, Down, Left, Right );

   Deltas: array(Directions) of Location
         := ( Up => ( 0, 1), Down => (0, -1), Left => (-1, 0), Right => (1, 0) );

   function "+"(P, Q: Location) return Location is
   ( ( P.X + Q.X , P.Y + Q.Y ) );

   function "*"(Scale: Natural; P: Location) return Location is
   ( ( P.X * Scale , P.Y * Scale ) );

   procedure Trace_Locations(
         Diagram: String; Locations: out Location_Sets.Set;
         Delays: out Location_Maps_To_Delay.Map
   )
   -- reads Diagram for directions, writes locations visited to Locations,
   -- and adds the time each location is first visited to Delays

   is

      -- iterating through Diagram
      I: Natural := Diagram'First;
      Current_Location: Location := ( 0 , 0 );
      Current_Delay: Natural := 0;

      -- each instruction
      Direction: Character;
      Distance: Natural;

   begin

      -- start at origin
      -- (we later remove this, but for the sake of completeness...)
      Locations.Insert( Current_Location );

      -- read each instruction, add the locations traveled to Locations,
      -- and the delays required for each location to Delays
      while I < Diagram'Last loop

         Direction := Diagram(I);
         I := I + 1;
         Get(Diagram(I .. Diagram'Last), Distance, I);
         I := I + 2;

         To_Next_Point:
         for J in 1 .. Distance loop

            declare
               New_Location: Location
                     := (

                         case Direction is

                            when 'U' => Current_Location + J * Deltas(Up),
                            when 'D' => Current_Location + J * Deltas(Down),
                            when 'L' => Current_Location + J * Deltas(Left),
                            when 'R' => Current_Location + J * Deltas(Right),
                            when others =>
                               raise Data_Error with "invalid direction"

                        );

            begin

               Current_Delay := Current_Delay + 1;
               if not Locations.Contains(New_Location) then
                  Locations.Insert(New_Location);
                  Delays.Insert( New_Location, Current_Delay );
               end if;

            end;

         end loop To_Next_Point;

         case Direction is
         when 'U' => Current_Location := Up_From(Current_Location, Distance);
         when 'D' => Current_Location := Down_From(Current_Location, Distance);
         when 'L' => Current_Location := Left_From(Current_Location, Distance);
         when 'R' => Current_Location := Right_From(Current_Location, Distance);
         when others => raise Data_Error with "invalid direction";
         end case;

      end loop;

   end Trace_Locations;

begin

   -- we do both parts at once

   Open(F, In_File, Filename);

   declare

      -- data from the file
      First_Diagram: String := Get_Line(F);
      Second_Diagram: String := Get_Line(F);

      -- data in a structure we understand
      First_Locations, Second_Locations, Common_Locations: Location_Sets.Set;
      First_Delays, Second_Delays: Location_Maps_To_Delay.Map;

      -- results
      Closest_Intersection, Closest_Delay: Location;
      Closest_Distance, Shortest_Delay: Natural;

   begin

      -- read the data and make sense of it

      Trace_Locations(First_Diagram, First_Locations, First_Delays);
      Trace_Locations(Second_Diagram, Second_Locations, Second_Delays);

      -- part 1: find closest location of an intersection

      Common_Locations := First_Locations.Intersection(Second_Locations);
      -- we don't need the origin
      Common_Locations.Delete( ( 0 , 0 ) );

      -- set up results from first element
      Closest_Intersection := Common_Locations.First_Element;
      Closest_Delay := Common_Locations.First_Element;
      Closest_Distance
            := Manhattan_Distance( ( 0, 0 ), Closest_Intersection );
      Shortest_Delay
            := First_Delays(Closest_Delay) + Second_Delays(Closest_Delay);

      -- check the remaining elements
      for P of Common_Locations loop

         declare

            This_Distance: Natural := Manhattan_Distance( ( 0, 0 ), P );
            This_Delay: Natural := First_Delays(P) + Second_Delays(P);

         begin

            -- part 1
            if This_Distance < Closest_Distance then
               Closest_Distance := This_Distance;
               Closest_Intersection := P;
            end if;

            -- part 2
            if This_Delay < Shortest_Delay then
               Shortest_Delay := This_Delay;
               Closest_Delay := P;
            end if;

         end;

      end loop;

      -- report
      Put("closest intersection is "); Put(Closest_Distance, 0); New_Line;
      Put("shortest delay is "); Put(Shortest_Delay, 0); New_Line;

   end;

   Close(F);

end Main;
