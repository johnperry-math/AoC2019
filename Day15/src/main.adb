-- Advent of Code 2019, Day 15
--
-- John Perry
--
-- Intcode again
--
-- part 1: determine the shortest route from the droid to the oxygen system
--
-- part 2: determine how long it takes for oxygen to spread through the section
--

pragma Ada_2020;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Containers.Ordered_Maps;

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

with Intcode;

procedure Main is

   -- SECTION
   -- input-related

   F: File_Type; -- input file

   Testing: constant Boolean := False;

   Filename: constant String
         := (
             if Testing then "/Users/user/common/Ada/AoC2019/Day15/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day15/input.txt"
            );

   -- SECTION
   -- debugging

   Verbose: Boolean := False;

   -- SECTION
   -- Intcode computer

   Output_Length: constant := 1;

   package Droid is new Intcode
   (
    Max_Location => 10000,
    Input_From_Command_Line => False,
    Input_Size => 1,
    Output_To_Command_Line => False,
    Output_Size => Output_Length
   );

   -- SECTION
   -- movement commands

   type Direction is ( North, South, West, East );
   -- directions the droid can move

   Translate_Direction: constant array ( Direction ) of Long_Long_Integer
   -- translation of directions from words to Intcode
     := ( North => 1, South => 2, West => 3, East => 4 );

   -- SECTION
   -- status codes

   type Status_Codes is ( Wall, Moved, Oxygen );
   -- what we might find in a location; "moved" is similar to "empty"
   -- except that we've actually moved there

   function To_Status(Status: Long_Long_Integer) return Status_Codes is
      -- interpreting droid's output
     (
      case Status is
         when 0 => Wall,
         when 1 => Moved,
         when 2 => Oxygen,
         when others => raise Data_Error with "invalid status" & Status'Image
     );

   -- SECTION
   -- internal map

   type Location is record
      -- a position on the map
      X, Y: Integer;
   end record;

   -- we don't from the outset know how large the map is,
   -- so we'll use an Ordered_Maps.Map to store status for locations we know

   function "<"(First, Second: Location) return Boolean is
      -- strictly lexicographic ordering
     (
      First.X < Second.X or else
        ( First.X = Second.X and then First.Y < Second.Y )
     );

   package Section_Maps is new Ada.Containers.Ordered_Maps
   -- structure for remembering locations
        (
         Key_Type => Location,
         Element_Type => Status_Codes
        );

   Section_Map: Section_Maps.Map;
   -- our record of the visited locations

   -- SECTION
   -- data and subprograms for part 1

   Current_Position: Location := ( 0, 0 );
   -- where the droid is currently located

   O2_Location: Location;
   -- location of the O2

   Distance_To_O2, Distance_Traveled: Natural := 0;
   -- Distance_To_O2 is the distance from the origin to O2, once we find it
   -- Distance_Traveled is the distance the droid has traveled from the origin

   Found_O2: Boolean := False;

   procedure Explore;
   -- forward declaration

   procedure Try(Where: Direction) is

      -- try the given direction, update distance
      -- if it's a valid move, and if we find oxygen, update the O2 variables,
      -- then explore further, then backtrack
      -- if it's a wall, backtrack

      New_Status: Status_Codes;
      -- droid's report

   begin

      -- update distance traveled, ask the droid to move in given direction,
      -- then determine status and record it

      Distance_Traveled := Distance_Traveled + 1;
      Droid.Supply_Input( ( others => Translate_Direction(Where) ) );
      Droid.Run_Program;
      New_Status := To_Status(Droid.Report_Output(1));
      Section_Map.Insert(Current_Position, New_Status);

      -- for debugging

      if Verbose then
         Put_Line(
                  Current_Position.X'Image & "," & Current_Position.Y'Image
                  & ":" & New_Status'Image
                 );
      end if;

      -- have we found oxygen?
      -- if so, record it

      Found_O2 := New_Status = Oxygen;
      if Found_O2 then
         O2_Location := Current_Position;
         Distance_To_O2 := Distance_Traveled;
      end if;

      -- if we can continue exploring, do so
      -- (we need to do this even if we find oxygen on account of part 2)

      if New_Status = Moved or New_Status = Oxygen then

         Explore;

         -- backtrack a move: adjust distance and move droid back

         Distance_Traveled := Distance_Traveled - 1;
         Droid.Supply_Input( ( others => Translate_Direction(
                               case Where is
                                  when North => South,
                                  when South => North,
                                  when West => East,
                                  when East => West
                              ) ) );
         Droid.Run_Program;

         -- debugging

         if Verbose then
            Put_Line("told droid to move away from " & Where'Image & " and received" & Droid.Report_Output(1)'Image);
            Put_Line("Distance_Traveled:" & Distance_Traveled'Image);
         end if;

      else

         -- backtrack a wall: droid hasn't moved, so we only adjust distance

         Distance_Traveled := Distance_Traveled - 1;

      end if;

   end Try;

   -- for moving around

   subtype Delta_Range is Integer range -1 .. 1;
   -- how far to move in each dimension from current location

   type Delta_Value is record
      -- a particular change of direction
      X, Y: Delta_Range;
   end record;

   Deltas: array ( Direction ) of Delta_Value
   -- how far to move for each direction
     := (
         North => ( 0, -1 ), South => ( 0, 1 ),
         East => ( 1, 0 ), West => ( -1, 0 )
        );

   procedure Explore is
      -- Try all unexplored positions adjacent to Current_Position
      -- depth-first search

      Old_Position: Location := Current_Position;
      -- modify the old position to try new positions

   begin

      -- explore in each direction unless we've already visited the position
      for Dir in Direction loop

         declare D renames Deltas(Dir);
         begin

            Current_Position := ( Old_Position.X + D.X, Old_Position.Y + D.Y );
            if not Section_Map.Contains(Current_Position) then
               Try(Dir);
            end if;

         end;

      end loop;

      Current_Position := Old_Position;

   end Explore;

   procedure Print_Map is
      -- useful for debugging, and also cute to look at

      Min_Y, Max_Y, Min_X, Max_X: Integer := 0;
      -- largest and smallest locations on the map

      C: Section_Maps.Cursor := Section_Map.First;
      -- used to loop through all positions

      Status_To_Character: constant array( Status_Codes ) of Character
        := ( Moved => '.', Wall => '#', Oxygen => 'O' );
      -- map the status to a character we can print

   begin

      -- first identify min's and max's

      while Section_Maps."/="(C, Section_Maps.No_Element) loop

         declare Location renames Section_Maps.Key(C);
         begin

            if Location.X < Min_X then Min_X := Location.X;
            elsif Location.X > Max_X then Max_X := Location.X;
            end if;

            if Location.Y < Min_Y then Min_Y := Location.Y;
            elsif Location.Y > Max_Y then Max_Y := Location.Y;
            end if;

         end;

         Section_Maps.Next(C);

      end loop;

      -- for debugging

      if Verbose then
         Put_Line(
                  Min_X'Image & "," & Max_X'Image & ";"
                  & Min_Y'Image & "," & Max_Y'Image
                 );
      end if;

      -- prepare to draw, then draw
      declare

         Bitmap: array( Min_X .. Max_X , Min_Y .. Max_Y ) of Character
           := ( others => ( others => ' ' ) );
         C: Section_Maps.Cursor := Section_Map.First;

      begin

         -- draw to bitmap

         while Section_Maps."/="(C, Section_Maps.No_Element) loop
            Bitmap( Section_Maps.Key(C).X, Section_Maps.Key(C).Y )
              := Status_To_Character( Section_Maps.Element(C) );
            Section_Maps.Next(C);
         end loop;

         -- show bitmap

         for I in Min_Y .. Max_Y loop
            for J in Min_X .. Max_X loop
               Put(Bitmap(J,I));
            end loop;
            New_Line;
         end loop;

      end;

   end Print_Map;

   -- SECTION
   -- data and subprograms for part 2

   type Location_And_Time is record
      -- time that oxygen reaches a given location
      Loc: Location;
      Time: Natural;
   end record;

   package Location_Queue_Interface
   is new Ada.Containers.Synchronized_Queue_Interfaces
   -- interface to a queue to keep track of locations we have yet to visit
     (
      Element_Type => Location_And_Time
     );

   package Location_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
   -- queue structure for locations we have yet to visit
     (
      Queue_Interfaces => Location_Queue_Interface
     );

   Oxygen_Moving: Location_Queues.Queue;
   -- locations we have yet to visit

   Time_Steps: Natural := 0;
   -- how many time steps since the oxygen unit has repaired

   procedure Diffuse is
      -- let oxygen diffuse through chamber and determine how long it takes
      -- to fill up
      -- breadth-first search

   begin

      -- continue while there are positions to diffuse to
      while Integer(Oxygen_Moving.Current_Use) /= 0 loop

         declare

            LT: Location_And_Time;
            -- the location we now consider, and the time it arrives

            New_Loc: Location;
            -- the next location

         begin

            -- get the location and process if oxygen hasn't yet arrived

            Oxygen_Moving.Dequeue(LT);

            if Section_Map( LT.Loc ) /= Oxygen then

               -- debugging

               if Verbose then
                  Put_Line(
                           "at time" & LT.Time'Image & " location"
                           & Lt.Loc.X'Image & "," & Lt.Loc.Y'Image
                          );
               end if;

               -- oxygen comes in; enqueue adjacent locations if applicable.
               -- along with the next time

               Section_Map( LT.Loc ) := Oxygen;

               for D of Deltas loop

                  New_Loc := ( LT.Loc.X + D.X, LT.Loc.Y + D.Y );

                  if Section_Map.Contains(New_Loc)
                    and then Section_Map(New_Loc) = Moved
                  then
                     Oxygen_Moving.Enqueue( ( Loc => New_Loc, Time => LT.Time + 1 ) );
                     Time_Steps := Natural'Max( Time_Steps, LT.Time + 1 );
                  end if;

               end loop;

            end if;

         end;

      end loop;

   end Diffuse;

begin

   Droid.Read_Program(Filename);

   -- SECTION
   -- part 1: find distance to O2 system

   -- probably a safe assumption
   -- that the current position is not the O2 system
   Section_Map.Insert( ( 0, 0 ), Moved );
   Explore;
   Print_Map;

   Put_Line(
            "found o2 at "
            & O2_Location.X'Image & "," & O2_Location.Y'Image
           );
   Put_Line("distance to o2 system:" & Distance_To_O2'Image);

   -- SECTION
   -- part 2: determine how long it will take oxygen
   -- to spread through the section

   Section_Map(O2_Location) := Moved;
   Oxygen_Moving.Enqueue(
                         ( Loc => ( O2_Location.X, O2_Location.Y ),
                           Time => 0 )
                        );
   Diffuse;
   Put_Line("it takes" & Time_Steps'Image & " steps to fill with o2");

   if Verbose then Print_Map; end if;

end Main;
