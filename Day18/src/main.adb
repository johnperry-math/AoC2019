-- Advent of Code 2019, Day 18
--
-- John Perry
--
-- part 1: find the shortest path through a maze which unlocks all the doors
--
-- part 2: repeat, but for four robots working simultaneously, on a slightly
-- modified maze

pragma Ada_2020;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;
with Ada.Containers.Ordered_Maps;

with Ada.Characters.Handling;

procedure Main is

   -- SECTION
   -- input-related

   F: File_Type; -- input file

   Testing: constant Boolean := False;
   Which_Test: constant := 1;

   Filename: constant String
         := (
             if Testing then (
               if Which_Test = 1
               then "/Users/user/common/Ada/AoC2019/Day18/example.txt"
               elsif Which_Test = 2
               then "/Users/user/common/Ada/AoC2019/Day18/example2.txt"
               elsif Which_Test = 3
               then "/Users/user/common/Ada/AoC2019/Day18/example3.txt"
               else "/Users/user/common/Ada/AoC2019/Day18/example4.txt"
              )
             else "/Users/user/common/Ada/AoC2019/Day18/input.txt"
            );

   -- SECTION
   -- debugging

   Verbose: Boolean := False;

   -- SECTION
   -- data types and subprograms for the vault

   -- SUBSECTION
   -- datatypes for the Vault

   -- dimensions gleaned directly from the files
   Vault_Width: constant
     -- the vault's width, varies according to maze
     := (
         if Testing then (
           if Which_Test = 1 or Which_Test = 2 then 13
           elsif Which_Test = 4 then 15
           else 7
          )
         else 81
        );
   Vault_Height: constant
     -- the vault's height, varies according to maze
     := (
         if Testing then (
           if Which_Test = 1 or Which_Test = 3 or Which_Test = 4 then 7
           else 9
          )
         else 81
        );

   type Vault_Width_Range is mod Vault_Width;
   type Vault_Height_Range is mod Vault_Height;

   -- SUBSUBSECTION
   -- Doors and Keys

   subtype Door_Name is Character range 'A' .. 'Z';
   -- for each upper-case door there is a corresponding lower-case key
   subtype Key_Name is Character range 'a' .. 'z';
   -- for each lower-case key there is a corresponding upper-case door

   function Key_For_Door(D: Door_Name) return Key_Name is
      -- returns the key needed to unlock door D
     ( Ada.Characters.Handling.To_Lower(D) );

   type Key_Array is array ( Key_Name ) of Boolean with Pack;
   -- used to track which keys are needed or held

   procedure Put(Keys: Key_Array) is
      -- convenience subprogram, for debugging
   begin
      for K in Keys'Range loop
         if Keys(K) then Put(K & " "); end if;
      end loop;
   end Put;

   -- SUBSUBSECTION
   -- Positions in the Vault

   type Position is record
      -- a position somewhere in the vault
      X: Vault_Width_Range;
      Y: Vault_Height_Range;
   end record;

   function "="(First, Second: Position) return Boolean is
      -- true iff x, y values are the same
      ( First.X = Second.X and then First.Y = Second.Y );

   function "<"(First, Second: Position) return Boolean is
      -- lexicographic ordering of position
     (
      First.X < Second.X or else ( First.X = Second.X and First.Y < Second.Y )
     );

   procedure Put(P: Position) is
      -- prints P in the format ( X, Y )
   begin
      Put("(" & P.X'Image & "," & P.Y'Image & " )");
   end Put;

   type Directions is ( Up, Dn, Lt, Rt );
   -- useful for easy iteration of directions; see Deltas
   Deltas: array ( Directions ) of Position := ( (0,-1), (0,1), (-1,0), (1,0) );
   -- which way to travel for a given Direction

   function "+"(First, Second: Position) return Position is
      -- vector addition of positions (i.e., componentwise addition)
     ( ( First.X + Second.X, First.Y + Second.Y ) );

   package Position_Sets is new Ada.Containers.Ordered_Sets
     (
      Element_Type => Position
     );

   type Location_Feature is ( Wall, Empty, Entrance, Door, Key );
   -- features appear in any particular location

   type Location ( Feature: Location_Feature := Empty ) is record
      -- information on what is present at a vault location
      -- Wall, Empty, and Entrace have no further information
      -- Door or Key adds information on Which_Door or Which_Key
      case Feature is
         when Wall | Empty | Entrance => null;
         when Door => Which_Door: Door_Name;
         when Key => Which_Key: Key_Name;
      end case;
   end record;

   -- SUBSUBSECTION
   -- reading the vault and initial analysis

   type Vault_Map
     -- the vault as a grid
   is array ( Vault_Width_Range , Vault_Height_Range ) of Location;

   Vault: Vault_Map;
   -- the vault

   Max_Robots: constant := 4;
   -- in part 2 we need 4 robots
   type Robot_Range is mod Max_Robots;
   -- cycle through robots

   type Robot_Positions is array ( Robot_Range ) of Position;
   -- a type to record where each robot is in the map
   Start_Position: Robot_Positions;
   -- each robot's entrance to the vault

   procedure Read_Map is
      -- reads the input file and translates the information into a Vault_Map
      -- stored in the global Vault
      -- in the case of a bad input, raises Data_Error

      I: Vault_Height_Range := 0; -- the row we're working on

   begin

      Open(F, In_File, Filename);

      while not End_Of_File(F) loop

         declare
            S: String := Get_Line(F);
            J: Vault_Width_Range := 0; -- the column we're working on

         begin

            -- process each character and update Vault

            for C of S loop

               Vault(J, I)
                 := (
                     case C is
                        when '.' => Location'(Feature => Empty),
                        when '#' => Location'(Feature => Wall),
                        when '@' => Location'(Feature => Entrance),
                        when Door_Name =>
                          Location'(Feature => Door, Which_Door => C),
                        when Key_Name =>
                          Location'(Feature => Key, Which_Key => C),
                        when others =>
                           raise Data_Error
                             with "invalid vault value " & C'Image
                    );
               if C = '@' then Start_Position := ( others => ( J, I ) ); end if;

               J := J + 1;
               if J = 0 then I := I + 1; end if;

            end loop;

         end;

      end loop;

      Close(F);

   end Read_Map;

   -- SUBSECTION
   -- parts 1 and 2 are much more tractable if we redo the vault as a graph

   type Edge is record
      -- a connection from one position to another
      Start, Stop: Position;
   end record;

   function "<"(First, Second: Edge) return Boolean is
      -- lexicographic ordering of edges
     (
      First.Start < Second.Start or else
        (
         First.Start = Second.Start and then First.Stop < Second.Stop
        )
     );

   function "="(First, Second: Edge) return Boolean is
      -- True iff edges have identical initial and terminal vertices
     (
      First.Start = Second.Start and then First.Stop = Second.Stop
     );

   procedure Put(E: Edge) is
      -- prints E in the format ( X1, Y1 ) -> ( X2, Y2 )
   begin
      Put(E.Start);
      Put(" -> ");
      Put(E.Stop);
   end Put;

   -- we map each edge to its corresponding distance

   package Reachable_Points_Maps is new Ada.Containers.Ordered_Maps
   -- each of these maps is associated with a position,
   -- so we need merely store the terminal position
     (
      Key_Type => Position,
      Element_Type => Natural
     );

   subtype Reachable_Map is Reachable_Points_Maps.Map;
   -- each edge's cost
   subtype Reachable_Cursor is Reachable_Points_Maps.Cursor;
   -- iteration through the edges reachable from a point
   No_RM_Element renames Reachable_Points_Maps.No_Element;
   -- you reach this when you have traversed all points reachable
   -- from this map's Position
   function Element(C: Reachable_Cursor) return Natural
   -- the cost to reach Key(C) from this map's Position
                    renames Reachable_Points_Maps.Element;
   function Key(C: Reachable_Cursor) return Position
   -- a Position reachable from this map's Position
                renames Reachable_Points_Maps.Key;
   procedure Next(C: in out Reachable_Cursor)
                  renames Reachable_Points_Maps.Next;
   -- returns another Position reachable from this map's Position
   function "="(First, Second: Reachable_Cursor) return Boolean
                renames Reachable_Points_Maps."=";
   -- whether First and Second indicate the same position

   package Graph_Reachable_Maps is new Ada.Containers.Ordered_Maps
   -- each position is associated with a map of reachable positions
   -- (see Reachable_Points_Map)
     (
      Key_Type => Position,
      Element_Type => Reachable_Map,
      "=" => Reachable_Points_Maps."="
     );

   subtype Graph_Map is Graph_Reachable_Maps.Map;
   -- a map for each interesting Position to other interesting Positions
   -- reachable from it
   subtype Graph_Cursor is Graph_Reachable_Maps.Cursor;
   -- a cursor to iterate the Graph_Map type
   No_Graph_Element renames Graph_Reachable_Maps.No_Element;
   -- you reach this when you have traversed all points in the graph
   function Element(C: Graph_Cursor) return Reachable_Map
   -- returns a map indicating all interesting Positions reachable from Key(C)
   -- and their corresponding costs
                    renames Graph_Reachable_Maps.Element;
   function Key(C: Graph_Cursor) return Position
   -- returns an interesting Position on the graph
                renames Graph_Reachable_Maps.Key;
   procedure Next(C: in out Graph_Cursor) renames Graph_Reachable_Maps.Next;
   -- returns another interesting Position on the graph
   function "="(First, Second: Graph_Cursor) return Boolean
   -- whether First and Second indicate the same map of interesting Positions
                renames Graph_Reachable_Maps."=";

   Graph: Graph_Map;
   -- the graph version of our map

   -- SUBSUBSECTION
   -- converting the grid to a graph

   function Reachable_From(X: Vault_Width_Range; Y: Vault_Height_Range)
                             return Reachable_Map
   -- uses a breadth-first search to determine which interesting vault positions
   -- are reachable from position (X,Y) without passing through another
   -- interesting vault positions
   --
   -- "interesting" means a door, key, or entrance
   is

      Distance_To: Reachable_Map;
      -- distances from (X,Y) to various positions in the vault
      Visiting: Position_Sets.Set;
      -- which positions we still plan to visit
      Result: Reachable_Map;
      -- interesting positions we have visited
      Curr_Pos: constant Position := ( X, Y );
      -- current position

   begin

      Distance_To.Insert( Curr_Pos, 0 );

      -- insert the first positions from the start
      for Dir in Directions loop

         declare New_Position: Position := Curr_Pos + Deltas(Dir);
         begin
            if Vault(New_Position.X, New_Position.Y).Feature /= Wall then
               Visiting.Insert(New_Position);
               Distance_To.Insert(New_Position, 1);
            end if;
         end;

      end loop;

      -- consider each position we still have to visit;
      -- if it has an interesting item, add it to the result;
      -- otherwise, if it is empty, generate new reachable positions to visit

      -- we track which positions we've already visited, so as not to waste
      -- too much time, with the only exception when traveling to a position
      -- by a second route would be shorter than the first route discovered

      while not Visiting.Is_Empty loop

         declare

            P: Position := Visiting.First_Element;
            D: Positive := Distance_To(P);

         begin

            case Vault( P.X, P.Y ).Feature is

               when Wall => null;

               when Key | Door | Entrance =>
                  if Result.Contains(P) then Result(P) := D;
                  else Result.Insert( P, D );
                  end if;

               when Empty =>

                  for Dir in Directions loop

                     declare Q: Position := P + Deltas(Dir);
                     begin

                        if Vault(Q.X, Q.Y).Feature /= Wall then

                           -- don't travel to the same place twice...
                           -- ...unless you would save time doing so
                           if Distance_To.Contains(Q) then
                              -- overlooked this detail on first implementation
                              -- revised it badly during optimization
                              -- these comparisons are A BIG DEAL
                              -- and cannot be modified (THINK ABOUT IT)
                              if Distance_To(Q) > D + 1 then
                                 Distance_To(Q) := D + 1;
                                 if not Visiting.Contains(Q) then
                                    Visiting.Insert(Q);
                                 end if;
                              end if;
                           else
                              -- new position
                              Visiting.Insert(Q);
                              Distance_To.Insert(Q, D + 1);
                           end if;

                        end if;

                     end;

                  end loop;

            end case;

            Visiting.Delete(P);

         end;

      end loop;

      return Result;

   end Reachable_From;

   procedure Convert_Map_To_Graph is
      -- converts the map to a graph by determining,
      -- for each interesting position, how far it is to every other
      -- interesting position that is not blocked by another

   begin

      for Y in Vault_Height_Range loop
         for X in Vault_Width_Range loop

            if Vault(X,Y).Feature /= Wall and then Vault(X,Y).Feature /= Empty
            then

               -- generate the points for each location, then create an edge
               -- and add to the graph, unless it already exists, in which case
               -- we set to the smaller distance

               Graph.Insert( ( X, Y ), Reachable_From(X, Y) );

            end if;

         end loop;
      end loop;

   end Convert_Map_To_Graph;

   -- SECTION
   -- data types and subprograms used to solve part 1

   -- SUBSECTION
   -- data types and subprograms for tracking paths traveled

   package Path_Vectors is new Ada.Containers.Vectors
     -- package for Vectors of Positions traveled
     (
      Index_Type => Positive,
      Element_Type => Position
     );

   subtype Path_Vector is Path_Vectors.Vector;
   -- a list of Positions traveled

   procedure Put(Path: Path_Vector) is
      -- prints information on the Path: positions traveled, which features
      -- are at each location, the cost of each edge, and the running cost
      -- of the trip
      Prev: Position := Path.First_Element; --Start_Position(Robot);
      Cost, Distance: Natural := 0;
   begin
      Put(Prev);
      for Curr of Path loop
         if Curr /= Prev then
            Put(Curr);
            case Vault(Curr.X, Curr.Y).Feature is
               when Entrance => Put('@');
               when Door => Put(Vault(Curr.X, Curr.Y).Which_Door);
               when Key => Put(Vault(Curr.X, Curr.Y).Which_Key);
               when others => raise Data_Error with "invalid feature";
            end case;
            Cost := Graph(Prev)(Curr);
            Distance := Distance + Cost;
            Put(Cost'Image & "," & Distance'Image &"; ");
            Prev := Curr;
         end if;
      end loop;
   end Put;

   type Path_Array is array ( Robot_Range ) of Path_Vector;
   -- type to remember each robot's paths through the map
   type Robot_Key_Array is array ( Robot_Range ) of Key_Array;
   -- type to record which robot has its keys
   Robot_Goal: Robot_Key_Array;
   -- the keys each robot needs to collect

   type Path_Data is record
      -- information about a robot's potential path through the vault
      Cost: Natural := 0; -- distance traveled on the path
      Paths: Path_Array; -- positions traveled on the path
      Keys_Held: Robot_Key_Array := ( others => ( others => False ) );
      -- keys obtained while traveling on the path
      All_Keys_Held: Key_Array := ( others => False );
      -- all the keys held by a robot
      -- this should be the union of Keys_Held(i) for i in Robot_Range
      Actor: Robot_Range := Robot_Range'First;
   end record;

   procedure Put(Data: Path_Data) is
      -- writes Data in the form
      --    Cost:
      --    Path1
      --    Path2
      --    ..
      --    PathLast
      --
      --    all the Keys held
      --    acting robot
      --    two new lines
   begin
      Put_Line(Data.Cost'Image & ":");
      for P of Data.Paths loop
         Put(P); New_Line;
      end loop;
      New_Line;
      for K in Key_Name loop
         if Data.All_Keys_Held(K) then Put(K); end if;
      end loop;
      Put_Line(Data.Actor'Image & " active");
      New_Line(2);
   end Put;

   package Paths_To_Consider_Interfaces
   is new Ada.Containers.Synchronized_Queue_Interfaces
     -- an interface for a queue of Path_Data
     (
      Element_Type => Path_Data
     );

   function Lower_Cost(Data: Path_Data) return Natural is ( Data.Cost );
   -- used for the priority queue below, returns Data's Cost field

   package Path_To_Consider_Queues
   is new Ada.Containers.Unbounded_Priority_Queues
   -- a package for a queue of paths we still need to consider
     (
      Queue_Interfaces => Paths_To_Consider_Interfaces,
      Queue_Priority => Natural,
      Get_Priority => Lower_Cost,
      Before => "<"
     );

   Paths_To_Consider: Path_To_Consider_Queues.Queue;
   -- a priority queue where the higher priority is the lower cost of travel

   procedure Put_Line(P: Path_Vector) is
      -- a convenience function for printing a path; used for debugging
   begin
      for Pos of P loop
         Put("(" & Pos.X'Image & "," & Pos.Y'Image & " )");
      end loop;
      New_Line;
   end Put_Line;

   -- SUBSECTION
   -- other data types and subprograms needed for searching

   Search_Error: exception; -- did not find a path, should not be raised

   type State_Cache is record
      -- tracks current position along with the keys obtained thus far
      Where: Robot_Positions;
      -- each robot's position in the vault
      Keys: Key_Array := ( others => False );
      --  Keys: Robot_Key_Array := ( others => ( others => False ) );
      Distance: Natural := 0;
      -- the cost it has taken to arrive at this state
   end record;

   function "="(First, Second: State_Cache) return Boolean is
      -- True iff same position and same keys held
     (
      First.Where = Second.Where and First.Keys = Second.Keys
     );

   function Keys_Lex_Smaller(First, Second: Key_Array) return Boolean is
      -- lexicographic comparison of keys, where First is smaller than Second
      -- iff the first different key is held by Second ("True") but not by
      -- First ("False")

      Result: Boolean := False; -- default is to assume First is not smaller
      Found_Difference: Boolean := False;
      I: Character := Key_Array'First;

   begin

      -- we compare the keys until either we run out or we find a difference
      -- in what is held
      loop
         if First(I) /= Second(I) then Found_Difference := True; exit; end if;
         if I = Key_Array'Last then exit; end if;
         I := Character'Succ(I);
      end loop;

      if Found_Difference then Result := First(I) = False; end if;

      return Result;

   end Keys_Lex_Smaller;

   function "<"(First, Second: State_Cache) return Boolean is
      -- order by position, then by keys held
   begin
      for R in Robot_Range loop
         if First.Where(R) < Second.Where(R) then return True;
         elsif Second.Where(R) < First.Where(R) then return False;
         end if;
      end loop;
      return Keys_Lex_Smaller( First.Keys, Second.Keys );
   end "<";

   package Known_States_Package is new Ada.Containers.Ordered_Maps
     -- map of state to its cost
     (
      Key_Type => State_Cache,
      Element_Type => Positive
     );

   Known_States: Known_States_Package.Map;
   -- positions we've visited, along with the keys held, mapped to the cost
   -- of traveling to that position

   -- SUBSECTION
   -- the subprograms that perform the search

   package Keys_To_Robot_Maps is new Ada.Containers.Ordered_Maps
     -- package for mapping a key to the robot who needs to collect it
        (
         Key_Type => Key_Name,
         Element_Type => Robot_Range
        );

   Keys_To_Robot: Keys_To_Robot_Maps.Map;
   -- map of each key to the robot who needs to collect it

   procedure Determine_Keys_For(Robot: Robot_Range) is
      -- determines the keys that Robot has to collect

      Visited: Position_Sets.Set;
      Visiting: Position_Sets.Set;
      Keys renames Robot_Goal(Robot);

   begin

      -- start with no keys and add them as we find them
      Keys := ( others => False );
      Visiting.Insert(Start_Position(Robot));

      -- breadth-first search for all keys
      while not Visiting.Is_Empty loop

         declare
            P: Position := Visiting.First_Element;
            C: Reachable_Cursor := Graph(P).First;
         begin

            Visiting.Delete_First;
            while C /= No_RM_Element loop

               -- we have a key!
               if Vault(Key(C).X, Key(C).Y).Feature = Key then
                  declare Found_Key: Key_Name
                       := Vault(Key(C).X, Key(C).Y).Which_Key;
                  begin
                     Keys(Found_Key) := True;
                     if not Keys_To_Robot.Contains(Found_Key) then
                        Keys_To_Robot.Insert(Found_Key, Robot);
                     end if;
                  end;
               end if;

               -- if we haven't visited this position, and
               -- if we aren't visiting this position already,
               -- queue the position
               if ( not Visiting.Contains(Key(C)) ) and
                 ( not Visited.Contains(Key(C)) )
               then
                  Visiting.Insert(Key(C));
               end if;

               Next(C);

            end loop;

            Visited.Insert(P);

         end;

      end loop;

      Put("keys for" & Robot'Image & ": ");
      for K in Key_Name loop
         if Keys(K) then Put(K); Put(' '); end if;
      end loop;
      New_Line;

   end Determine_Keys_For;

   procedure Setup_Search is
      -- sets up the search for each robot by enqueuiing a new path that
      -- starts from its starting position

      Paths: Path_Array;

   begin

      for R in Robot_Range loop
         Paths(R).Append(Start_Position(R));
      end loop;

      for R in Robot_Range loop
         Paths_To_Consider.Enqueue( (
                                    Cost => 0, Paths => Paths,
                                    Actor => R, others => <>
                                   ) );
      end loop;

   end Setup_Search;

   function Good_To_Add(S: State_Cache; Length: Positive) return Boolean is
      -- returns True iff we've not traveled to S.Where or if Length is smaller
      -- than the known cost of traveling to S.Where
     (
      not ( Known_States.Contains(S) and then Known_States(S) <= Length )
     );

   procedure Add_If_Good_Extension(
                         Data: Path_Data; E: Edge; D: Positive;
                         Robot: Robot_Range
                        )
   is
      -- adds E's Stop to Data's Path so long as Good_To_Add signs off on it
      -- D should be the cost of traveling from Robot's Start_Position
      -- to E's Start

      S: State_Cache := (
                         (
                          for R in Robot_Range =>
                            (
                             if R = Robot then E.Stop
                             else Data.Paths(R).Last_Element
                            )
                         ),
                         Data.All_Keys_Held,
                         D + Data.Cost
                         ); -- state of this position
      Length: Positive := D + Data.Cost;
      -- how long it takes to travel to E's Stop
      Loc renames Vault(E.Stop.X, E.Stop.Y); -- where we are in the vault

   begin

      -- make sure we haven't already covered this state, or if we have,
      -- that the previously computed value was higher
      if Good_To_Add(S, Length) then

         -- don't add enqueue a door that we have to unlock ourselves
         if Loc.Feature = Door and then
           Robot_Goal(Robot)(Key_For_Door(Loc.Which_Door)) and then
           not Data.Keys_Held(Robot)(Key_For_Door(Loc.Which_Door))
         then
            return;
         end if;

         -- update Known_States, either if S is unknown or
         -- if Length by this path is smaller than the previously known
         if Known_States.Contains(S) then
            Known_States(S) := Length;
         else
            Known_States.Insert(S, Length);
         end if;

         -- enqueue it
         declare
            New_Path: Path_Vector := Path_Vectors.Copy(Data.Paths(Robot));
            New_Data: Path_Data
              := (
                  Length,
                  ( (
                   for R in Robot_Range =>
                      ( if R = Robot then New_Path else Data.Paths(R) )
                  ) ),
                  Data.Keys_Held,
                  Data.All_Keys_Held,
                  Robot
                 );
         begin
            New_Data.Paths(Robot).Append(E.Stop);
            --  Put(New_Data);
            Paths_To_Consider.Enqueue( New_Data );
         end;

      end if;

   end Add_If_Good_Extension;

   function Valid_Transfer(Data: Path_Data; R: Robot_Range) return Boolean is
      -- returns True iff we can transfer the actor on path Data to R; that is,
      -- if the next Position on Data.Paths(R) is either not a Door, or
      -- we have the key for that Door already

      Last renames Data.Paths(R).Last_Element; -- most recent Position of R
      Loc renames Vault(Last.X, Last.Y); -- ... as a Location
      Result: Boolean := False; -- whether we can transfer it

   begin

      if Loc.Feature /= Door or else
        Data.All_Keys_Held(Key_For_Door(Loc.Which_Door))
      then
         Result := True;
      end if;

      return Result;

   end Valid_Transfer;

   function Find_Route return Path_Data is
      -- find a route that minimizes the cost to collect all keys
      -- currently works only for one robot

      Iterations: Natural := 0; -- for debugging
      Active_Robot: Robot_Range := Robot_Range'First;
      -- the robot currently moving

   begin

      -- ...consider all paths until we've found one that has all keys
      while ( Natural(Paths_To_Consider.Current_Use) /= 0 ) loop

         -- some debugging
         Iterations := Iterations + 1;
         if Iterations mod 100 = 0 then
            Put_Line(Iterations'Image & " iterations");
            Put_Line( Paths_To_Consider.Current_Use'Image & " paths");
         end if;

         declare
            Data: Path_Data; -- the path we consider
            Last: Position; -- the last position on the active Path
            C: Reachable_Cursor;
            -- iteration through Positions reachable from Last
            New_Key: Key_Name; -- if we find a key
         begin

            Paths_To_Consider.Dequeue(Data);
            Active_Robot := Data.Actor;
            if Verbose then
               Put_Line(Active_Robot'Image & " considering"); Put(Data);
               New_Line;
            end if;

            Last := Data.Paths(Active_Robot).Last_Element;

            -- if this position has a key, add it to the record of keys held;
            -- if this completes the collection, we are finished!
            if Vault(Last.X, Last.Y).Feature = Key and then
              not Data.All_Keys_Held(Vault(Last.X, Last.Y).Which_Key)
            then

               -- record that we have the key
               New_Key := Vault(Last.X, Last.Y).Which_Key;
               Data.Keys_Held(Active_Robot)(New_Key) := True;
               Data.All_Keys_Held(New_Key) := True;
               Verbose := False;
               if Verbose then
                  Put("found keys: ");
                  for K in Key_Name loop
                     if Data.All_Keys_Held(K) then Put(K); end if;
                  end loop;
                  New_Line;
               end if;
               Verbose := False;

               -- if we've found all of Active_Robot's keys, retire Active_Robot
               -- in this path
               if Robot_Goal(Active_Robot) = Data.Keys_Held(Active_Robot) then
                  -- if we've found all of every robot's keys, we're finished
                  if (
                      for all Robot in Robot_Range =>
                        Robot_Goal(Robot) = Data.Keys_Held(Robot)
                     )
                  then
                     return Data;
                  end if;
               end if;

               -- add to the list of states, to prevent consideration
               -- of a longer path to take these keys
               declare S: State_Cache
                    := ( (
                         for R2 in Robot_Range =>
                           (
                            if R2 = Active_Robot then Last
                            else Data.Paths(R2).Last_Element
                           )
                        ),
                         Data.All_Keys_Held,
                         Data.Cost
                        );
               begin
                  if not Known_States.Contains(S) then
                     Known_States.Insert( S, Data.Cost );
                  elsif Known_States(S) > Data.Cost then
                     Known_States(S) := Data.Cost;
                  end if;
               end;

            end if;

            -- continue path's search unless we've hit a door
            -- and don't have the key

            -- first check if this robot has found all its keys;
            -- if so, move on
            if Data.Keys_Held(Active_Robot) = Robot_Goal(Active_Robot) then

               declare R: Robot_Range := Active_Robot + 1;
               begin
                  while Data.Keys_Held(R) = Robot_Goal(R) loop
                     R := R + 1;
                  end loop;
                  Data.Actor := R;
                  Paths_To_Consider.Enqueue(Data);
               end;

               -- check if we've hit a door and don't have the key
            elsif Vault(Last.X, Last.Y).Feature /= Door or else
              Data.All_Keys_Held(Key_For_Door(Vault( Last.X, Last.Y ).Which_Door))
            then

               -- enqueue next positions
               C := Graph(Last).First;
               while C /= No_RM_Element loop
                  Add_If_Good_Extension( Data, ( Last, Key(C) ), Element(C), Active_Robot );
                  Next(C);
               end loop;

            else

               -- reactivate the robot who can unlock this door
               declare
                  R: Robot_Range renames
                    Keys_To_Robot(Key_For_Door(Vault(Last.X,Last.Y).Which_Door));
               begin
                  if R /= Active_Robot and then Valid_Transfer(Data, R) then
                     Data.Actor := R;
                     Paths_To_Consider.Enqueue(Data);
                  end if;
               end;

            end if;

         end;

      end loop;

      -- this should never happen on a well-formed maze & a well-written program
      -- ;-)
      raise Search_Error with "ran out of paths to search!";

   end Find_Route;

   -- SECTION
   -- alternate solution for part 2, on account of something found online

   package State_Cache_Sets is new Ada.Containers.Ordered_Sets
   -- package for sets of State_Cache
     (
      Element_Type => State_Cache
     );

   package Alternate_To_Consider_Interfaces
   is new Ada.Containers.Synchronized_Queue_Interfaces
   -- an interface for a queue of Path_Data
     (
      Element_Type => State_Cache
     );

   function Lower_Cost(Path: State_Cache) return Natural is ( Path.Distance );
   -- used for the priority queue below, returns Data's Cost field

   package Alternate_To_Consider_Queues
   is new Ada.Containers.Unbounded_Priority_Queues
   -- package for a queue of states
     (
      Queue_Interfaces => Alternate_To_Consider_Interfaces,
      Queue_Priority => Natural,
      Get_Priority => Lower_Cost,
      Before => "<"
     );

   Alternate_To_Consider: Alternate_To_Consider_Queues.Queue;
   -- queue of states to consider

   type Reachable_Result is record
      -- a record of a key that is reachable from a Position
      Key: Key_Name;
      Where: Position;
      Distance: Natural;
   end record;

   package Reachable_Result_Vectors is new Ada.Containers.Vectors
   -- a package of vectors of Reachable_Result
     (
      Index_Type => Positive,
      Element_Type => Reachable_Result
     );

   type Reachable_Edge is record
      -- a Position reachable from another
      Where: Position;
      Distance: Natural;
   end record;

   function Lower_Cost(Thing: Reachable_Edge) return Natural is
      -- returns Thing's Distance
     ( Thing.Distance );

   package Reachable_Interfaces
   is new Ada.Containers.Synchronized_Queue_Interfaces
   -- a package for an interface for a queue of Reachable_Edge
     (
      Element_Type => Reachable_Edge
     );

   package Reachable_Queues is new Ada.Containers.Unbounded_Priority_Queues
   -- package for a queue of Reachable_Edge
     (
      Queue_Interfaces => Reachable_Interfaces,
      Queue_Priority => Natural,
      Get_Priority => Lower_Cost,
      Before => "<"
     );

   function Reachable_Keys( Loc: Position; Keys: Key_Array )
                           return Reachable_Result_Vectors.Vector
   -- returns a Vector of Positions that are reachable from Loc and
   -- that feature a Key
   is

      My_Queue: Reachable_Queues.Queue; -- positions yet to consider
      Visited: Position_Sets.Set; -- positions we have visited
      E: Reachable_Edge; -- a Position we can reach from Loc, along with cost
      Result: Reachable_Result_Vectors.Vector; -- eventual result of Positions

   begin

      -- start from self
      My_Queue.Enqueue( ( Loc, 0 ) );

      while Natural( My_Queue.Current_Use ) > 0 loop

         -- take the next position in line
         My_Queue.Dequeue(E);

         -- if this is a key, add it to our list of reachable keys
         if Vault( E.Where.X, E.Where.Y ).Feature = Key and then
           not Keys( Vault( E.Where.X, E.Where.Y ).Which_Key )
         then

            Result.Append( (
                           Vault( E.Where.X, E.Where.Y ).Which_Key,
                           E.Where, E.Distance
                          ) );

         else -- otherwise, queue up the positions we can reach from here

            declare
               Reachable_From_E: Reachable_Map renames Graph(E.Where);
               C: Reachable_Cursor := Reachable_From_E.First;
            begin

               while C /= No_RM_Element loop

                  declare
                     P renames Key(C); -- C's Position
                     L renames Element(C); -- C's cost
                  begin

                     -- add P to consideration only if we haven't already
                     if not Visited.Contains( P ) then

                        Visited.Insert( P );

                        -- if it's neither a door nor a wall, or
                        -- if it's a door and we have a key, enqueue it
                        if Vault( P.X, P.Y ).Feature /= Door or else
                           Keys(Key_For_Door( Vault( P.X, P.Y ).Which_Door) )
                        then
                           My_Queue.Enqueue( ( P, E.Distance + L ) );
                        end if;

                     end if;

                  end;

                  Next(C);

               end loop;

            end;

         end if;

      end loop;

      return Result;

   end Reachable_Keys;

   function Find_Alternate return Natural is
      -- find a route for four robots

      Visited: State_Cache_Sets.Set; -- positions we've already visited
      Current_State: State_Cache -- where the robots are at present
        := ( Where => Start_Position, others => <> );
      All_Keys: Key_Array := ( others => False ); -- list of all keys
      Result: Natural := 0; -- the length

   begin

      -- set up goal: collect each robot's keys
      for R in Robot_Range loop
         for K in Key_Name loop
            All_Keys(K) := @ or Robot_Goal(R)(K);
         end loop;
      end loop;

      Alternate_To_Consider.Enqueue(Current_State);

      while Natural(Alternate_To_Consider.Current_Use) /= 0 loop

         -- get the next State to consider, check if it's a winning state
         Alternate_To_Consider.Dequeue(Current_State);
         if Current_State.Keys = All_Keys then
            return Current_State.Distance;
         end if;

         -- handle it if we hadn't already visited
         if not Visited.Contains(Current_State) then

            Visited.Insert(Current_State);

            -- iterate each robot to each of its next reachable keys
            for R in Robot_Range loop

               declare

                  Loc renames Current_State.Where(R); -- where R is now
                  Cx renames Loc.X;
                  Cy renames Loc.Y;
                  Reachables: Reachable_Result_Vectors.Vector
                    := Reachable_Keys(Loc, Current_State.Keys);
                  -- Keys reachable from Current_State
               begin

                  for Item of Reachables loop
                     declare
                        New_State: State_Cache := Current_State;
                        Distance renames Item.Distance;
                        Where renames Item.Where;
                        New_X renames Where.X;
                        New_Y renames Where.Y;
                        Key renames Item.Key;
                     begin
                        New_State.Distance := @ + Distance;
                        New_State.Where(R) := ( New_X, New_Y );
                        New_State.Keys(Key) := True;
                        Alternate_To_Consider.Enqueue(New_State);
                     end;
                  end loop;

               end;

            end loop;

         end if;

      end loop;
      return Result;
   end Find_Alternate;

begin

   -- read maze and report on it
   Read_Map;
   Put("entrances at");
   for R in Robot_Range loop Put(Start_Position(R)); end loop;
   New_Line;

   -- convert to graph and report the edges
   Convert_Map_To_Graph;
   if Verbose then
   declare
         C: Graph_Cursor := Graph.First;
      begin
         while C /= No_Graph_Element loop
            declare
               M renames Element(C);
               C2: Reachable_Cursor := M.First;
            begin
               while C2 /= No_RM_Element
               loop
                  Put(Key(C)); Put(" -> ");
                  Put(Key(C2)); Put(": ");
                  Put(Element(C2)); New_Line;
                  Next(C2);
               end loop;
            end;
            Next(C);
         end loop;
      end;
   end if;

   -- PART 1

   -- find the shortest route and report on it
   Setup_Search;
   Determine_Keys_For(Robot_Range'First);
   declare Shortest_Route: Path_Data := Find_Route;
   begin
      Put_Line("shortest route has length" & Shortest_Route.Cost'Image);
      Put(Shortest_Route.Paths(Robot_Range'First));
      New_Line(2);
   end;

   -- PART 2

   -- change the maze, so that it now has four quadrants; solve each quadrant,
   -- and report the sum

   declare Discard: Path_Data;
   begin
      while Natural(Paths_To_Consider.Current_Use) /= 0 loop
         Paths_To_Consider.Dequeue(Discard);
      end loop;
   end;

   Vault(Start_Position(1).X, Start_Position(1).Y) := Location'(Feature => Wall);
   Vault(Start_Position(1).X - 1, Start_Position(1).Y) := Location'(Feature => Wall);
   Vault(Start_Position(1).X + 1, Start_Position(1).Y) := Location'(Feature => Wall);
   Vault(Start_Position(1).X, Start_Position(1).Y - 1) := Location'(Feature => Wall);
   Vault(Start_Position(1).X, Start_Position(1).Y + 1) := Location'(Feature => Wall);
   Vault(Start_Position(1).X - 1, Start_Position(1).Y - 1)
     := Location'(Feature => Entrance);
   Vault(Start_Position(1).X + 1, Start_Position(1).Y - 1)
     := Location'(Feature => Entrance);
   Vault(Start_Position(1).X - 1, Start_Position(1).Y + 1)
     := Location'(Feature => Entrance);
   Vault(Start_Position(1).X + 1, Start_Position(1).Y + 1)
     := Location'(Feature => Entrance);

   Known_States.Clear;
   Graph.Clear;
   Convert_Map_To_Graph;
   --  declare
   --     C: Graph_Cursor := Graph.First;
   --  begin
   --     while C /= No_Graph_Element loop
   --        declare
   --           M renames Element(C);
   --           C2: Reachable_Cursor := M.First;
   --           P renames Key(C);
   --        begin
   --           while C2 /= No_RM_Element
   --           loop
   --              declare Q renames Key(C2);
   --              begin
   --                 Put(P);
   --                 case Vault(P.X, P.Y).Feature is
   --                 when Entrance => Put('@');
   --                 when Door => Put(Vault(P.X, P.Y).Which_Door);
   --                 when Key => Put(Vault(P.X, P.Y).Which_Key);
   --                 when others => raise Data_Error with "invalid feature";
   --                 end case;
   --                 Put(" -> "); Put(Q);
   --                 case Vault(Q.X, Q.Y).Feature is
   --                 when Entrance => Put('@');
   --                 when Door => Put(Vault(Q.X, Q.Y).Which_Door);
   --                 when Key => Put(Vault(Q.X, Q.Y).Which_Key);
   --                 when others => raise Data_Error with "invalid feature";
   --                 end case;
   --                 Put(": ");
   --                 Put(Element(C2)); New_Line;
   --                 Next(C2);
   --              end;
   --           end loop;
   --        end;
   --        Next(C);
   --     end loop;
   --  end;

   Start_Position(0) := ( @.X - 1, @.Y - 1 );
   Start_Position(1) := ( @.X + 1, @.Y - 1 );
   Start_Position(2) := ( @.X - 1, @.Y + 1 );
   Start_Position(3) := ( @.X + 1, @.Y + 1 );
   Setup_Search;
   Keys_To_Robot.Clear;
   for R in Robot_Range loop Determine_Keys_For(R); end loop;

   Put_Line("with four, the alternate approach has " & Find_Alternate'Image);

   --  Verbose := True;
   --  declare Shortest_Route: Path_Data := Find_Route;
   --  begin
   --     Put_Line("shortest route has length" & Shortest_Route.Cost'Image);
   --     for R in Robot_Range loop
   --        Put("Robot" & R'Image & ": ");
   --        Put(Shortest_Route.Paths(R));
   --        New_Line;
   --     end loop;
   --  end;

end Main;
