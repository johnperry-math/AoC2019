-- Advent of Code 2019, Day 18
--
-- John Perry
--
-- part 1: find the shortest path which unlocks all the doors
--
-- part 2:

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

   Filename: constant String
         := (
             if Testing then "/Users/user/common/Ada/AoC2019/Day18/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day18/input.txt"
            );

   -- SECTION
   -- debugging

   Verbose: Boolean := False;

   -- SECTION
   -- data types and functions for the vault

   -- SUBSECTION
   -- general information

   -- dimensions gleaned directly from the files
   Vault_Width: constant := ( if Testing then 24 else 81 );
   Vault_Height: constant := ( if Testing then 6 else 81 );

   type Vault_Width_Range is mod Vault_Width;
   type Vault_Height_Range is mod Vault_Height;

   subtype Door_Name is Character range 'A' .. 'Z';
   subtype Key_Name is Character range 'a' .. 'z';

   function Key_For_Door(D: Door_Name) return Key_Name is
      -- returns the key needed to unlock door D
     ( Ada.Characters.Handling.To_Lower(D) );

   type Key_Array is array ( Key_Name ) of Boolean with Pack;
   -- used to track which keys are needed or held

   All_Keys: Key_Array := ( others => False );
   -- tracks which keys are needed; True means "yes"

   procedure Put(Keys: Key_Array) is
      -- convenience subprogram, for debugging
   begin
      for K in Keys'Range loop
         if Keys(K) then Put(K & " "); end if;
      end loop;
   end Put;

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
   Deltas: array( Directions ) of Position := ( (0,-1), (0,1), (-1,0), (1,0) );

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

   -- SUBSECTION
   -- the vault as a grid

   type Vault_Map
   is array ( Vault_Width_Range , Vault_Height_Range ) of Location;

   Vault: Vault_Map;
   Start_Position: Position;
   -- entrance to the vault (varies by map)

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
               if C = '@' then Start_Position := ( J, I ); end if;
               if C in Key_Name then
                  All_Keys(C) := True;
               end if;

               J := J + 1;
               if J = 0 then I := I + 1; end if;

            end loop;

         end;

      end loop;

      Close(F);

   end Read_Map;

   -- SUBSECTION
   -- the vault as a graph

   type Edge is record
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

   package Graph_Maps is new Ada.Containers.Ordered_Maps
     (
      Key_Type => Edge,
      Element_Type => Positive
     );

   package Reachable_Points_Maps is new Ada.Containers.Ordered_Maps
     (
      Key_Type => Position,
      Element_Type => Natural
     );

   function "="(First, Second: Reachable_Points_Maps.Map) return Boolean is
      use Reachable_Points_Maps;
      C1: Cursor := First.First;
      C2: Cursor := Second.First;
      Result: Boolean := True;
   begin
      while Result and then ( C1 /= No_Element and C2 /= No_Element ) loop
         Result := Key(C1) = Key(C2) and Element(C1) = Element(C2);
      end loop;
      Result := Result and then C1 = No_Element and then C2 = No_Element;
      return Result;
   end "=";

   package Graph_Reachable_Maps is new Ada.Containers.Ordered_Maps
     (
      Key_Type => Position,
      Element_Type => Reachable_Points_Maps.Map
     );

   --  Graph_By_Edges: Graph_Maps.Map;

   Graph: Graph_Reachable_Maps.Map;

   -- SUBSECTION
   -- converting the grid to a graph

   function Reachable_From(X: Vault_Width_Range; Y: Vault_Height_Range)
                             return Reachable_Points_Maps.Map
   -- uses a breadth-first search to determine which interesting vault positions
   -- are reachable from position (X,Y) without passing through another
   -- interesting vault positions
   --
   -- "interesting" means a door, key, or entrance
   is

      Distance_To: Reachable_Points_Maps.Map;
      -- distances from (X,Y) to various positions in the vault
      Visiting: Position_Sets.Set;
      -- which positions we still plan to visit
      Result: Reachable_Points_Maps.Map;
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

               when Key | Door | Entrance => Result.Insert( P, D );

               when Empty =>

                  for Dir in Directions loop

                     declare Q: Position := P + Deltas(Dir);
                     begin

                        if Vault(Q.X, Q.Y).Feature /= Wall then

                           -- don't travel to the same place twice...
                           if Distance_To.Contains(Q) then
                              -- ...unless you would save time doing so
                              -- overlooked this detail on first implementation
                              Distance_To(Q) := Natural'Min(
                                                            Distance_To(Q),
                                                            D + 1
                                                           );
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

               declare C: Reachable_Points_Maps.Cursor := Graph( ( X, Y ) ).First;
               begin
                  while Reachable_Points_Maps."/="(C, Reachable_Points_Maps.No_Element) loop
                     Put( Position'(X,Y) ); Put(" -> ");
                     Put( Reachable_Points_Maps.Key(C) ); Put(":");
                     Put( Reachable_Points_Maps.Element(C)'image ); New_Line;
                     Reachable_Points_Maps.Next(C);
                  end loop;
               end;

               --  declare
               --     Edge_Data: Reachable_Points_Maps.Map := Reachable_From(X, Y);
               --     Edge_Cursor: Reachable_Points_Maps.Cursor := Edge_Data.First;
               --     use Reachable_Points_Maps;
               --  begin
               --
               --     Graph.Insert( ( X, Y ), Edge_Data );
               --
               --     while Edge_Cursor /= No_Element loop
               --        declare
               --           Initial: Position := ( X, Y );
               --           Terminal: Position := Key(Edge_Cursor);
               --           Forward: Edge := ( Initial, Terminal );
               --           Backward: Edge := ( Terminal, Initial );
               --           Distance: Positive := Element(Edge_Cursor);
               --        begin
               --           if not Graph_By_Edges.Contains( Forward ) then
               --              Graph_By_Edges.Insert( Forward, Distance );
               --              Graph_By_Edges.Insert( Backward, Distance );
               --           end if;
               --        end;
               --        Next(Edge_Cursor);
               --     end loop;
               --  end;

            end if;

         end loop;
      end loop;

   end Convert_Map_To_Graph;

   -- SECTION
   -- data types and subprograms used to solve part 1

   -- SUBSECTION
   -- data types and subprograms for tracking paths traveled

   package Path_Vectors is new Ada.Containers.Vectors
     (
      Index_Type => Positive,
      Element_Type => Position
     );

   procedure Put(Path: Path_Vectors.Vector) is
      -- prints information on the Path: positions traveled, which features
      -- are at each location, the cost of each edge, and the running cost
      -- of the trip
      Prev: Position := Start_Position;
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
            --  Cost := Graph_By_Edges( ( Prev, Curr ) );
            Cost := Graph(Prev)(Curr);
            Distance := Distance + Cost;
            Put(Cost'Image & "," & Distance'Image &"; ");
            Prev := Curr;
         end if;
      end loop;
   end Put;

   type Path_Data is record
      Cost: Positive; -- distance traveled on the path
      Path: Path_Vectors.Vector; -- positions traveled on the path
      Keys_Held: Key_Array; -- keys obtained while traveling on the path
   end record;

   package Paths_To_Consider_Interfaces
   is new Ada.Containers.Synchronized_Queue_Interfaces
     -- an interface for a queue of Path_Data
     (
      Element_Type => Path_Data
     );

   function Lower_Cost(Data: Path_Data) return Positive is ( Data.Cost );
   -- used for the priority queue below, returns Data's Cost field

   package Path_To_Consider_Queues
   is new Ada.Containers.Unbounded_Priority_Queues
     (
      Queue_Interfaces => Paths_To_Consider_Interfaces,
      Queue_Priority => Natural,
      Get_Priority => Lower_Cost,
      Before => "<"
     );

   Paths_To_Consider: Path_To_Consider_Queues.Queue;
   -- a priority queue where the higher priority is the lower cost of travel

   procedure Put_Line(P: Path_Vectors.Vector) is
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
      Where: Position;
      Keys: Key_Array := ( others => False );
   end record;

   function "="(First, Second: State_Cache) return Boolean is
      -- True iff same position and same keys held
     (
      First.Where = Second.Where and then First.Keys = Second.Keys
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
     (
      First.Where < Second.Where or else
        (
         First.Where = Second.Where and then
                               Keys_Lex_Smaller(First.Keys, Second.Keys)
        )
     );

   package Known_States_Package is new Ada.Containers.Ordered_Maps
     (
      Key_Type => State_Cache,
      Element_Type => Positive
     );

   Known_States: Known_States_Package.Map;
   -- positions we've visited, along with the keys held, mapped to the cost
   -- of traveling to that position

   -- SUBSECTION
   -- the subprograms that perform the search

   function Good_To_Add(S: State_Cache; Length: Positive) return Boolean is
      -- returns True iff we've not traveled to S.Where or if Length is smaller
      -- than the known cost of traveling to S.Where
     (
      not ( Known_States.Contains(S) and then Known_States(S) <= Length )
     );

   procedure Add_If_Good(Data: Path_Data; E: Edge; D: Positive) is
      -- adds E's Terminal to Data's Path so long as Good_To_Add signs off on it

      S: State_Cache := ( E.Stop, Data.Keys_Held ); -- state of this position
      Length: Positive := D + Data.Cost; -- how long it takes to travel here
      Loc renames Vault(E.Stop.X, E.Stop.Y); -- where we are in the vault

   begin

      -- not good if there's a door whose key we don't yet have
      if Loc.Feature /= Door or else
        Data.Keys_Held(Key_For_Door(Loc.Which_Door))
      then

         if Good_To_Add(S, Length) then

            -- update Known_States, either if S is unknown or
            -- if Length by this path is smaller than the previously known
            if Known_States.Contains(S) then
               Known_States(S) := Length;
            else
               Known_States.Insert(S, Length);
            end if;

            -- enqueue it
            declare
               New_Path: Path_Vectors.Vector := Path_Vectors.Copy(Data.Path);
            begin
               New_Path.Append(E.Stop);
               Paths_To_Consider.Enqueue( ( Length, New_Path, Data.Keys_Held ) );
            end;

         end if;

      end if;

   end Add_If_Good;

   function Find_Route return Path_Data is
      -- find a route that minimizes the cost to collect all keys

      --  Graph_Cursor: Graph_Maps.Cursor;
      --  use Graph_Maps;
      Graph_Cursor: Reachable_Points_Maps.Cursor;
      use Reachable_Points_Maps;

   begin

      -- from start...
      --  Graph_Cursor := Graph_By_Edges.First;
      --  while Graph_Cursor /= No_Element loop
      --
      --     declare
      --        E: Edge := Key(Graph_Cursor);
      --        D: Positive := Element(Graph_Cursor);
      --        K: Key_Array := ( others => False );
      --        P: Path_Vectors.Vector;
      --     begin
      --
      --        if E.Start = Start_Position then
      --           P.Append(Start_Position);
      --           P.Append(E.Stop);
      --           Paths_To_Consider.Enqueue( ( D, P, K ) );
      --        end if;
      --
      --     end;
      --
      --     Next(Graph_Cursor);
      --
      --  end loop;
      Graph_Cursor := Graph(Start_Position).First;
      while Graph_Cursor /= No_Element loop
         declare
            D: Positive := Element(Graph_Cursor);
            P: Path_Vectors.Vector;
            K: Key_Array := ( others => False );
         begin
            P.Append(Start_Position); P.Append(Key(Graph_Cursor));
            Paths_To_Consider.Enqueue( ( D, P, K) );
         end;
         Next(Graph_Cursor);
      end loop;

      -- ...consider all paths until we've found one that has all keys
      while Natural(Paths_To_Consider.Current_Use) /= 0 loop

         declare
            Data: Path_Data;
            Last: Position;
            --  C: Graph_Maps.Cursor := Graph_By_Edges.First;
            C: Reachable_Points_Maps.Cursor;
         begin

            Paths_To_Consider.Dequeue(Data);
            Last := Data.Path.Last_Element;

            -- if this position has a key, add it to the record of keys held;
            -- if this completes the collection, we are finished!
            if Vault(Last.X, Last.Y).Feature = Key and then
              not Data.Keys_Held(Vault(Last.X, Last.Y).Which_Key)
            then
               Data.Keys_Held(Vault(Last.X, Last.Y).Which_Key) := True;
               if All_Keys = Data.Keys_Held then return Data; end if;
               -- add to the list of states, as it could prevent consideration
               -- of a longer path to take these keys
               declare S: State_Cache := ( Last, Data.Keys_Held );
               begin
                  if not Known_States.Contains(S) then
                     Known_States.Insert( S, Data.Cost );
                  end if;
               end;
            end if;

            -- we aren't done, so continue search unless we've hit a door
            -- and don't have the key
            -- (I get a speedup here from assuming that the graph's map stores
            -- edges with the same start location in a contiguous area
            -- of the map, which is should on account of the ordering)
            if Vault(Last.X, Last.Y).Feature /= Door or else
              Data.Keys_Held(Key_For_Door(Vault( Last.X, Last.Y ).Which_Door))
            then
               --  -- find edges that start from location Last
               --  while Key(C).Start /= Last loop Next(C); end loop;
               --  -- process all edges that start from Last
               --  while C /= No_Element and then Key(C).Start = Last loop
               --     declare
               --        E: Edge := Key(C);
               --        D: Positive := Element(C);
               --     begin
               --        if E.Start = Last then Add_If_Good(Data, E, D); end if;
               --     end;
               --     Next(C);
               --  end loop;
               C := Graph(Last).First;
               while C /= No_Element loop
                  Add_If_Good(Data, ( Last, Key(C) ), Element(C));
                  Next(C);
               end loop;
            end if;

         end;

      end loop;

      -- this should never happen on a well-formed maze & a well-written program
      -- ;-)
      raise Search_Error with "ran out of paths to search!";

   end Find_Route;

begin

   -- read maze and report on it
   Read_Map;
   Put("entrance at"); Put(Start_Position); New_Line;
   -- convert to graph and report the edges
   Convert_Map_To_Graph;
   --  declare
   --     C: Graph_Maps.Cursor := Graph_By_Edges.First;
   --  begin
   --     while Graph_Maps."/="(C, Graph_Maps.No_Element) loop
   --        Put(Graph_Maps.Key(C));
   --        Put_Line( ":" & Graph_Maps.Element(C)'Image );
   --        Graph_Maps.Next(C);
   --     end loop;
   --  end;
   -- find the shortest route and report on it
   declare Shortest_Route: Path_Data := Find_Route;
   begin
      Put_Line("shortest route has length" & Shortest_Route.Cost'Image);
      Put(Shortest_Route.Path);
   end;
end Main;
