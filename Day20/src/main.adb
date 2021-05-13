-- Advent of Code 2019, Day 20
--
-- John Perry
--
-- another maze puzzle
--
-- part 1: find the shortest path through a maze where you have to traverse
-- portals
--
-- part 2: some portals lead to deeper mazes; find the shortest path out in
-- this case
--
-- I got part 1 easily on the first try, once I ironed out the wrinkles in the
-- code. Part 2 was another story; the solution I came up with was incorrect in
-- multiple ways. For isntance, I didn't think about how the direction you pass
-- through a portal affects which portals are available.

pragma Ada_2020;

-- Ada libraries

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;

with Ada.Characters.Handling;

procedure Main is

   -- SECTION
   -- input-related

   F : File_Type; -- input file

   Testing : constant Boolean := False;
   Which_Test : constant := 1;

   Filename : constant String
      := (
          if Testing then (
             if Which_Test = 1
             then "/Users/user/common/Ada/AoC2019/Day20/example.txt"
             elsif Which_Test = 2
             then "/Users/user/common/Ada/AoC2019/Day20/example2.txt"
             elsif Which_Test = 3
             then "/Users/user/common/Ada/AoC2019/Day20/example3.txt"
             else "/Users/user/common/Ada/AoC2019/Day20/example.txt"
            )
          else "/Users/user/common/Ada/AoC2019/Day20/input.txt"
         );

   -- SECTION
   -- debugging

   Verbose : Boolean := False;

   -- SECTION
   -- data types and subprograms for the maze

   -- SUBSECTION
   -- maze dimensions and storage

   Num_Rows : constant
      := (
          if not Testing then 123
          else
             (
              case Which_Test is
                 when 1 => 15,
                 when 2 => 33,
                 when 3 => 33,
                 when others => 123
             )
         );
   Num_Columns : constant
      := (
          if not Testing then 131
          else
             (
              case Which_Test is
                 when 1 => 17,
                 when 2 => 31,
                 when 3 => 41,
                 when others => 131
             )
         );
   subtype Row_Range is Positive range 1 .. Num_Rows;
   subtype Col_Range is Positive range 1 .. Num_Columns;

   -- SUBSECTION
   -- positions within the maze

   type Position_Type is record
      Row : Row_Range;
      Col : Col_Range;
   end record;

   function "=" (First, Second : Position_Type) return Boolean is
      ( First.Row = Second.Row and then First.Col = Second.Col );

   function "<" (First, Second : Position_Type) return Boolean is
      (
        First.Row < Second.Row or else
           ( First.Row = Second.Row and then First.Col < Second.Col )
       );

   -- SUBSECTION
   -- motion while exploring the maze

   type Directions is ( Up, Down, Left, Right );

   subtype Delta_Range is Integer range -1 .. 1;

   type Position_Delta is record Row, Col : Delta_Range; end record;

   Position_Deltas : array ( Directions ) of Position_Delta
      := (
          Up    => ( -1,  0 ),
          Down  => (  1,  0 ),
          Left  => (  0, -1 ),
          Right => (  0,  1 )
         );

   function Adjacent_Position (Position : Position_Type; Which : Directions)
                               return Position_Type
   is (  (
           Position.Row + Position_Deltas (Which).Row,
           Position.Col + Position_Deltas (Which).Col
          ) );

   -- SUBSECTION
   -- portal identification and correlation

   subtype Portal_Letters is Character range 'A' .. 'Z';

   type Portal_Name is array ( 1 .. 2 ) of Character;

   function "=" ( First, Second : Portal_Name ) return Boolean
   is ( First (1) = Second (1) and then First (2) = Second (2) );

   function "<" (First, Second : Portal_Name) return Boolean
   is (
        First (1) < Second (1) or else
           ( First (1) = Second (1) and then First (2) < Second (2) )
       );

   type Portal_Details is record
      Inner, Outer : Position_Type := ( 1, 1 );
   end record;

   function "=" ( First, Second : Portal_Details ) return Boolean is
      ( First.Inner = Second.Inner and then First.Outer = Second.Outer );

   function "<" ( First, Second : Portal_Details ) return Boolean is
      (
        First.Inner < Second.Inner or else
           ( First.Inner = Second.Inner and then First.Outer < Second.Outer )
       );

   package Portal_Maps is new Ada.Containers.Ordered_Maps
      (
       Key_Type     => Portal_Name,
       Element_Type => Portal_Details
      );

   All_Portals : Portal_Maps.Map;

   package Locations_Of_Portals_Maps is new Ada.Containers.Ordered_Maps
      (
       Key_Type     => Position_Type,
       Element_Type => Portal_Name
      );

   Locations_Of_Portals : Locations_Of_Portals_Maps.Map;

   package Reachable_Portal_Maps is new Ada.Containers.Ordered_Maps
      (
       Key_Type     => Position_Type,
       Element_Type => Positive
      );

   type Inner_And_Outer_Connections is record
      Inner, Outer : Reachable_Portal_Maps.Map;
   end record;

   function "=" (First, Second : Inner_And_Outer_Connections) return Boolean
   is
      use type Reachable_Portal_Maps.Map;
   begin
      return First.Inner = Second.Inner and then First.Outer = Second.Outer ;
   end "=";

   package Portal_Connections_Maps is new Ada.Containers.Ordered_Maps
      (
       Key_Type     => Portal_Name,
       Element_Type => Inner_And_Outer_Connections,
       "<"          => "<"
      );

   Portal_Connections : Portal_Connections_Maps.Map;

   type Portal_Direction is ( Inner, Outer );

   -- SUBSECTION
   -- maze features and storage proper

   type Feature_Type is ( Space, Wall, Portal, Empty );

   type Maze_Type is array ( Row_Range, Col_Range ) of Feature_Type;
   Maze : Maze_Type := ( others => ( others => Space ) );
   -- temporary storage until we work out the portals' relationships

   -- SECTION
   -- subprograms

   -- SUBSECTION
   -- preliminary setup

   procedure Update_Portals
      ( Which : Portal_Name; Kind : Portal_Direction; Where : Position_Type )
      -- update portal "Which" to position "Where" indicating "kind" of portal
   is

      C : Portal_Maps.Cursor := All_Portals.Find (Which);
      use type Portal_Maps.Cursor;

   begin

      if Verbose then
         Put_Line ("updating " & Which (1) & Which (2) & " to" & Where'Image);
      end if;

      Locations_Of_Portals.Insert ( Where, Which );

      if C = Portal_Maps.No_Element then
         All_Portals.Insert
            (
             Which,
             (
              if Kind = Inner then ( Inner => Where , Outer => <> )
              else ( Inner => <> , Outer => Where )
             )
            );
      else
         if Kind = Inner then
            All_Portals.Reference (C).Inner := Where;
         else
            All_Portals.Reference (C).Outer := Where;
         end if;
      end if;

   end Update_Portals;

   procedure Read_Maze is
   -- read the maze from the file and store it to Maze

      Prev_Line  : String := Get_Line (F);
      New_Portal : Portal_Name;

   begin

      -- read first two lines for portals
      declare
         Line2 : String := Get_Line (F);
      begin
         for Col in Prev_Line'Range loop
            if Prev_Line (Col) /= ' ' then
               Maze ( 1, Col - 2 ) := Portal;
               New_Portal := ( Prev_Line (Col), Line2 (Col) );
               Update_Portals ( New_Portal, Outer, ( 1, Col - 2 ) );
            end if;
         end loop;
         Prev_Line := Line2;
      end;

      -- read until end of maze
      for Row in Row_Range loop

         declare
            Line    : String := Get_Line (F);
            J0      : Integer := Line'First;
            Col     : Natural := Col_Range'First;
            In_Maze : Boolean := False;
            -- In_Maze tracks whether the current Col position is in the maze
            -- we decide this by looking at whether we've moved into or out of
            -- spaces; at first we're in a space, so we're out of the maze;
            -- then we see spaces or walls, so we're in, then we're in spaces
            -- again, so we're out, etc.
         begin

            -- if the first character isn't a space,
            -- then we're looking at a portal's label
            -- (determined by visual inspection of data)
            if Line (J0) /= ' ' then
               Maze (Row, 1) := Portal;
               New_Portal := ( Line (J0), Line (J0 + 1) );
               Update_Portals ( New_Portal, Outer, ( Row, 1 ) );
               Col := Col + 1;
            end if;

            -- now we're into the maze; process the data
            In_Maze := True;

            while Col <= Col_Range'Last loop

               case Line (J0 + Col + 1) is

                  when '#' =>
                     Maze (Row, Col) := Wall;
                     In_Maze := True;

                  when '.' => null;
                     -- "space" is the default value in Maze, so nothing to do

                  when ' ' =>
                     Maze (Row, Col) := Empty;
                     In_Maze := False;

                  when others => -- portal; this case is complicated

                     if In_Maze then -- portal to left of label; we've moved out

                        Maze (Row, Col - 1) := Portal;
                        Maze ( Row, Col ) := Empty;
                        Maze ( Row, Col + 1) := Empty;
                        New_Portal
                           := ( Line (J0 + Col + 1), Line (J0 + Col + 2) );
                        Update_Portals ( New_Portal, Inner, ( Row, Col - 1 ) );
                        Col := Col + 1;
                        In_Maze := False;

                     elsif Prev_Line (J0 + Col + 1) in Portal_Letters then

                        -- this is a vertical label
                        -- processing this is a little tough;
                        -- I chose to skip when first encountering a vertical
                        -- label (see below), then on the label's second row
                        -- look at the row above it and process then

                        if Maze (Row - 2, Col) = Space then

                           -- portal above label
                           Maze (Row, Col) := Empty;
                           Maze (Row - 1, Col) := Empty;
                           Maze (Row - 2, Col) := Portal;
                           New_Portal
                              := ( Prev_Line (J0 + Col + 1), Line (J0 + Col + 1) );
                           Update_Portals ( New_Portal, Inner, ( Row - 2, Col ) );

                        else

                           -- portal below label
                           Maze (Row - 1, Col) := Empty;
                           Maze (Row, Col) := Empty;
                           Maze (Row + 1, Col) := Portal;
                           New_Portal
                              := ( Prev_Line (J0 + Col + 1), Line (J0 + Col + 1) );
                           Update_Portals ( New_Portal, Inner, ( Row + 1, Col ) );

                        end if;

                     elsif Line (J0 + Col + 2) in Portal_Letters then

                        -- portal to right of label
                        Maze (Row, Col) := Empty;
                        Maze (Row, Col + 1) := Empty;
                        Maze (Row, Col + 2) := Portal;
                        New_Portal
                           := ( Line (J0 + Col + 1), Line (J0 + Col + 2) );
                        Update_Portals ( New_Portal, Inner, ( Row, Col + 2 ) );
                        Col := Col + 3;
                        In_Maze := True;

                     else

                        -- beginning of portal below label
                        -- do not add portal now; will do so on next line
                        Maze (Row, Col) := Empty;

                     end if;

               end case;

               Col := Col + 1;

            end loop;

            -- possibly a portal to right of label now that we've left maze
            if Line (J0 + Col + 1) /= ' ' then
               Maze (Row, Col - 1) := Portal;
               New_Portal := ( Line (J0 + Col + 1), Line (J0 + Col + 2) );
               Update_Portals ( New_Portal, Outer, ( Row, Col - 1 ) );
            end if;

            Prev_Line := Line;

         end;

      end loop;

      -- read last two lines for portals
      declare
         Line1 : String := Get_Line (F);
         Line2 : String := Get_Line (F);
      begin
         for Col in Line1'Range loop
            if Line1 (Col) /= ' ' then
               Maze ( Row_Range'Last, Col - 2 ) := Portal;
               New_Portal := ( Line1 (Col), Line2 (Col) );
               Update_Portals ( New_Portal, Outer, ( Row_Range'Last, Col - 2 ) );
            end if;
         end loop;
      end;

      -- the last two parts show some information on what was read
      -- the first prints the matrix to the console, indicating portal locations
      -- the second lists each portal's location
      -- set Verbose to True to see this

      if Verbose then

         for Row in Row_Range loop
            for Col in Col_Range loop
               Put ( (
                     case Maze (Row, Col) is
                        when Space => '.',
                        when Empty => ' ',
                        when Portal => 'P',
                        when Wall => '#'
                    ) );
            end loop;
            New_Line;
         end loop;

         for C in Locations_Of_Portals.Iterate loop
            declare
               Where : Position_Type renames Locations_Of_Portals_Maps.Key (C);
               Which : Portal_Name renames Locations_Of_Portals_Maps.Element (C);
            begin
               Put (Where.Row'Image); Put (",");
               Put (Where.Col'Image); Put (" : ");
               Put (Which (1)); Put (Which (2)); Put (" : ");
               Put ( (
                     if All_Portals (Which).Inner = Where
                     then 'I' else 'O'
                    ) );
               New_Line;
            end;
         end loop;

      end if;

   end Read_Maze;

   procedure Explore (
                      Curr_Pos : Position_Type; -- current position
                      Prev_Pos : Position_Type; -- previous position
                      P_Pos    : Position_Type; -- position of portal P
                      P        : Portal_Name;   -- portal we're connecting
                      Dist     : Natural        -- how far we've traveled
                     )
      -- determines where each portal leads
   is
      Is_Inner : Boolean := All_Portals (P).Inner = P_Pos;
   begin

      for Dir in Directions loop

         declare
            Next_Pos : Position_Type := Adjacent_Position (Curr_Pos, Dir);
            -- the position we'll move to from Curr_Pos
         begin

            -- don't go back previous position for more exploration
            if Next_Pos /= Prev_Pos then

               if Maze (Next_Pos.Row, Next_Pos.Col) = Space then
                  Explore ( Next_Pos, Curr_Pos, P_Pos, P, Dist + 1);

               elsif Maze (Next_Pos.Row, Next_Pos.Col) = Portal then

                  -- we have a portal; record connection to P
                  if Is_Inner then
                     Portal_Connections (P).Inner.Include (Next_Pos, Dist + 1);
                  else
                     Portal_Connections (P).Outer.Include (Next_Pos, Dist + 1);
                  end if;

                  -- now record P's connection to new portal
                  declare
                     Next_Portal : Portal_Name
                        := Locations_Of_Portals (Next_Pos);
                  begin
                     if All_Portals (Next_Portal).Inner = Next_Pos then
                        Portal_Connections (Next_Portal).Inner.Include
                           (P_Pos, Dist + 1);
                     else
                        Portal_Connections (Next_Portal).Outer.Include
                           (P_Pos, Dist + 1);
                     end if;
                  end;

               end if;

            end if;

         end;

      end loop;

   end Explore;

   procedure Make_Connections_From (Position : Position_Type; P : Portal_Name)
      -- determine the connections from the portal P, located at Position
   is
   begin
      if Position.Row = Row_Range'First or else
         (
           Position.Row < Row_Range'Last and then
           Maze (Position.Row + 1, Position.Col) = Space
          )
      then
         Explore (Adjacent_Position (Position, Down), Position, Position, P, 1);
      elsif Position.Row = Row_Range'Last or else
         (
           Position.Row > Row_Range'First and then
           Maze (Position.Row - 1, Position.Col) = Space
          )
      then
         Explore ( Adjacent_Position (Position, Up), Position, Position, P, 1 );
      elsif Position.Col = Col_Range'First or else
         (
           Position.Col < Col_Range'Last and then
           Maze (Position.Row, Position.Col + 1) = Space
          )
      then
         Explore (Adjacent_Position (Position, Right), Position, Position, P, 1);
      else
         Explore (Adjacent_Position (Position, Left), Position, Position, P, 1);
      end if;
   end Make_Connections_From;

   procedure Make_All_Connections is
      use all type Locations_Of_Portals_Maps.Cursor;
      function Key (C : Locations_Of_Portals_Maps.Cursor) return Position_Type
                    renames Locations_Of_Portals_Maps.Key;
      function Element (C : Locations_Of_Portals_Maps.Cursor) return Portal_Name
                        renames Locations_Of_Portals_Maps.Element;
   begin

      -- sets up portals with their connections, give some output
      if Verbose then Put_Line ("all portals: "); end if;
      for PC in All_Portals.Iterate loop
         declare
            P_Name renames Portal_Maps.Key (PC);
            P_Details renames Portal_Maps.Element (PC);
         begin
            if Verbose then
               Put (P_Name (1)); Put (P_Name (2));
               Put (" ( I("); Put (P_Details.Inner.Row'Image); Put (" ,");
               Put (P_Details.Inner.Col'Image); Put ( " ) O(");
               Put (P_Details.Outer.Row'Image); Put (" ,");
               Put (P_Details.Outer.Col'Image); Put ( " ) )");
               New_Line;
            end if;
            Portal_Connections.Insert
               (
                P_Name,
                (
                 Reachable_Portal_Maps.Empty_Map,
                 Reachable_Portal_Maps.Empty_Map
                )
               );
         end;
      end loop;
      New_Line;

      -- make the connections
      for C in Locations_Of_Portals.Iterate loop
         Make_Connections_From (Key (C), Element (C));
      end loop;

      -- report the connections; to see this, activate verbosity
      if Verbose then
         for Portal_Cur in Portal_Connections.Iterate loop

            declare
               Which       : Portal_Name
                  := Portal_Connections_Maps.Key (Portal_Cur);
               Where_Inner : Reachable_Portal_Maps.Map
                  := Portal_Connections_Maps.Element (Portal_Cur).Inner;
               Where_Outer : Reachable_Portal_Maps.Map
                  := Portal_Connections_Maps.Element (Portal_Cur).Outer;

            begin

               -- label
               Put (Which (1)); Put (Which (2)); Put_Line (" :");

               -- inner connections
               Put ("    inner: ");
               for Dest_Cur in Where_Inner.Iterate loop
                  declare
                     Dest_Portal : Portal_Name
                        := Locations_Of_Portals
                           (Reachable_Portal_Maps.Key (Dest_Cur));
                  begin
                     Put (Dest_Portal (1)); Put (Dest_Portal (2));
                     Put (" : ");
                     Put (Reachable_Portal_Maps.Element (Dest_Cur)'Image);
                     Put ("; ");
                  end;
               end loop;
               New_Line;

               -- outer connections
               Put ("    outer: ");
               for Dest_Cur in Where_Outer.Iterate loop
                  declare
                     Dest_Portal : Portal_Name
                        := Locations_Of_Portals (Reachable_Portal_Maps.Key (Dest_Cur));
                  begin
                     Put (Dest_Portal (1)); Put (Dest_Portal (2));
                     Put (" :");
                     Put (Reachable_Portal_Maps.Element (Dest_Cur)'Image);
                     Put ("; ");
                  end;
               end loop;
               New_Line;

            end;

         end loop;
      end if;

   end Make_All_Connections;

   -- SECTION
   -- data types and variables for recording routes in part 1

   type Route_Record is record
      P        : Position_Type;
      Distance : Natural;
   end record;

   function Lower_Cost (R : Route_Record) return Natural is ( R.Distance );

   package Search_Route_Interfaces
   is new Ada.Containers.Synchronized_Queue_Interfaces
      (
       Element_Type => Route_Record
      );

   package Search_Route_Queues
   is new Ada.Containers.Unbounded_Priority_Queues
      (
       Queue_Interfaces => Search_Route_Interfaces,
       Queue_Priority   => Natural,
       Get_Priority     => Lower_Cost,
       Before           => "<"
      );

   function Find_Route return Natural is
   -- this procedure solves part 1 by finding the shortest route from AA to ZZ

      package Passed_Portal_Maps is new Ada.Containers.Ordered_Maps
         (
          Key_Type     => Portal_Name,
          Element_Type => Boolean
         );
      Passed_Portal : Passed_Portal_Maps.Map;
      -- tracks which portals we've passed, to avoid creating loops

      use all type Reachable_Portal_Maps.Cursor;

      Search_Routes : Search_Route_Queues.Queue;
      -- queue of routes we're searching, prioritized by shortest route first

   begin

      -- we haven't passed any portals yet
      for P in All_Portals.Iterate loop
         Passed_Portal.Insert (Portal_Maps.Key (P), False);
      end loop;

      -- enqueue all portals reachable from AA
      for Cur in Portal_Connections (('A', 'A')).Outer.Iterate loop
         Search_Routes.Enqueue ( ( Key (Cur), Element (Cur) ) );
      end loop;

      loop -- loop until we find a solution

         declare
            P_Record : Route_Record;
            Which    : Portal_Name;
         begin

            Search_Routes.Dequeue (P_Record);
            Which := Locations_Of_Portals (P_Record.P);

            -- are we done?
            if Which = ( 'Z', 'Z' ) then
               -- yes
               Put_Line ("maximum Queued is " & Search_Routes.Peak_Use'Image);
               return P_Record.Distance;
            end if;

            -- no; record that we passed through this portal,
            -- then add new routes
            Passed_Portal (Which) := True;

            declare
               -- if we pass through the inner, we need the outer connections,
               -- and vice versa
               Available_Portals : Reachable_Portal_Maps.Map
                  := (
                      if P_Record.P = All_Portals (Which).Inner then
                         Portal_Connections ( Which ).Outer
                      else
                         Portal_Connections ( Which ).Inner
                     );
               Distance          : Natural := P_Record.Distance + 1;
               use type Reachable_Portal_Maps.Cursor;
               function Key (C : Reachable_Portal_Maps.Cursor)
                             return Position_Type
                             renames Reachable_Portal_Maps.Key;
               function Element (C : Reachable_Portal_Maps.Cursor)
                                 return Positive
                                 renames Reachable_Portal_Maps.Element;
            begin

               for Cur in Available_Portals.Iterate loop
                  if not Passed_Portal (Locations_Of_Portals (Key (Cur))) then
                     Search_Routes.Enqueue
                        ( ( Key (Cur), Element (Cur) + Distance ) );
                  end if;
               end loop;

            end;

         end;

      end loop;

   end Find_Route;

   -- SECTION
   -- data type and variables for part 2

   -- SUBSECTON
   -- routes for this part need to know their depth in addition to the distance
   -- traveled

   subtype Depth_Type is Natural;

   type Recursive_Route_Record is record
      P        : Position_Type;
      Distance : Natural;
      Depth    : Depth_Type;
   end record;

   function Lower_Cost (R : Recursive_Route_Record) return Natural
   is ( R.Distance );

   package Search_Recursive_Route_Interfaces
   is new Ada.Containers.Synchronized_Queue_Interfaces
      (
       Element_Type => Recursive_Route_Record
      );

   package Search_Recursive_Route_Queues
   is new Ada.Containers.Unbounded_Priority_Queues
      (
       Queue_Interfaces => Search_Recursive_Route_Interfaces,
       Queue_Priority   => Natural,
       Get_Priority     => Lower_Cost,
       Before           => "<"
      );

   -- when we record passing through a portal we need to know the depth,
   -- since we can pass through a portal at different depths
   type Recursive_Portal_Type is record
      Location : Position_Type;
      Level    : Depth_Type;
   end record;

   function "<" (First, Second : Recursive_Portal_Type) return Boolean
   is (
        First.Location < Second.Location or else
           ( First.Location = Second.Location and then First.Level < Second.Level )
       );

   -- SUBSECTON
   -- subprograms for part 2

   function Find_Recursive_Route return Natural is
   -- this procedure solves part 2 by finding the shortest route from AA to ZZ
   -- when the portals lead into and out of recursive copies of the maze

      Search_Recursive_Routes : Search_Recursive_Route_Queues.Queue;

      -- track which portals we've passed, at which depths
      package Passed_Portals_Maps is new Ada.Containers.Ordered_Maps
         (
          Key_Type     => Recursive_Portal_Type,
          Element_Type => Boolean
         );
      Passed_Portals : Passed_Portals_Maps.Map;
      use all type Portal_Maps.Cursor;

   begin

      -- close off the outer walls for all but AA and ZZ
      for Cur in All_Portals.Iterate loop
         if Portal_Maps.Key (Cur) /= ( 'Z', 'Z' ) then
            Passed_Portals.Include( ( Portal_Maps.Element(Cur).Outer, 0 ), True );
         end if;
      end loop;

      -- enqueue the initial destinations
      for Cur in Portal_Connections (('A', 'A')).Outer.Iterate loop
         Search_Recursive_Routes.Enqueue
            ( (
              Reachable_Portal_Maps.Key (Cur),
              Reachable_Portal_Maps.Element (Cur),
              2
             ) );
         Passed_Portals.Include
            ( ( Reachable_Portal_Maps.Key (Cur), 1 ), False );
      end loop;

      -- loop through enqueued positions
      loop

         declare

            P_Record : Recursive_Route_Record; -- route we're examining
            Which    : Portal_Name; -- portal at end of current route
            Depth    : Depth_Type; -- depth of current portal
            Is_Outer : Boolean; -- whether it's an outer or inner portal

         begin

            -- get information on current position
            Search_Recursive_Routes.Dequeue (P_Record);
            Which := Locations_Of_Portals (P_Record.P);
            Depth := P_Record.Depth;
            Is_Outer := All_Portals (Which).Outer = P_Record.P;

            -- quit when we reach ZZ; this should happen only when Depth = 0
            if Which = ( 'Z', 'Z' ) then
               Put_Line (
                         "maximum Queued is "
                         & Search_Recursive_Routes.Peak_Use'Image
                        );
               return P_Record.Distance;
            end if;

            -- add reachable portals
            declare

               Distance : Natural := P_Record.Distance + 1;
               P_Name   : Portal_Name := Locations_Of_Portals ( P_Record.P );

               Available_Portals : Reachable_Portal_Maps.Map
                  := (
                      if not Is_Outer then Portal_Connections ( P_Name ).Outer
                      else Portal_Connections (P_Name).Inner
                     );

            begin

               for Cur in Available_Portals.Iterate loop

                  declare -- information on a reachable portal

                     Where    : Position_Type := Reachable_Portal_Maps.Key (Cur);
                     Which    : Portal_Name := Locations_Of_Portals (Where);
                     Details  : Portal_Details := All_Portals (Which);
                     Is_Inner : Boolean := Where = Details.Inner;

                     New_Dist  : Natural
                        := Distance + Reachable_Portal_Maps.Element (Cur);
                     New_Depth : Depth_Type;

                  begin

                     -- only enqueue for portals we should enqueue
                     if Which /= ( 'A', 'A' ) and then
                        ( if Which = ( 'Z', 'Z' ) then Depth = 1 )
                     then
                        New_Depth := Depth + ( if Is_Inner then 1 else -1 );

                        -- the following test can be omitted, but the program
                        -- takes a little more time
                        if not Passed_Portals.Contains ( ( Where, New_Depth ) )
                        then
                           Search_Recursive_Routes.Enqueue
                              ( ( Where, New_Dist, New_Depth ) );
                           Passed_Portals.Include (( Where, New_Depth), True );
                        end if;
                     end if;

                  end;

               end loop;

            end;

         end;

      end loop;

   end Find_Recursive_Route;

   -- SECTION
   -- main program

begin

   -- get information on maze
   Open (F, In_File, Filename);
   Read_Maze;
   Close (F);

   -- set up portal connections
   Make_All_Connections;

   -- solve and report results
   Put_Line ("shortest route is" & Find_Route'Image);
   Put_Line ("shortest recursive route is" & Find_Recursive_Route'Image);

end Main;
