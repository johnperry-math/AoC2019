-- Advent of Code 2019, Day 18
--
-- John Perry
--
-- part 1: find the shortest path which unlocks all the doors
--
-- part 2: figure out the message, which is an ungodly number of digits
-- into an ungodly long signal, which itself has to be processed 100 times...
-- but even an efficient processor won't do the trick because it's so ungodly
-- long that you need to analyze the procedure and the input
-- to figure out how to solve it, which requires you to ignore what you did
-- in part 1 -- not to improve, but to ignore it
--
-- this needs a larger stack: ulimit -S -s 131072 on bash was more than enough

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

   Testing: constant Boolean := True;

   Filename: constant String
         := (
             if Testing then "/Users/user/common/Ada/AoC2019/Day18/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day18/input.txt"
            );

   -- SECTION
   -- debugging

   Verbose: Boolean := False;

   -- SECTION
   -- data types

   Vault_Width: constant := ( if Testing then 24 else 81 );
   Vault_Height: constant := ( if Testing then 6 else 81 );

   type Vault_Width_Range is mod Vault_Width;
   type Vault_Height_Range is mod Vault_Height;

   type Feature is ( Wall, Empty, Entrance, Door, Key );

   subtype Door_Name is Character range 'A' .. 'Z';

   subtype Key_Name is Character range 'a' .. 'z';

   type Position is record
      X: Vault_Width_Range;
      Y: Vault_Height_Range;
   end record;

   type Location ( Contains: Feature := Empty ) is record
      case Contains is
         when Wall | Empty | Entrance => null;
         when Door => Which_Door: Door_Name;
         when Key => Which_Key: Key_Name;
      end case;
   end record;

   type Vault_Map
   is array ( Vault_Width_Range , Vault_Height_Range ) of Location;

   Vault: Vault_Map;
   Start_Position: Position;

   package Doors_Needed_Package is new Ada.Containers.Ordered_Sets
     (
      Element_Type => Door_Name
     );

   package Keys_Held_Package is new Ada.Containers.Ordered_Sets
     (
      Element_Type => Key_Name
     );

   All_Keys: Keys_Held_Package.Set;

   package Path_Vectors is new Ada.Containers.Vectors
     (
      Index_Type => Positive,
      Element_Type => Position
     );

   type Path_Data is record
      Path: Path_Vectors.Vector;
      Keys_Held: Keys_Held_Package.Set;
   end record;

   package Paths_To_Consider_Interfaces
   is new Ada.Containers.Synchronized_Queue_Interfaces
     (
      Element_Type => Path_Data
     );

   function Lower_Length(Path: Path_Data) return Natural
   is ( Natural(Path.Path.Length) );

   package Path_To_Consider_Queues
   is new Ada.Containers.Unbounded_Priority_Queues
     (
      Queue_Interfaces => Paths_To_Consider_Interfaces,
      Queue_Priority => Natural,
      Get_Priority => Lower_Length,
      Before => "<"
     );

   Paths_To_Consider: Path_To_Consider_Queues.Queue;

   function Key_For_Door(D: Door_Name) return Key_Name is
     ( Ada.Characters.Handling.To_Lower(D) );

   function Is_Good_To_Explore(P: Path_Data) return Boolean is
     (
       case Vault(P.Path.Last_Element.X, P.Path.Last_Element.Y).Contains is
          when Empty | Key | Entrance => True,
          when Door =>
            P.Keys_Held.Contains( Key_For_Door( Vault(
                                                      P.Path.Last_Element.X,
                                                      P.Path.Last_Element.Y
                                                     ).Which_Door
                                              )
                                ),
          when others => False
      );

   Search_Error: exception;

   procedure Put_Line(P: Path_Vectors.Vector) is
   begin
      for Pos of P loop
         Put("(" & Pos.X'Image & "," & Pos.Y'Image & " )");
      end loop;
      New_Line;
   end Put_Line;

   Last_Length: Positive := 1;

   type State_Cache is record
      Where: Position;
      Keys: Keys_Held_Package.Set;
   end record;

   function Keys_Lex_Smaller(First, Second: Keys_Held_Package.Set)
                             return Boolean
   is
     (
      First.Difference(Second).First_Element < Second.Difference(First).First_Element
     );

   function "<"(First, Second: State_Cache) return Boolean is
     (
      First.Where.X < Second.Where.X or else
        (
         First.Where.X = Second.Where.X and then
           (
            First.Where.Y < Second.Where.Y or else
              (
               First.Where.Y = Second.Where.Y and then
                 (
                  Positive(First.Keys.Length) < Positive(Second.Keys.Length) or else
                    (
                     Positive(First.Keys.Length) = Positive(Second.Keys.Length)
                     and then
                       (
                        Keys_Lex_Smaller(First.Keys, Second.Keys)
                       )
                    )
                 )
              )
           )
        )

     );

   package Known_States_Package is new Ada.Containers.Ordered_Maps
     (
      Key_Type => State_Cache,
      Element_Type => Positive
     );

   Known_States: Known_States_Package.Map;

   function Good_To_Add(S: State_Cache; Length: Positive) return Boolean is
     (
      not ( Known_States.Contains(S) and then Known_States(S) < Length )
     );

   function Explore return Path_Data is
      Data: Path_Data;
   begin
      while Natural( Paths_To_Consider.Current_Use ) /= 0 loop
         Paths_To_Consider.Dequeue(Data);
         if Positive(Data.Path.Length) > Last_Length then
            Last_Length := Positive(Data.Path.Length);
            Put_Line("now at length" & Last_Length'Image);
            Put_Line("carrying" & Paths_To_Consider.Current_Use'Image);
         end if;
         declare
            Pos: Position := Data.Path.Last_Element;
            Prev_Pos: Position := Data.Path(Data.Path.Last_Index - 1);
            X: Vault_Width_Range := Pos.X;
            Y: Vault_Height_Range := Pos.Y;
         begin
            --  Put_Line("path of length" & Data.Path.Length'Image);
            --  Put_Line(Data.Path);
            if Vault(X, Y).Contains = Key then
               if not Data.Keys_Held.Contains(Vault(X,Y).Which_Key) then
                  Data.Keys_Held.Insert(Vault(X,Y).Which_Key);
                  declare New_Data: Path_Data := Data;
                  begin
                     New_Data.Path := Path_Vectors.Copy(Data.Path);
                     New_Data.Path.Append(Prev_Pos);
                     Paths_To_Consider.Enqueue(New_Data);
                  end;
                  Put("found key ");
                  for K of Data.Keys_Held loop
                     Put(K & ", ");
                  end loop;
                  New_Line;
               end if;
               if Keys_Held_Package."="(Data.Keys_Held, All_Keys) then
                  return Data;
               end if;
            end if;
            if Is_Good_To_Explore(Data) then
               if X > Vault_Width_Range'First and then
                 Prev_Pos.X /= X - 1 and then
                 Vault(X - 1, Y).Contains /= Wall
               then
                  if Good_To_Add( ( ( X - 1, Y ), Data.Keys_Held ), Positive(Data.Path.Length) + 1 ) then
                     declare New_Path: Path_Data := Data;
                     begin
                        New_Path.Path := Path_Vectors.Copy(Data.Path);
                        New_Path.Path.Append( ( X - 1, Y ) );
                        Paths_To_Consider.Enqueue(New_Path);
                     end;
                  end if;
               end if;
               if X < Vault_Width_Range'Last and then
                 Prev_Pos.X /= X + 1 and then
                 Vault(X + 1, Y).Contains /= Wall
               then
                  if Good_To_Add( ( ( X + 1, Y ), Data.Keys_Held ), Positive(Data.Path.Length) + 1 ) then
                     declare New_Path: Path_Data := Data;
                     begin
                        New_Path.Path := Path_Vectors.Copy(Data.Path);
                        New_Path.Path.Append( ( X + 1, Y ) );
                        Paths_To_Consider.Enqueue(New_Path);
                     end;
                  end if;
               end if;
               if Y > Vault_Height_Range'First and then
                 Prev_Pos.Y /= Y - 1 and then
                 Vault(X, Y - 1).Contains /= Wall
               then
                  if Good_To_Add( ( ( X, Y - 1 ), Data.Keys_Held ), Positive(Data.Path.Length) + 1 ) then
                     declare New_Path: Path_Data := Data;
                     begin
                        New_Path.Path := Path_Vectors.Copy(Data.Path);
                        New_Path.Path.Append( ( X, Y - 1 ) );
                        Paths_To_Consider.Enqueue(New_Path);
                     end;
                  end if;
               end if;
               if Y < Vault_Height_Range'Last and then
                 Prev_Pos.Y /= Y + 1 and then
                 Vault(X, Y + 1).Contains /= Wall
               then
                  if Good_To_Add( ( ( X, Y - 1 ), Data.Keys_Held ), Positive(Data.Path.Length) + 1 ) then
                     declare New_Path: Path_Data := Data;
                     begin
                        New_Path.Path := Path_Vectors.Copy(Data.Path);
                        New_Path.Path.Append( ( X, Y + 1 ) );
                        Paths_To_Consider.Enqueue(New_Path);
                     end;
                  end if;
               end if;
            end if;
         end;
      end loop;
      raise Search_Error with "ran out of paths";
   end Explore;

   procedure Read_Map is

      I: Vault_Height_Range := 0;

   begin

      Open(F, In_File, Filename);

      while not End_Of_File(F) loop

         declare
            S: String := Get_Line(F);
            J: Vault_Width_Range := 0;

         begin

            for C of S loop

               Vault(J, I)
                 := (
                     case C is
                        when '.' => Location'(Contains => Empty),
                        when '#' => Location'(Contains => Wall),
                        when '@' => Location'(Contains => Entrance),
                        when Door_Name =>
                          Location'(Contains => Door, Which_Door => C),
                        when Key_Name =>
                          Location'(Contains => Key, Which_Key => C),
                        when others =>
                           raise Data_Error with "invalid vault value " & C'Image
                    );
               if C = '@' then Start_Position := ( J, I ); end if;
               if C in Key_Name then
                  All_Keys.Insert(C);
               end if;

               J := J + 1;
               if J = 0 then I := I + 1; end if;

            end loop;

         end;

      end loop;

      Close(F);

   end Read_Map;

begin
   Read_Map;
   Put_Line(
            "entrance at" & Start_Position.X'Image
            & "," & Start_Position.Y'Image
           );
   declare
      First_Path, Next_Path: Path_Vectors.Vector;
      First_Keys: Keys_Held_Package.Set;
      Starting_Data: Path_Data;
   begin
      First_Path.Append( Start_Position );
      Next_Path := Path_Vectors.Copy(First_Path);
      Next_Path.Append( ( Start_Position.X + 1 , Start_Position.Y ) );
      Starting_Data := ( Next_Path, First_Keys );
      Paths_To_Consider.Enqueue(Starting_Data);
      Next_Path := Path_Vectors.Copy(First_Path);
      Next_Path.Append( ( Start_Position.X - 1 , Start_Position.Y ) );
      Starting_Data := ( Next_Path, First_Keys );
      Paths_To_Consider.Enqueue(Starting_Data);
      Next_Path := Path_Vectors.Copy(First_Path);
      Next_Path.Append( ( Start_Position.X , Start_Position.Y + 1 ) );
      Starting_Data := ( Next_Path, First_Keys );
      Paths_To_Consider.Enqueue(Starting_Data);
      Next_Path := Path_Vectors.Copy(First_Path);
      Next_Path.Append( ( Start_Position.X , Start_Position.Y - 1 ) );
      Starting_Data := ( Next_Path, First_Keys );
      Paths_To_Consider.Enqueue(Starting_Data);
      declare Best_Route: Path_Data := Explore;
      begin
         Put_Line("the shortest route is " & Best_Route.Path.Length'Image);
         Put_Line("(subtract 1 b/c of starting location)");
         Put_Line(Best_Route.Path);
      end;
   end;
end Main;
