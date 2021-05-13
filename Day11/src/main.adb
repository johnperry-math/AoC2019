-- Advent of Code 2019, Day 11
--
-- John Perry
--
-- part 1: write an Intcode robot that paints the hull, changing panels
-- from black to white or back again, and report how many panels change color
--
-- part 2: run the program again with a different input report the letters
-- that get painted

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Containers.Ordered_Maps;

with Intcode;

procedure Main is

   -- SECTION
   -- input-related

   F: File_Type; -- input file

   Testing: constant Boolean := False;

   Filename: constant String
         := (
             if Testing then "/Users/user/common/Ada/AoC2019/Day11/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day11/input.txt"
            );

   -- the Intcode robot

   package Robot is new Intcode
         (
          Max_Location            => 10000,
          Input_Size              => 1,
          Output_Size             => 2,
          Input_From_Command_Line => False,
          Output_To_Command_Line  => False
         );

   -- panels and their colors

   type Panel_Color is ( Black, White );

   type Panel is record
      X, Y: Integer;
   end record;

   -- since we don't know the maximum and minimum panel coordinates,
   -- we'll store the locations of painted panels in a map

   function "<"(First, Second: Panel) return Boolean is
         (
               First.X < Second.X
               or else ( ( First.X = Second.X and then First.Y < Second.Y )
                            )
              );

   package Panel_Colors is new Ada.Containers.Ordered_Maps
         (
          Key_Type => Panel,
          Element_Type => Panel_Color
         );

   Painted_Panels: Panel_Colors.Map;

   -- constants relating to the robot's output

   Reports_Black: constant := 0; -- robot thinks this should be painted black
   Reports_White: constant := 1; -- robot thinks this should be painted white

   Reports_Counter: constant := 0; -- robot has turned counterclockwise
   Reports_Clockwise: constant := 1; -- robot has turned clockwise

   Bad_Panel_Color: exception; -- never raised!

   -- robot movement

   type Direction is ( Up, Right, Down, Left );

   type Turn is ( Counter, Clockwise );

   Turn_Directions: array( Turn'Range , Direction'Range ) of Direction
   -- easy determination of new direction from old direction & the turn
         := (
             Counter =>
                   ( Up => Left, Left => Down, Down => Right, Right => Up ),
             Clockwise =>
                   ( Up => Right, Right => Down, Down => Left, Left => Up )
            );

   procedure Run_Robot(Initial_Value: Panel_Color) is
   -- runs the robot on the Intcode program it has already read

      Current_Position: Panel := ( X => 0, Y => 0 );
      Current_Direction: Direction := Up;
      Robot_Input: Robot.Input_Type := ( 1 => Panel_Color'Pos(Initial_Value) );

   begin

      loop

         Robot.Supply_Input(Robot_Input);
         Robot.Run_Program;

         -- get color
         if not Painted_Panels.Contains(Current_Position) then
            Painted_Panels.Insert(
                                  Current_Position,
                                  Panel_Color'Val(Robot.Report_Output(1))
                                 );
         else
            Painted_Panels(Current_Position)
                  := Panel_Color'Val(Robot.Report_Output(1));
         end if;

         -- get direction and move
         Current_Direction := Turn_Directions(
                                              Turn'Val(Robot.Report_Output(2)),
                                              Current_Direction
                                             );
         case Current_Direction is
            when Up => Current_Position.Y := Current_Position.Y + 1;
            when Down => Current_Position.Y := Current_Position.Y - 1;
            when Left => Current_Position.X := Current_Position.X - 1;
            when Right => Current_Position.X := Current_Position.X + 1;
         end case;

         -- prepare input for the robot (color of the panel it's currently on)
         Robot_Input(1)
               := (
                   if not Painted_Panels.Contains(Current_Position) then
                            Panel_Color'Pos(Black)
                   else Panel_Color'Pos(Painted_Panels(Current_Position))
                  );

         -- the robot is suspended for input iff program has not yet completed
         exit when not Robot.Is_Suspended_For_Input;

      end loop;

      Put_Line(
               "the robot has painted" & Painted_Panels.Length'Image & " panels"
              );

   end Run_Robot;

begin

   Robot.Read_Program(Filename);

   -- part 1: run while starting on a black panel

   Run_Robot(Black);

   -- part 2: run while starting on a white panel, then report the resulting
   -- registration code

   Painted_Panels.Clear;
   Painted_Panels.Insert((X => 0, Y => 0), White);

   Run_Robot(White);

   -- print registration code
   declare

      -- we'll need to know the maximum and minimum coordinates
      Min_X, Min_Y: Integer := 100;
      Max_X, Max_Y: Integer := -100;

      -- used to loop through the panels
      Location: Panel;
      Cursor: Panel_Colors.Cursor := Painted_Panels.First;

   begin

      -- first determine min, max x, y
      while Panel_Colors."/="(Cursor, Panel_Colors.No_Element) loop

         if Panel_Colors.Key(Cursor).X < Min_X then
            Min_X := Panel_Colors.Key(Cursor).X;
         elsif Panel_Colors.Key(Cursor).X > Max_X then
            Max_X := Panel_Colors.Key(Cursor).X;
         end if;

         if Panel_Colors.Key(Cursor).Y < Min_Y then
            Min_Y := Panel_Colors.Key(Cursor).Y;
         elsif Panel_Colors.Key(Cursor).Y > Max_Y then
            Max_Y := Panel_Colors.Key(Cursor).Y;
         end if;

         Panel_Colors.Next(Cursor);

      end loop;

      -- now print result
      for I in reverse Min_Y .. Max_Y loop

         Location.Y := I;

         for J in Min_X .. Max_X loop

            Location.X := J;

            if not Painted_Panels.Contains(Location)
                  or else Painted_Panels(Location) = Black
            then
               Put('.');
            else
               Put('#');
            end if;

         end loop;

         New_Line;

      end loop;

   end;

end Main;
