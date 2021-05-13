-- Advent of Code 2019, Day 12
--
-- John Perry
--
-- part 1: determine total energy in a system of moons
--
-- part 2: determine how long it will take for the system to repeat state
--
-- it turns out that in part 2 it suffices to determine how long it takes
-- for the system to repeat initial state, which you can obtain by computing
-- the lcm of how long it takes to repeat initial state in each dimension.
--
-- in fact, you can obtain this by computing the lcm of how long it takes
-- the system to become stationary in each dimension, which indicates
-- how long it takes the system to reach its "halfway point".
-- multiply that result by 2, and you're done.
--
-- (confession: I didn't work this out on my own, but after a while of working
-- with a slightly flawed approach, and a longer while of just plain flailing,
-- I looked up the general idea online)
--
-- MOMENT OF VENTING THAT I WILL REGRET LATER
-- i like part 2 in principle, but not in practice.
-- no hints were given, nor were the examples useful in any way.
-- even the examples' size was unhelpful: the smallest has a period of 2772,
-- with the dimensions' periods being 9, 14, and 22.
-- this excluded any sort of careful analysis.

pragma Ada_2020;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Containers.Ordered_Maps;

procedure Main is

   -- SECTION
   -- input-related

   F: File_Type; -- input file

   Testing: constant Boolean := False;

   Filename: constant String
         := (
             if Testing then "/Users/user/common/Ada/AoC2019/Day12/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day12/input.txt"
            );

   Iterations_Part1: constant := ( if Testing then 10 else 1000 );

   -- SECTION
   -- the moons' structure

   type Moon is record
      X, Y, Z: Integer;
      Dx, Dy, Dz: Integer := 0;
   end record;

   type Moon_Names is ( Io, Europa, Ganymede, Callisto);

   type Moon_System is array( Moon_Names ) of Moon;

   Jovian_Moons: Moon_System;

   -- SECTION
   -- moon I/O

   procedure Put(M: Moon; Velocity: Boolean := True) is
   -- output the moon
   begin
      Put("<x=" & M.X'Image & ", y=" & M.Y'Image & ", z=" & M.Z'Image & ">" );
      if Velocity then
         Put(
             " <dx=" & M.Dx'Image & ", dy=" & M.Dy'Image
             & ", dz=" & M.Dz'Image & ">"
            );
      end if;
   end Put;

   procedure Read_Initial_Position(M: in out Moon) is
   -- reads the initial position of the moon M from a line of the file
   -- completely reads the line, according to the input specification given
      Ch: Character;
   begin
      -- "<x="
      for I in 1 .. 3 loop Get(F, Ch); end loop;
      -- x, y, z, with comma and label separation
      Get(F, M.X);
      for I in 1 .. 4 loop Get(F, Ch); end loop;
      Get(F, M.Y);
      for I in 1 .. 4 loop Get(F, Ch); end loop;
      Get(F, M.Z);
      Get(F, Ch);
      M.Dx := 0; M.Dy := 0; M.Dz := 0;
   end;

   -- SECTION
   -- part 1: simple celestial mechanics

   procedure Determine_Velocities(M: in out Moon) is
   -- compute the velocities due to mutual gravity from each moon
   -- and record within the moon itself

      DeltaX_MN, DeltaY_MN, DeltaZ_MN: Integer;

   begin

      -- loop through all the other moons
      for Name in Jovian_Moons'Range loop
         if M /= Jovian_Moons(Name) then

            declare N renames Jovian_Moons(Name);
            begin

               -- determine the deltas for each dimension
               DeltaX_MN := M.X - N.X;
               if DeltaX_MN /= 0 then
                  DeltaX_MN := abs(DeltaX_MN) / DeltaX_MN;
               end if;
               DeltaY_MN := M.Y - N.Y;
               if DeltaY_MN /= 0 then
                  DeltaY_MN := abs(DeltaY_MN) / DeltaY_MN;
               end if;
               DeltaZ_MN := M.Z - N.Z;
               if DeltaZ_MN /= 0 then
                  DeltaZ_MN := abs(DeltaZ_MN) / DeltaZ_MN;
               end if;

               -- record them
               M.Dx := M.Dx - DeltaX_MN;
               M.Dy := M.Dy - DeltaY_MN;
               M.Dz := M.Dz - DeltaZ_MN;

            end;

         end if;
      end loop;

   end Determine_Velocities;

   procedure Apply_Velocities(M: in out Moon) is
   -- adjust position according to velocity computed in Determine_Velocities
   begin
      M.X := M.X + M.Dx;
      M.Y := M.Y + M.Dy;
      M.Z := M.Z + M.Dz;
   end Apply_Velocities;

   function Potential_Energy(M: Moon) return Integer is
   -- the moon's potential energy
   ( abs(M.X) + abs(M.Y) + abs(M.Z) );

   function Kinetic_Energy(M: Moon) return Integer is
   -- the moon's kinetic energy
   ( abs(M.Dx) + abs(M.Dy) + abs(M.Dz) );

   function Total_Energy(M: Moon) return Integer is
   -- the moon's total energy
   ( Potential_Energy(M) * Kinetic_Energy(M) );

   -- SECTION
   -- part 2: find how long it takes to return to a previous state

   function Gcd(A,B: Long_Long_Integer) return Long_Long_Integer is
   -- returns True iff gcd(A,B) = 1
      M: Long_Long_Integer := Long_Long_Integer'Max(abs(A), abs(B));
      N: Long_Long_Integer := Long_Long_Integer'Min(abs(A), abs(B));
      Q, R: Long_Long_Integer;
   begin
      while N /= 0 loop
         Q := M / N;
         R := M mod N;
         M := N;
         N := R;
      end loop;
      return M;
   end;

   function Lcm(A, B: Long_Long_Integer) return Long_Long_Integer is
   -- compute the leasat common multiple of A and B
   ( A / Gcd(A, B) * B );

   type Velocity_Dimensions is ( X, Y, Z );

   function Velocity(M: Moon; I: Velocity_Dimensions) return Integer is
   (
         case I is
            when X => M.Dx,
            when Y => M.Dy,
            when Z => M.Dz
   );

begin

   -- SECTION
   -- part 1

   -- SUBSECTION
   -- read initial state

   Open(F, In_File, Filename);
   for M of Jovian_Moons loop
      Read_Initial_Position(M);
   end loop;
   Close(F);

   -- SUBSECTION
   -- apply positions & velocities

   Put_Line("intial state:");
   for M of Jovian_Moons loop Put(M); New_Line; end loop; New_Line;

   for I in 1 .. Iterations_Part1 loop
      for M of Jovian_Moons loop Determine_Velocities(M); end loop;
      for M of Jovian_Moons loop Apply_Velocities(M); end loop;
   end loop;

   -- report results

   Put_Line("final state:");
   for M of Jovian_Moons loop Put(M); New_Line; end loop; New_Line;

   declare
      Result: Natural := 0;
   begin
      for M of Jovian_Moons loop Result := Result + Total_Energy(M); end loop;
      Put_Line("total energy is" & Result'Image);
   end;

   -- part 2

   -- SUBSECTION
   -- read initial state

   Open(F, In_File, Filename);
   for M of Jovian_Moons loop
      Read_Initial_Position(M);
   end loop;
   Close(F);

   -- SUBSECTION
   -- loop until we can extrapolate length using lcm

   Put_Line("intial state:");
   for M of Jovian_Moons loop Put(M); New_Line; end loop; New_Line;

   declare

      Iterations_Part2: Long_Long_Integer := 0;
      -- how many iterations we've done so far

      Stationary: array( Velocity_Dimensions ) of Boolean
      -- whether the i'th dimension has become stationary (all velocities 0)
            := ( others => False );

      Dimension_Period: array( Velocity_Dimensions ) of Long_Long_Integer
      -- the period of the i'th dimension
            := ( others => 0 );

   begin

      while ( for some Dim in Velocity_Dimensions => not Stationary(Dim) ) loop

         Iterations_Part2 := Iterations_Part2 + 1;

         -- perform the mechanics
         for M of Jovian_Moons loop Determine_Velocities(M); end loop;
         for M of Jovian_Moons loop Apply_Velocities(M); end loop;

         -- check for repetition in each dimension
         for I in Velocity_Dimensions loop
            if not Stationary(I) then
               Stationary(I)
                     := (
                         for all Name in Moon_Names
                         => Velocity(Jovian_Moons(Name), I) = 0
                        );
               if Stationary(I) then
                  Dimension_Period(I) := Iterations_Part2;
               end if;
            end if;
         end loop;

      end loop;

      Put_Line("finished after " & Iterations_Part2'Image & " iterations");

      declare Result: Long_Long_Integer := Dimension_Period'Reduce(Lcm, 1);
      begin

         for I in Velocity_Dimensions loop
            Put_Line(I'Image & ":" & Dimension_Period(I)'Image);
         end loop;

         -- multiply by 2 since we only checked for halfway
         Result := Result * 2;

         Put_Line(" the system repeats state after" & Result'Image & " turns");

      end;

   end;

end Main;
