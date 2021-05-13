-- Advent of Code 2019, Day 6
--
-- John Perry
--
-- part 1: determine the number of (direct or indirect) orbits in a system
--
-- part 2: determine the number of orbital transfers needed to get from YOU
-- to SAN

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Containers.Hashed_Maps;

procedure Main is

   -- SECTION
   -- input-related

   F: File_Type; -- input file

   Testing: constant Boolean := False;

   Filename: constant String
         := (
             if Testing then "/Users/user/common/Ada/AoC2019/Day6/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day6/input.txt"
            );

   -- SECTION
   -- information on the orbital system

   subtype Name_String is String(1..3);

   type Space_Object is record
   -- the main object structure; we indicate what it orbits,
   -- and how many objects it indirectly orbits
      Directly_Orbits: Name_String;
      Indirectly_Orbits: Natural := 0;
   end record;

   function Space_Object_Hash(Name: Name_String) return Ada.Containers.Hash_Type
   -- base 100 hashing
   is
   (
      Ada.Containers.Hash_Type(
         Character'Pos(Name(1)) * 100 * 100
            + Character'Pos(Name(2)) * 100 + Character'Pos(Name(3))
      )
   );

   package All_Objects is new Ada.Containers.Hashed_Maps
   -- map of all objects in the orbital system
   (
    Key_Type => Name_String,
    Element_Type => Space_Object,
    Hash => Space_Object_Hash,
    Equivalent_Keys => "="
   );

   Orbital_System: All_Objects.Map;

   COM: constant Space_Object
   -- the Center Of Mass
         := (
             Directly_Orbits => "COM", -- directly orbits nothing, or "itself"
             Indirectly_Orbits => 0
            );

   procedure Determine_Indirect_Orbits(Name: Name_String) is
   -- counts the number of objects Name orbits indirectly
   -- this function is recursive and the descent stops at COM
      Obj: All_Objects.Reference_Type := Orbital_System.Reference(Name);
   begin
      if Obj.Directly_Orbits /= "COM" or else Name /= "COM" then
         if Obj.Indirectly_Orbits = 0 then
            Determine_Indirect_Orbits(Obj.Directly_Orbits);
         end if;
         Obj.Indirectly_Orbits
               := Orbital_System(Obj.Directly_Orbits).Indirectly_Orbits + 1;
      end if;
   end Determine_Indirect_Orbits;

   function Exists_Path_To(Destination: Name_String) return Boolean is
   -- True if and only if there is a path from YOU to Destination
      Can_Reach: Space_Object := Orbital_System("YOU");
   begin
      while Can_Reach.Directly_Orbits /= "COM"
            and Can_Reach.Directly_Orbits /= Destination
      loop
         Can_Reach := Orbital_System(Can_Reach.Directly_Orbits);
      end loop;
      return Can_Reach.Directly_Orbits /= "COM";
   end Exists_Path_To;

   function Orbital_Transfers_Required return Natural is
   -- determines the number of orbital transfers required to get from YOU to SAN
      Result: Natural := 0;
      Found: Boolean := False;
      Currently_Seeking: Name_String := "SAN";
   begin
      -- first find a point of common intersection
      while not Found loop
         exit when Exists_Path_To(Currently_Seeking);
         Currently_Seeking := Orbital_System(Currently_Seeking).Directly_Orbits;
      end loop;
      -- now count the transfers:
      -- it will equal the sum of indirect orbits for you and for santa,
      -- less the number of object counted twice in those indirect orbits,
      -- which will includes the common point of intersect
      Result
            := Orbital_System("YOU").Indirectly_Orbits
                  + Orbital_System("SAN").Indirectly_Orbits
                  - 2 * Orbital_System(Currently_Seeking).Indirectly_Orbits
                  - 2; -- removes Currently_Seeking
      return Result;
   end Orbital_Transfers_Required;

begin

   Orbital_System.Include( "COM", Com);

   -- read the orbital information

   Open(F, In_File, Filename);

   while not End_Of_File(F) loop
      declare
         S: String := Get_Line(F);
         Named: Name_String := S(S'First + 4 .. S'First + 6);
         Orbits: Name_String := S(S'First .. S'First + 2);
         The_Object: Space_Object
               := ( Directly_Orbits => Orbits, Indirectly_Orbits => <> );
      begin
         Orbital_System.Include(Named, The_Object);
      end;
   end loop;

   Close(F);

   -- part 1

   -- first we compute the indirect orbits of each object

   declare C: All_Objects.Cursor := Orbital_System.First;
   begin
      while All_Objects."/="(C, All_Objects.No_Element) loop
         Determine_Indirect_Orbits( All_Objects.Key(C) );
         All_Objects.Next(C);
      end loop;
   end;

   -- now we sum them together

   declare Total_Orbits: Natural := 0;
   begin
      for Obj of Orbital_System loop
         Total_Orbits := Total_Orbits + Obj.Indirectly_Orbits;
      end loop;
      Put_Line("total orbits: " & Total_Orbits'Image);
   end;

   -- part 2

   Put("orbital transfers required: ");
   Put(Orbital_Transfers_Required, 0);
   New_Line;

end Main;
