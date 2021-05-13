-- Intcode compiler for Advent of Code, 2019
--
-- John Perry

pragma Ada_2020;

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Long_Long_Integer_Text_IO;
use Ada.Long_Long_Integer_Text_IO;

package body Intcode is

   -- SECTION
   -- opcodes

   subtype Opcode_Range is Integer range 1 .. 99;

   type Opcode is (
                   Add,
                   Mul,
                   Input,
                   Print,
                   Jump_If_True,
                   Jump_If_False,
                   Less_Than,
                   Equals,
                   Adj_Rel_Base,
                   Halt,
                   Data
                  );

   -- SECTION
   -- parameter modes

   Pos : constant := 0;
   Imm : constant := 1;
   Rel : constant := 2;

   -- SECTION
   -- performing opcodes

   -- SUBSECTION
   -- reading / writing input / output from an array rather than command line

   type Integer_Access is access all Integer;
   type Boolean_Access is access all Boolean;
   type Positive_Access is access all Positive;
   type Input_Type_Access is access all Input_Type;
   type Output_Type_Access is access all Output_Type;
   type Memory_Location_Access is access all Memory_Location;

   Program_Length: Positive;

   Inputs : array (Network_Range) of aliased Input_Type
      := ( others => ( others => 0 ) );

   Input_Array : Input_Type_Access := Inputs (0)'access;

   Input_Positions : array (Network_Range) of aliased Positive
      := ( others => Input_Array'Last + 1 );

   Input_Position : Positive_Access := Input_Positions (0)'access;

   Outputs : array ( Network_Range ) of aliased Output_Type
      := ( others => (others => 0) );

   Output_Array : Output_Type_Access := Outputs (0)'access;

   Output_Positions : array ( Network_Range ) of aliased Positive
      := ( others => Output_Array'First );

   Output_Position  : Positive_Access := Output_Positions (0)'access;

   Filled_Outputs      : array (Network_Range) of aliased Boolean
      := ( others => False );

   Filled_Output       : Boolean_Access := Filled_Outputs (0)'access;

   Resume_Locations, Resume_Relatives :
      array (Network_Range) of aliased Memory_Location
      := ( others => 0 );
   -- where in each memory to resume execution

   Resume_Location  : Memory_Location_Access := Resume_Locations (0)'access;
   -- where in memory to resume execution
   Resume_Relative  : Memory_Location_Access := Resume_Relatives (0)'access;
   -- where in memory to resume relative base
   Resume_Memories  : array (Network_Range) of aliased Storage
      := ( others => ( others => 99 ) );
   -- state of all memories when last execution completed
   -- (needed because scratch is a temporary variable)
   Resume_Memory : Storage_Access := Resume_Memories(0)'access;
   -- state of memory when last execution completed
   -- (needed because scratch is a temporary variable)

   Waiting_For_Inputs : array (Network_Range) of aliased Boolean
      := ( others => False );

   Waiting_For_Input : Boolean_Access := Waiting_For_Inputs(0)'access;
   -- this is set to true when input is expected from a buffer
   -- but the buffer is exhausted

   procedure Switch_To (Machine : Network_Range) is
   begin
      if Verbose then Put_Line("switching to" & Machine'Image); end if;
      Memory := Network (Machine)'access;
      Input_Array := Inputs (Machine)'access;
      Input_Position := Input_Positions (Machine)'access;
      Output_Array := Outputs (Machine)'access;
      Output_Position := Output_Positions (Machine)'access;
      Filled_Output := Filled_Outputs (Machine)'access;
      Resume_Location := Resume_Locations (Machine)'access;
      Resume_Relative := Resume_Relatives (Machine)'access;
      Resume_Memory := Resume_Memories (Machine)'access;
      Waiting_For_Input := Waiting_For_Inputs (Machine)'access;
   end Switch_To;

   function Report_Location return Memory_Location is ( Resume_Location.all );

   procedure Supply_Input (
                           Here : Input_Type;
                           Pos  : Positive := Input_Type'First
                          )
   is
   begin
      Input_Array.all := Here;
      Input_Position.all := Pos;
   end Supply_Input;

   function Has_Filled_Output return Boolean is ( Filled_Output.all );

   function Report_Output return Output_Type is ( Output_Array.all );

   procedure Reset_Output is
   begin
      Output_Position.all := Output_Array'First;
      Filled_Output.all := False;
   end Reset_Output;

   -- SUBSECTION
   -- types needed

   type Modes_Array is array ( Positive range <> ) of Integer;
   -- stores whether the parameter mode is immediate or at a position

   type Decoded_Instruction (N : Positive) is record
   -- a decoded instruction, containing all parameters,
   -- but not where to write to (so really all parameters but the last)
      Instruction : Opcode;
      Modes       : Modes_Array ( 1 .. N ) := ( others => 0 );
   end record;

   Invalid_Opcode : exception;

   function Determine_Number_Params (Instruction : Integer) return Positive is
   -- returns the number of parameters that Instruction requires
      (
        case Instruction mod 100 is
           when 5 | 6 => 2,
           when 1 | 2 | 7 | 8 => 3,
           when 3 | 4 | 9 | 99 => 1,
           when others => raise Invalid_Opcode with Instruction'Image
       );

   function Decipher_Opcode (Value : Opcode_Range) return Opcode is
   -- deciphers Value according to the valid Opcode's
      (
        case Value is
           when 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 => Opcode'Val (Value - 1),
           when 99 => Halt,
           when others
              => raise Invalid_Opcode with "bad opcode " & Value'Image
       );

   function Digit (N : Integer; D : Natural) return Natural is
   -- returns the Dth digit of N
      ( Natural ( ( (N - N mod 10 ** D ) - (N - N mod 10 ** (D + 1) ) ) / 10 ** D ) );


   function Decode_Modes (Instruction : Integer) return Decoded_Instruction is
   -- determines the instruction, the number of parameters it needs,
   -- and the parameter modes (Imm, Pos, etc.), then returns this information
      Result : Decoded_Instruction ( N => Determine_Number_Params (Instruction) );
   begin
      Result.Instruction := Decipher_Opcode (Instruction mod 100);
      for I in 1 .. Result.N loop
         Result.Modes (I) := Digit (Instruction, I + 1);
      end loop;
      return Result;
   end;
   -- the following should work, but there is a bug in GNAT
   --     ( (
   --             N           => Determine_Number_Params(Instruction),
   --             Instruction => Decipher_Opcode(Instruction mod 100),
   --             Modes       => (
   --                             for I in 1 .. Determine_Number_Params(Instruction)
   --                             => Digit(Instruction, I + 1)
   --                            )
   --     ) );

   function Is_Suspended_For_Input return Boolean is ( Waiting_For_Input.all );
   -- returns True iff the program was suspended to obtain input

   procedure Perform_Opcode (
                             Position      : in out Memory_Location;
                             Memory        : in out Storage;
                             Relative_Base : in out Memory_Location
                            )
      -- interpret opcode at Position in Memory
      -- will suspend operation if input buffer is exhausted

   is

      Opcode            : Integer := Integer ( Memory (Position) mod 100 );
      To_Do             : Decoded_Instruction := Decode_Modes (Integer (Memory (Position)));
      Param             : array ( 1 .. To_Do.N ) of Long_Long_Integer;
      Mode_Error        : exception;
      Write_Destination : Memory_Location;

   begin

      if Verbose then Put(Position'Image & " :" & To_Do.Instruction'Image); end if;

      -- obtain the parameter modes

      for I in 1 .. To_Do.N loop
         Param (I)
            := (
                case To_Do.Modes (I) is
                   when Imm => Memory (Position + I),
                   when Pos => Memory (Integer (Memory (Position + I))),
                   when Rel => Memory (Relative_Base + Integer (Memory (Position + I))),
                   when others
                      => raise Mode_Error
                         with "at " & Position'Image & ", " & I'Image
               );
         if Verbose then
            Put (To_Do.Modes (I)'Image);
            Put (Param (I)'Image);
         end if;
      end loop;
      if Verbose then New_Line; end if;

      -- if the last parameter is a writing destination (Add, Mul, ...), then
      -- we don't want the value at that location; we want the location itself

      Write_Destination
         := (
             case To_Do.Modes (To_Do.N) is
                when Imm => Position + To_Do.N,
                when Pos => Integer (Memory (Position + To_Do.N)),
                when Rel
                   => Relative_Base + Integer (Memory (Position + To_Do.N)),
                when others
                   => raise Mode_Error
                      with "at " & Position'Image & ", " & To_Do.N'Image
            );

      -- now perform the instruction

      case To_Do.Instruction is

      when Add =>
         Memory (Write_Destination) := Param (1) + Param (2);
         Position := Position + 4;

      when Mul =>
         Memory (Write_Destination) := Param (1) * Param (2);
         Position := Position + 4;

      when Input =>

         declare Input_Value : Long_Long_Integer;
         begin

            -- this one is tricky, because sometimes we want to use
            -- the command line, sometimes an input array;
            -- if we have exhausted the buffer, we suspend

            if Input_From_Command_Line then

               if not ASCII_Mode then
                  Get (Input_Value);
               else
                  -- in ASCII_Mode we're reading characters
                  declare Input_Character : Character;
                  begin
                     Get (Input_Character);
                     Input_Value := (
                                     if Input_Character = '-' then 10
                                     else Character'Pos (Input_Character)
                                    );
                  end;
               end if;
               Memory (Write_Destination) := Input_Value;
               Position := Position + 2;

            elsif Input_Position.all <= Input_Array'Last then

               Input_Value := Input_Array (Input_Position.all);
               Input_Position.all := Input_Position.all + 1;
               Memory (Write_Destination) := Input_Value;
               Position := Position + 2;

            else -- position is too large; suspend until new input ready

               Waiting_For_Input.all := True;

            end if;

         end;

      when Print =>

         if Output_To_Command_Line then

            if ASCII_Mode and then ( Param (1) <= Character'Pos (Character'Last_Valid) ) then
               Put (Character'Val (Param (1)));
            else Put (Param (1)'Image); New_Line;
            end if;

         else

            if Output_Position.all > Output_Array'Last then
               Output_Position.all := Output_Array'First;
            end if;

            Output_Array (Output_Position.all) := Param (1);
            Output_Position.all := Output_Position.all + 1;
            if Output_Position.all > Output_Array'Last then
               Filled_Output.all := True;
            end if;

         end if;

         Position := Position + 2;

      when Jump_If_True =>
         if Param (1) /= 0 then Position := Integer (Param (2));
         else Position := Position + 3;
         end if;

      when Jump_If_False =>
         if Param (1) = 0 then Position := Integer (Param (2));
         else Position := Position + 3;
         end if;

      when Less_Than =>
         if Param (1) < Param (2) then Memory (Write_Destination) := 1;
         else Memory (Write_Destination) := 0;
         end if;
         Position := Position + 4;

      when Equals =>
         if Param (1) = Param (2) then Memory (Write_Destination) := 1;
         else Memory (Write_Destination) := 0;
         end if;
         Position := Position + 4;

      when Adj_Rel_Base =>
         Relative_Base := Relative_Base + Integer (Param (1));
         Position := Position + 2;

      when Halt => null;

      when others => raise Data_Error with "bad opcode";

      end case;

      if Verbose then
         case To_Do.Instruction is
            when Add | Mul | Less_Than | Equals =>
               Put_Line (
                         "wrote" & Memory (Write_Destination)'image
                            & " to" & Write_Destination'Image
                        );
            when Jump_If_True | Jump_If_False =>
               Put_Line("jumped to" & Position'Image);
            when others => null;
         end case;
      end if;

   end Perform_Opcode;

   procedure Run_Program (Start : Memory_Location := 0) is

      Scratch_Memory : Storage
         := ( if Is_Suspended_For_Input then Resume_Memory.all else Memory.all );
      -- we copy it, so that we can reuse the original later
      Program_Counter : Memory_Location
         := ( if Is_Suspended_For_Input then Resume_Location.all else Start );
      Relative_Base : Memory_Location
         := (
             if Is_Suspended_For_Input then Resume_Relative.all
             else 0
            );

   begin

      Output_Array.all := ( others => 0 );
      Filled_Output.all := False;

      if Is_Suspended_For_Input then Waiting_For_Input.all := False; end if;

      while Scratch_Memory (Program_Counter) /= 99 and not Is_Suspended_For_Input
      loop
         Perform_Opcode ( Program_Counter, Scratch_Memory, Relative_Base );
      end loop;

      -- if we suspended, we need to remember where, and what was in memory
      if Is_Suspended_For_Input then
         Resume_Location.all := Program_Counter;
         Resume_Relative.all := Relative_Base;
      end if;

      Resume_Memory.all := Scratch_Memory;

   end Run_Program;

   procedure Step_Program is

      Program_Counter : Memory_Location := Resume_Location.all;
      Relative_Base : Memory_Location := Resume_Relative.all;

   begin

      -- unlike Run_Program, Step_Program does not copy the original program

      if Is_Suspended_For_Input then Waiting_For_Input.all := False; end if;

      if Memory (Program_Counter) /= 99 and not Is_Suspended_For_Input
      then
         Perform_Opcode ( Program_Counter, Memory.all, Relative_Base );
      end if;

      Resume_Location.all := Program_Counter;
      Resume_Relative.all := Relative_Base;

   end Step_Program;

   procedure Read_Program (Filename : String) is
      F : File_Type;
   begin

      Open (F, In_File, Filename);

      declare
         Address : Memory_Location := 0;
         Value   : Long_Long_Integer;
         C       : Character;
      begin
         while not End_Of_File (F) loop
            Get (F, Value);
            if not End_Of_File (F) then Get (F, C); end if; -- read comma
            Memory (Address) := Value;
            Address := Address + 1;
         end loop;
         Program_Length := Address - 1;
      end;

      Close (F);

   end Read_Program;

   function Run_Program_Report_Location (
                                         Start  : Memory_Location := 0;
                                         Report : Memory_Location := 0
                                        )
                                        return Long_Long_Integer
   is
   begin
      Run_Program (Start);
      return Resume_Memory (Report);
   end Run_Program_Report_Location;

   procedure Print_Disassembly is

      Position: Natural := 0;

   begin

      while Position < Program_Length loop

         declare
            Opcode      : Integer := Integer ( Memory (Position) mod 100 );
            Num_Params  : Positive := (
                                       case Opcode is
                                          when 5 | 6 => 2,
                                          when 1 | 2 | 7 | 8 => 3,
                                          when 3 | 4 | 9 | 99 => 1,
                                          when others => 1
                                      );
            Instruction : Decoded_Instruction (Num_Params);
            Line_No_Tab : Positive := Memory_Location'Width;
            Valid_Instruction : Boolean := True;

         begin

            Put (Position, Line_No_Tab); Put (' ');

            for I in 1 .. Num_Params loop
               if Memory(Position) < Long_Long_Integer(Integer'Last) then
                  Valid_Instruction := Valid_Instruction
                     and ( Digit( Integer (Memory (Position) ), I + 1) in 0 .. 2 );
               else
                  Valid_Instruction := False;
               end if;
            end loop;

            case Opcode is
               when 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 =>
                  if Valid_Instruction then
                     Instruction := Decode_Modes (Integer (Memory (Position)));
                     Put ( (
                           case Instruction.Instruction is
                              when Add => "ADD  ",
                              when Mul => "MUL  ",
                              when Input => "INP  ",
                              when Print => "OUT  ",
                              when Jump_If_True => "JMTRU",
                              when Jump_If_False => "JMFAL",
                              when Less_Than => "LTHAN",
                              when Equals => "EQUAL",
                              when Adj_Rel_Base => "ADJRB",
                              when Halt => "HALT ",
                              when others => "DATA "
                          ) & " " );
                     for I in 1 .. Instruction.N loop
                        Put ( (
                              case Instruction.Modes (I) is
                                 when Rel => "r",
                                 when Imm => "i",
                                 when Pos => "a",
                                 when others => "?"
                             ) );
                        Put ( Memory (Position + I)'Image ); Put (' ');
                     end loop;
                     New_Line;
                     Position := Position + Instruction.N + 1;
                  else
                     Put_Line ("DATA " & Memory (Position)'Image);
                     Position := Position + 1;
                  end if;
               when 99 =>
                  Put_Line ("HALT");
                  Position := Position + 1;
               when others =>
                  Put_Line ("DATA " & Memory (Position)'Image);
                  Position := Position + 1;
            end case;

         end;

      end loop;

   end Print_Disassembly;

end Intcode;
