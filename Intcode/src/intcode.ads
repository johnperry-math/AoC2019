-- @summary Intcode compiler for Advent of Code, 2019
--
-- @author John Perry
--
-- @description
-- Since I've now had to write two Intcode programs, I figured
-- this was as good a time as any to work on packaging.
--
-- As time went on, I typically had to modify the program slightly.
--
-- This has been verified against Day 2 (main_alternate)
-- and Days 5, 7, 9, 11, 13, 15, 17, 19, 21, 23.

generic

   Max_Location : Positive := 200;
   -- Max_Location indicates how large the memory should be

   Input_From_Command_Line : Boolean := True;
   -- whether input should come from the command line or from a buffer;
   --  if from an array, use Input_Size;
   -- when buffer has been exhausted, program will suspend

   Output_To_Command_Line : Boolean := True;
   -- whether output should go to the command line or to a buffer;
   --  when buffer has been exhausted, program will NOT suspend, but will start
   -- from beginning

   Input_Size   : Natural := 1;
   -- Input_Size: size of the input buffer

   Output_Size  : Natural := 1;
   -- Output_Size: size of the output buffer

   ASCII_Mode   : Boolean := False;
   -- whether command line version should be read and written as ASCII data
   --  this will not work when supplying input strings!

   Num_Computers : Positive := 1;
   -- the number of computers to use in a network
   --  (should have done this on Day 7 (?amplifiers) but put it off until Day 23)

package Intcode is

   Verbose : Boolean := False;
   -- if set to true, will output a lot of possibly useful information

   subtype Memory_Location is Integer range 0 .. Integer (Max_Location);
   -- acceptable memory locations

   function Report_Location return Memory_Location;
   -- @summary the location of the current machine's program counter

   type Storage is array ( Memory_Location ) of Long_Long_Integer;
   -- how memory is stored

   type Storage_Access is access all Storage;
   -- access to a machine's memory

   subtype Network_Range is Natural range 0 .. Num_Computers - 1;
   -- range of the machines in use

   Network : array (Network_Range) of aliased Storage
      := ( others => ( others => 0 ) );
   -- memory array of machines in use by the droid
   --  (on day 23 we suddenly need a bunch of them)

   Memory : Storage_Access := Network (0)'access;
   -- memory used by the current machine

   type Input_Type is array ( 1 .. Input_Size ) of Long_Long_Integer;
   -- use this to supply input when you don't want the command line

   type Output_Type is array ( 1 .. Output_Size ) of Long_Long_Integer;
   -- use this to supply output when you don't want the command line

   procedure Switch_To ( Machine : Network_Range);
   -- @summary switches to the indicated Machine in the Network

   procedure Read_Program (Filename : String);
   -- @summary read the program and set up memory
   --  the program is specified in the format "integer,integer,..."

   procedure Supply_Input (
                           Here : Input_Type;
                           Pos  : Positive := Input_Type'First
                          );
   -- @summary supply an input array for the program to use
   --
   -- @param Here the input data
   -- @param Pos the first valid position in Here
   --
   --  this is important: place your inputs in order,
   -- but starting from at the end of Here, not from its beginning;
   -- this way, only valid inputs are read
   --
   -- example: when we can have up to 3 inputs, but only have two values 1, 3,
   -- set Here to ( anything, 1, 3 )

   function Has_Filled_Output return Boolean;
   -- @summary does the computer have new output to report?
   -- @return True if so, False, otherwise

   function Report_Output return Output_Type;
   -- @summary read from the array for the program
   -- @return the entire output array

   procedure Reset_Output;
   -- @summary clear the output buffer when it's completely read

   function Is_Suspended_For_Input return Boolean;
   -- @summary whether the function has been suspended for input

   procedure Run_Program (Start : Memory_Location := 0);
   -- @summary runs the program, starting at the given memory location
   -- @param Start where to start the program
   --
   -- 
   -- if the program is suspended, it will start at the last location
   -- regardless of the value of Start
   -- this procedure always copies the original program so that you can rerun
   -- the original later if desired
   --
   -- @exception Invalid_Opcode raised if machine encounters an invalid opcode
   -- @exception Mode_Error may be raised if a parameter has an incorrect mode

   procedure Step_Program;
   -- @summary runs only one opcode, starting at the given memory location
   --  unlike Run_Program, Step_Program does not copy the original
   -- program, so you cannot rely on it remaining unchanged after execution

   function Run_Program_Report_Location (
                                         Start  : Memory_Location := 0;
                                         Report : Memory_Location := 0
                                        ) return Long_Long_Integer;
   -- @summary runs the program
   -- @param Start where to start the program
   -- @param Report memory location to report when program terminates
   -- @return value of memory at Report

   procedure Print_Disassembly;
   -- @summary prints a disassembly of the intcode to standard output

end Intcode;
