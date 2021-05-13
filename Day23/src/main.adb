-- Advent of Code 2019, Day 23
--
-- John Perry
--
-- Intcode again
--
-- part 1: network 50 Intcode computers to communicate with each other.
-- motivated me to implement aliased types and use access, I think for the first
-- time when doing Advent of Code. That required me to relearn aliased & access!
--
-- part 2: add a 51st computer, called the NAT, which monitors for when the
-- other computers are idle, at which point it sends the last message it
-- received to the first one.
--
-- I had difficulty with part 1, because I misunderstood how the Intcode program
-- would read the input. I supplied both inputs at once, then cleared the input.
-- That caused the machine to lose its second input. After discovering the
-- problem and thinking about it a little (in part by comparing to the output of
-- someone else's working solution, in part by rereading the problem) I realized
-- how to fix it. Then I realized that I had already set up the Intcode machines
-- to handle cases where input sizes were not always the same, so I reverted to
-- my original code and abstained from clearing input after supplying it. Works
-- great!
--
-- I had difficulty with part 2, as well -- not as much as part 1, but more than
-- I should have. One problem was that I misread the directions: I tried to
-- return the first value returned twice, rather than the first value returned
-- twice IN A ROW. The last mistake I fixed was determining when the network was
-- idle. I think I had that implemented correctly at first, but I had definitely
-- misimplemented the NAT transmission conditions at one point, and the wording
-- is a bit funny ("continuously trying to receive packets without sending
-- packets"). If they're trying to receive, why would they be sending? Well, OK,
-- boss, if you say so. That extra condition led me astray, and I misimplemented
-- the check for not sending, and I think I misdiagnosed the problem and broke
-- the inputs.
--
-- Another thing that held me up on part 2 was the fact that a correct solution
-- actually takes quite a while. I thought I was getting the wrong answer when
-- 2-3 seconds pass and it's still running. That's kind of unusual for AoC
-- puzzles in my experience, so I suspect there's a better way to implement this
-- - for instance, not using round-robin.

pragma Ada_2020;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Ordered_Sets;
use all type Ada.Containers.Count_Type;

with Intcode;

procedure Main is

   -- SECTION
   -- input-related

   Filename : constant String
      := "/Users/user/common/Ada/AoC2019/Day23/input.txt";

   -- SECTION
   -- Intcode computers for part 1

   Output_Length : constant := 3; -- destination, X, Y

   package NIC is new Intcode
      (
       Max_Location            => 10000,
       Output_To_Command_Line  => False,
       Input_From_Command_Line => False,
       Input_Size              => 2,
       Output_Size             => 3,
       Num_Computers           => 50
      );

   package NIC_Queue_Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces
      (
       Element_Type => NIC.Input_Type
      );

   package NIC_Queues_Package is new Ada.Containers.Unbounded_Synchronized_Queues
      (
       Queue_Interfaces => NIC_Queue_Interfaces
      );

   NIC_Inputs : array ( NIC.Network_Range ) of NIC_Queues_Package.Queue;

   Result_1 : Long_Long_Integer;
   Result_1_Valid : Boolean := False;

   -- SECTION
   -- NAT device for part 2

   NAT_Packet : NIC.Input_Type;

   Result_2 : Long_Long_Integer := 0;

   NIC_Is_Waiting : array (NIC.Network_Range) of Boolean := ( others => False );
   Nat_Has_Data : Boolean := False;

begin

   -- PARTS 1 AND 2 SIMULTANEOUSLY
   -- read program & initialize intcode computers

   for I in NIC.Network'Range loop
      NIC.Switch_To (I);
      NIC.Read_Program (Filename);
      Nic.Supply_Input ( (-1, Long_Long_Integer (I)), NIC.Input_Type'Last );
   end loop;

   --  NIC.Print_Disassembly;

   -- main loop

   NIC.Verbose := False;

   Main_Loop :
   loop

      -- process each computer in round robin
      for I in NIC.Network'Range loop

         NIC.Switch_To (I);

         -- set up input if needed
         declare Was_Waiting_For_Input : Boolean := NIC.Is_Suspended_For_Input;
         begin

            if Was_Waiting_For_Input then
               if NIC_Inputs (I).Current_Use = 0 then
                  NIC.Supply_Input ( ( others => -1 ), Nic.Input_Type'Last );
               else
                  declare Packet : Nic.Input_Type;
                  begin
                     NIC_Inputs (I).Dequeue (Packet);
                     NIC.Supply_Input (Packet, Nic.Input_Type'First);
                  end;
               end if;
            end if;

         end;

         NIC.Step_Program;

         -- process output if ready

         if NIC.Has_Filled_Output then

            declare
               Message : NIC.Output_Type := NIC.Report_Output;
               J       : Integer := Message'First;
            begin

               NIC.Reset_Output;

               if Message (J) = 255 then

                  if not Result_1_Valid then
                     Result_1 := Message (J + 2);
                     Result_1_Valid := True;
                  end if;
                  NAT_Packet := ( Message (J + 1), Message (J + 2) );
                  Nat_Has_Data := True;

               else

                  NIC_Inputs (Integer (Message (J))).Enqueue
                     (
                         ( Message (J + 1), Message (J + 2) )
                     );
                  NIC_Is_Waiting (Integer (Message (J))) := False;

               end if;

            end;

         end if;

         -- check if waiting for input

         if NIC.Is_Suspended_For_Input and then NIC_Inputs (I).Current_Use = 0
         then
            NIC_Is_Waiting (I) := True;
         end if;

      end loop;

      -- if all NIC's idle, operate NAT

      if ( for all I in NIC_Inputs'Range => NIC_Is_Waiting (I) )
         and then Nat_Has_Data -- don't want to send too early (this can happen)
      then
         if Result_2 = NAT_Packet (NAT_Packet'Last) then
            exit Main_Loop;
         end if;
         NIC_Inputs (0).Enqueue ( NAT_Packet );
         Result_2 := NAT_Packet (NAT_Packet'Last);
         Nat_Has_Data := False;
      end if;

   end loop Main_Loop;

   Put_Line ("first answer is" & Result_1'Image);
   Put_Line ("second answer is" & Result_2'Image);

end Main;
