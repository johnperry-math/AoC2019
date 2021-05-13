-- Advent of Code 2019, Day 8
--
-- John Perry
--
-- part 1: determine the layer with the fewest 0's, report number of 1's times
-- number of 2's
--
-- part 2: decode the image, where 0 is black, 1 is white, and 2 is transparent
-- (which means, move down to the next layer for data), then report the message

pragma Ada_2020;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Containers.Vectors;

procedure Main is

   -- SECTION
   -- input-related

   F: File_Type; -- input file

   Testing: constant Boolean := False;

   Filename: constant String
         := (
             if Testing then "/Users/user/common/Ada/AoC2019/Day8/example.txt"
             else "/Users/user/common/Ada/AoC2019/Day8/input.txt"
            );

   -- SECTION
   -- Space Image Format
   -- several 25x6 layers of digits

   Image_Width: constant := 25;
   Image_Height: constant := 6;

   type Layer_Array is array( 1 .. Image_Height, 1 .. Image_Width ) of Natural;

   package Raw_Data_Vectors is new Ada.Containers.Vectors
   (
    Element_Type => Layer_Array,
    Index_Type => Positive
   );

   Raw_Data: Raw_Data_Vectors.Vector;

   -- SECTION
   -- data for part 1

   Min_Zeros: Natural := 25 * 6;
   Layer_With_Min_Zeros: Positive := 1;

   -- SECTION
   -- function for part 2

   function Decode_Image return Layer_Array is
   -- decodes Raw_Data according to the rules stated above

      Result: Layer_Array;

   begin

      for I in 1 .. Image_Height loop
         for J in 1 .. Image_Width loop

            declare Desired_Layer: Positive := Raw_Data.First_Index;
            begin

               -- find a layer that has data for this pixel
               while Raw_Data(Desired_Layer)(I,J) = 2 loop
                  Desired_Layer := Desired_Layer + 1;
               end loop;
               Result(I,J) := Raw_Data(Desired_Layer)(I,J);

            end;

         end loop;
      end loop;

      return Result;
   end;

begin

   -- read data and do part of part 1

   Open(F, In_File, Filename);

   while not End_Of_File(F) loop
      declare
         Layer: Layer_Array;
         C: Character;
         Num_Zeros: Natural := 0;
      begin
         for I in 1 .. Image_Height loop
            for J in 1 .. Image_Width loop
               Get(F, C);
               Layer(I,J) := Character'Pos(C) - Character'Pos('0');
               if Layer(I,J) = 0 then Num_Zeros := Num_Zeros + 1; end if;
            end loop;
         end loop;
         Raw_Data.Append(Layer);
         if Num_Zeros < Min_Zeros then
            Min_Zeros := Num_Zeros;
            Layer_With_Min_Zeros := Raw_Data.Last_Index;
         end if;
      end;
   end loop;

   Close(F);

   -- finish part 1

   Put_Line("number of layers: " & Raw_Data.Length'Image);
   Put_Line("Minimum number of zeros: " & Min_Zeros'Image);
   Put_Line("occurs on layer " & Layer_With_Min_Zeros'Image);

   declare

      Num_1s, Num_2s: Natural := 0;
      Layer: Raw_Data_Vectors.Reference_Type
            renames Raw_Data(Layer_With_Min_Zeros);

   begin

      for I in 1 .. Image_Height loop
         for J in 1 .. Image_Width loop

            if Layer(I,J) = 1 then Num_1s := Num_1s + 1;
            elsif Layer(I,J) = 2 then Num_2s := Num_2s + 1;
            end if;

         end loop;
      end loop;

      Put("product of number of 1's and number of 2's: ");
      Put(Num_1s * Num_2s, 0); New_Line;

   end;

   -- part 2: decode image and report message

   declare Final_Image: Layer_Array := Decode_Image;
   begin

      for I in 1 .. Image_Height loop

         for J in 1 .. Image_Width loop
            Put( ( if Final_Image(I,J) = 0 then '.' else '#' ) );
         end loop;
         New_Line;

      end loop;

   end;

end Main;
