--------------------------------------------------------------------------------
-- Bloque de control para la ALU. Arq0 2019-2020.
--
-- (INCLUIR AQUI LA INFORMACION SOBRE LOS AUTORES, Quitar este mensaje)
--
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity alu_control is
   port (
      -- Entradas:
      ALUOp  : in std_logic_vector (2 downto 0); -- Codigo de control desde la unidad de control
      Funct  : in std_logic_vector (5 downto 0); -- Campo "funct" de la instruccion
      -- Salida de control para la ALU:
      ALUControl : out std_logic_vector (3 downto 0) -- Define operacion a ejecutar por la ALU
   );
end alu_control;

architecture rtl of alu_control is

   -- Tipos
   subtype t_funct is std_logic_vector (5 downto 0);
   subtype t_aluControl is std_logic_vector (3 downto 0);
   subtype t_aluOp  is std_logic_vector (2 downto 0);

   -- Instrucciones funct
   constant F_ADD : t_funct := "100000";
   constant F_AND : t_funct := "100100";
   constant F_OR  : t_funct := "100101";
   constant F_SUB : t_funct := "100010";
   constant F_XOR : t_funct := "100110";

   -- Codigos de control:
   constant ALU_OR   : t_aluControl := "0111";
   constant ALU_NOT  : t_aluControl := "0101";
   constant ALU_XOR  : t_aluControl := "0110";
   constant ALU_AND  : t_aluControl := "0100";
   constant ALU_SUB  : t_aluControl := "0001";
   constant ALU_ADD  : t_aluControl := "0000";
   constant ALU_SLT  : t_aluControl := "1010";
   constant ALU_S16  : t_aluControl := "1101";
   constant ALU_ERR  : t_aluControl := "1111";

   -- Operacion a realizar por la alu
   constant ALUC_RTYPE : t_aluOp := "010";
   constant ALUC_ADD   : t_aluOp := "000";
   constant ALUC_SUB   : t_aluOp := "001";
   constant ALUC_S16   : t_aluOp := "100";
   constant ALUC_SLT   : t_aluOp := "101";
   constant ALUC_ERR   : t_aluOp := "111";

begin
   process(ALUOp, Funct)
   begin
      case ALUOp is
         when ALUC_RTYPE => -- R-Type
            case Funct is
               when F_ADD  => ALUControl <= ALU_ADD; -- ADD
               when F_AND  => ALUControl <= ALU_AND; -- AND
               when F_OR   => ALUControl <= ALU_OR;  -- OR
               when F_SUB  => ALUControl <= ALU_SUB; -- SUB
               when F_XOR  => ALUControl <= ALU_XOR; -- XOR
               when others => ALUControl <= ALU_ERR; -- ERROR
            end case;
         when ALUC_ADD => ALUControl <= ALU_ADD; -- LW, SW, ADDI, J
         when ALUC_SUB => ALUControl <= ALU_SUB; -- BEQ
         when ALUC_S16 => ALUControl <= ALU_S16; -- LUI
         when ALUC_SLT => ALUControl <= ALU_SLT; -- SLTI
         when others   => ALUControl <= ALU_ERR; -- ERROR
      end case;
   end process;
end architecture;
