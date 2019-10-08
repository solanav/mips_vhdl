--------------------------------------------------------------------------------
-- Unidad de control principal del micro. Arq0 2019-2020
--
-- (INCLUIR AQUI LA INFORMACION SOBRE LOS AUTORES)
--
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity control_unit is
   port (
      -- Entrada = codigo de operacion en la instruccion:
      OpCode   : in  std_logic_vector (31 downto 0);
      -- Seniales para el PC
      Branch   : out std_logic;                      -- 1 = Ejecutandose instruccion branch
      Jump     : out std_logic;                      -- 1 = Ejecutandose instruccion jump
      -- Seniales relativas a la memoria
      MemToReg : out std_logic;                      -- 1 = Escribir en registro la salida de la mem.
      MemWrite : out std_logic;                      -- Escribir la memoria
      MemRead  : out std_logic;                      -- Leer la memoria
      -- Seniales para la ALU
      ALUSrc   : out std_logic;                      -- 0 = oper.B es registro, 1 = es valor inm.
      ALUOp    : out std_logic_vector (2 downto 0);  -- Tipo operacion para control de la ALU
      -- Seniales para el GPR
      RegWrite : out std_logic;                      -- 1=Escribir registro
      RegDst   : out std_logic                       -- 0=Reg. destino es rt, 1=rd
   );
end control_unit;

architecture rtl of control_unit is

   -- Tipo para los codigos de operacion:
   subtype t_opCode is std_logic_vector (5 downto 0);
   subtype t_aluOp  is std_logic_vector (2 downto 0);

   -- Codigos de operacion para las diferentes instrucciones:
   constant OP_RTYPE : t_opCode := "000000";
   constant OP_BEQ   : t_opCode := "000100";
   constant OP_SW    : t_opCode := "101011";
   constant OP_LW    : t_opCode := "100011";
   constant OP_LUI   : t_opCode := "001111";
   constant OP_ADDI  : t_opCode := "001000";
   constant OP_SLT   : t_opCode := "101010";
   constant OP_SLTI  : t_opCode := "001010";
   constant OP_J     : t_opCode := "000010";
   constant OP_NOP   : t_opCode := "00000000000000000000000000000000"

   -- Operacion a realizar por la alu
   constant ALUC_RTYPE : t_aluOp := "010";
   constant ALUC_ADD   : t_aluOp := "000";
   constant ALUC_SUB   : t_aluOp := "001";
   constant ALUC_S16   : t_aluOp := "100";
   constant ALUC_SLT   : t_aluOp := "101";
   constant ALUC_ERR   : t_aluOp := "111";

begin
   process(OpCode)
   begin
      case OpCode(31 downto 26) is
         when OP_RTYPE => -- R-Type [OK]
			if OpCode = OP_NOP then -- Caso todo 0's NOP
				RegDst   <= '0';
				AluSrc   <= '0';
				MemToReg <= '0';
				RegWrite <= '0';
				MemRead  <= '0';
				MemWrite <= '0';
				Branch   <= '0';
				Jump     <= '0';
				AluOP    <= ALUC_ERR;
			else
				RegDst   <= '1';
				AluSrc   <= '0';
				MemToReg <= '0';
				RegWrite <= '1';
				MemRead  <= '0';
				MemWrite <= '0';
				Branch   <= '0';
				Jump     <= '0';
				AluOP    <= ALUC_RTYPE;
			end if;
         when OP_LW => -- LW [OK]
            RegDst   <= '0';
            AluSrc   <= '1';
            MemToReg <= '1';
            RegWrite <= '1';
            MemRead  <= '1';
            MemWrite <= '0';
            Branch   <= '0';
            Jump     <= '0';
            AluOP    <= ALUC_ADD;
         when OP_SW => -- SW [OK]
            RegDst   <= '0';
            AluSrc   <= '1';
            MemToReg <= '0';
            RegWrite <= '0';
            MemRead  <= '0';
            MemWrite <= '1';
            Branch   <= '0';
            Jump     <= '0';
            AluOP    <= ALUC_ADD;
         when OP_BEQ => -- BEQ [OK]
            RegDst   <= '0';
            AluSrc   <= '0';
            MemToReg <= '0';
            RegWrite <= '0';
            MemRead  <= '0';
            MemWrite <= '0';
            Branch   <= '1';
            Jump     <= '0';
            AluOP    <= ALUC_SUB;
         when OP_LUI => -- LUI
            RegDst   <= '0';
            AluSrc   <= '1';
            MemToReg <= '0';
            RegWrite <= '1';
            MemRead  <= '0';
            MemWrite <= '0';
            Branch   <= '0';
            Jump     <= '0';
            AluOP    <= ALUC_S16;
         when OP_ADDI => -- ADDI
            RegDst   <= '0';
            AluSrc   <= '1';
            MemToReg <= '0';
            RegWrite <= '1';
            MemRead  <= '0';
            MemWrite <= '0';
            Branch   <= '0';
            Jump     <= '0';
            AluOP    <= ALUC_ADD;
         when OP_SLTI => -- SLTI
            RegDst   <= '0';
            AluSrc   <= '1';
            MemToReg <= '0';
            RegWrite <= '1';
            MemRead  <= '0';
            MemWrite <= '0';
            Branch   <= '0';
            Jump     <= '0';
            AluOP    <= ALUC_SLT;
         when OP_J => -- J
            RegDst   <= '0';
            AluSrc   <= '1';
            MemToReg <= '0';
            RegWrite <= '0';
            MemRead  <= '0';
            MemWrite <= '0';
            Branch   <= '0';
            Jump     <= '1';
            AluOP    <= ALUC_ADD; 
         when others => -- ERROR
            RegDst   <= '0';
            AluSrc   <= '0';
            MemToReg <= '0';
            RegWrite <= '0';
            MemRead  <= '0';
            MemWrite <= '0';
            Branch   <= '0';
            AluOP    <= ALUC_ERR;
      end case;
   end process;
end architecture;
