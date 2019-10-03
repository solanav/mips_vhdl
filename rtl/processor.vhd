--------------------------------------------------------------------------------
-- Procesador MIPS con pipeline curso Arquitectura 2019-2020
--
-- (INCLUIR AQUI LA INFORMACION SOBRE LOS AUTORES)
--
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity processor is
   port(
      Clk      : in  std_logic; -- Reloj activo en flanco subida
      Reset    : in  std_logic; -- Reset asincrono activo nivel alto
      -- Instruction memory
      IAddr    : out std_logic_vector(31 downto 0); -- Direccion Instr
      IDataIn  : in  std_logic_vector(31 downto 0); -- Instruccion leida
      -- Data memory
      DAddr    : out std_logic_vector(31 downto 0); -- Direccion
      DRdEn    : out std_logic;                     -- Habilitacion lectura
      DWrEn    : out std_logic;                     -- Habilitacion escritura
      DDataOut : out std_logic_vector(31 downto 0); -- Dato escrito
      DDataIn  : in  std_logic_vector(31 downto 0)  -- Dato leido
   );
end processor;

architecture rtl of processor is
   component reg_bank -- Banco de registros
      port (
         Clk   : in std_logic;                          -- Reloj activo en flanco de subida
         Reset : in std_logic;                          -- Reset asoncrono a nivel alto
         A1    : in std_logic_vector(4 downto 0);       -- Direccion para el puerto Rd1
         Rd1   : out std_logic_vector(31 downto 0);     -- Dato del puerto Rd1
         A2    : in std_logic_vector(4 downto 0);       -- Direccion para el puerto Rd2
         Rd2   : out std_logic_vector(31 downto 0);     -- Dato del puerto Rd2
         A3    : in std_logic_vector(4 downto 0);       -- Direccion para el puerto Wd3
         Wd3   : in std_logic_vector(31 downto 0);      -- Dato de entrada Wd3
         We3   : in std_logic                           -- Habilitacion de la escritura de Wd3
      );
   end component;

   component control_unit -- Unidad de control principal
      port (
         OpCode   : in  std_logic_vector (5 downto 0);
         Branch   : out std_logic;                      -- 1 = Ejecutandose instruccion branch
         Jump     : out std_logic;                      -- 1 = Ejecutandose instruccion jump
         MemToReg : out std_logic;                      -- 1 = Escribir en registro la salida de la mem.
         MemWrite : out std_logic;                      -- Escribir la memoria
         MemRead  : out std_logic;                      -- Leer la memoria
         ALUSrc   : out std_logic;                      -- 0 = oper.B es registro, 1 = es valor inm.
         ALUOp    : out std_logic_vector (2 downto 0);  -- Tipo operacion para control de la ALU
         RegWrite : out std_logic;                      -- 1=Escribir registro
         RegDst   : out std_logic                       -- 0=Reg. destino es rt, 1=rd
      );
   end component;

   component alu_control
      port (
         ALUOp  : in std_logic_vector (2 downto 0);     -- Codigo de control desde la unidad de control
         Funct  : in std_logic_vector (5 downto 0);     -- Campo "funct" de la instruccion
         ALUControl : out std_logic_vector (3 downto 0) -- Define operacion a ejecutar por la ALU
      );
   end component;

   component alu
      port (
         OpA     : in  std_logic_vector (31 downto 0);  -- Operando A
         OpB     : in  std_logic_vector (31 downto 0);  -- Operando B
         Control : in  std_logic_vector ( 3 downto 0);  -- Codigo de control=op. a ejecutar
         Result  : out std_logic_vector (31 downto 0);  -- Resultado
         ZFlag   : out std_logic                        -- Flag Z
      );
   end component;
begin

end architecture;
