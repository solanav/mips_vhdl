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
   
   -- reg_bank
   signal P_A1  : std_logic_vector(4 downto 0);
   signal P_Rd1 : std_logic_vector(31 downto 0);
   signal P_A2  : std_logic_vector(4 downto 0);
   signal P_Rd2 : std_logic_vector(31 downto 0);
   signal P_A3  : std_logic_vector(4 downto 0);
   signal P_Wd3 : std_logic_vector(31 downto 0);
   signal P_We3 : std_logic;
   
   -- control_unit
   signal P_OpCode   : std_logic_vector(31 downto 0);	
   signal P_Branch   : std_logic;
   signal P_Jump     : std_logic;
   signal P_MemToReg : std_logic;
   signal P_MemWrite : std_logic;
   signal P_MemRead  : std_logic;
   signal P_ALUSrc   : std_logic;
   signal P_ALUOp    : std_logic_vector(2 downto 0);
   signal P_RegWrite : std_logic;
   signal P_RegDst   : std_logic;
   
   -- alu_control
	signal ALUOp      : std_logic_vector(2 downto 0);
	signal Funct      : std_logic_vector(5 downto 0);
	signal ALUControl : std_logic_vector(3 downto 0);
	
	-- alu
   	signal P_OpA     : std_logic_vector(31 downto 0);
    signal P_OpB     : std_logic_vector(31 downto 0);
    signal P_Control : std_logic_vector(3 downto 0);
    signal P_Result  : std_logic_vector(31 downto 0);
    signal P_ZFla    : std_logic;
   
   signal PC_ADD4 : std_logic_vector(31 downto 0);
   
   signal ALURESULT_ADD4 : std_logic_vector(31 downto 0);
   
   -- sign extend out
   signal SIGN_EXTEND_OUT :	std_logic_vector(31 downto 0);
   
   
begin
	
	-- sumador despues PC
	PC_ADD4 <= IAddr + 4;
	
	
	-- sumador ALU Result
	ALURESULT_ADD4 <= PC_ADD4 + (SIGN_EXTEND_OUT sll 2);
	
	-- Sign extend
	with IDataIn(15) select
		SIGN_EXTEND_OUT(31 downto 0) <= "0000000000000000" & IDataIn when "0",
										"1111111111111111" & IDataIn when "1";


	-- mapeo de componentes a las seniales
	u1: reg_bank PORT MAP
	(
		 Clk   =>  Clk,
         Reset =>  Reset,
         A1    =>  P_A1,
         Rd1   =>  P_Rd1,
         A2    =>  P_A2,
         Rd2   =>  P_Rd2,
         A3    =>  P_A3,
         Wd3   =>  P_Wd3,
         We3   =>  P_We3  
	);
	
	u2: control_unit PORT MAP
	(
		 OpCode   => P_OpCode,	
         Branch   => P_Branch,
         Jump     => P_Jump,
         MemToReg => P_MemToReg,
         MemWrite => P_MemWrite,
         MemRead  => P_MemRead,
         ALUSrc   => P_ALUSrc,
         ALUOp    => P_ALUOp,
         RegWrite => P_RegWrite,
		 RegDst   => P_RegDst  
	);
	
	u3: alu_control PORT MAP
	(
		 ALUOp      => P_ALUOp,
		 Funct      => P_Funct,
         ALUControl => P_ALUControl
	);
	
	u4: alu PORT MAP
	(
		 OpA     => P_OpA,
         OpB     => P_OpB,
         Control => P_Control,
         Result  => P_Result,
         ZFlag   => P_ZFlag  
	);
	
end architecture;
