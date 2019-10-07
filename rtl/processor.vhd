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
   signal P_Rd1 : std_logic_vector(31 downto 0);
   signal P_Rd2 : std_logic_vector(31 downto 0);
   signal P_A3  : std_logic_vector(4 downto 0);
   
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
   
   signal ALURESULT_ADD : std_logic_vector(31 downto 0);
   
   -- sign extend out
   signal SIGN_EXTEND_OUT :	std_logic_vector(31 downto 0);
   
   -- mux_out
   signal MUX_OUT :	std_logic_vector(31 downto 0);
   
   -- instruction memory
   signal INSTRUCTION_MEMORY : std_logic_vector(31 downto 0);
   
   -- WRITE_REGISTER_MUX
   signal WRITE_REGISTER_MUX : std_logic_vector(4 downto 0);
   
   -- WRITE_DATA_MUX
   signal WRITE_DATA_MUX : std_logic_vector(31 downto 0);
   
   -- MUX_ALU_IN
   signal MUX_ALU_IN : std_logic_vector(31 downto 0);

   -- IAddr_SIGNAL
   signal IAddr_SIGNAL : std_logic_vector(31 downto 0);

   -- P_ZFlag
   signal P_ZFlag : std_logic;
   
	-- P_ALUControl
	signal P_ALUControl : std_logic_vector(3 downto 0);
	
	-- pipeline_enables
	signal E_IFID : std_logic;
	signal E_IDEX : std_logic;
	signal E_EXMEM : std_logic;
	signal E_MEMWB : std_logic;
	
	-- pipelines IFID
	signal PC_ADD4_IFID : std_logic_vector(31 downto 0);
	signal INSTRUCTION_MEMORY_IFID : std_logic_vector(31 downto 0);


	-- pipelines IDEX
	signal PC_ADD4_IDEX : std_logic_vector(31 downto 0);
	
	
	
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
begin
	
	process(Clk, Reset)
	begin
	if Reset = '1' then
		IAddr_SIGNAL <= (others => '0');
	elsif rising_edge(Clk) then
		IAddr_SIGNAL <= MUX_OUT;
	end if;
	end process;

	-- sumador despues PC
	PC_ADD4 <= IAddr_SIGNAL + 4;
	
	
	-- sumador ALU Result
	ALURESULT_ADD <= PC_ADD4 + (SIGN_EXTEND_OUT(29 downto 0) & "00" );
	
	-- Sign extend
	with INSTRUCTION_MEMORY_IFID(15) select
		SIGN_EXTEND_OUT(31 downto 0) <= "0000000000000000" & INSTRUCTION_MEMORY_IFID(15 downto 0) when '0',
						"1111111111111111" & INSTRUCTION_MEMORY_IFID(15 downto 0) when '1',
						"1111111111111111" & INSTRUCTION_MEMORY_IFID(15 downto 0) when others ;

	
	-- mux
	with (P_Branch and P_ZFlag) select
		MUX_OUT <= PC_ADD4 when '0',
			   ALURESULT_ADD when '1',
			   ALURESULT_ADD when others;
	
	-- pc
	process(Clk, Reset) 
	begin
		if Reset = '1' then
			IAddr <= (others => '0');	
		elsif rising_edge(Clk) then
			IAddr <= MUX_OUT;
		end if;
	end process;
	
	-- instruction memory
	INSTRUCTION_MEMORY <= IDataIn;
	
	-- WRITE_REGISTER_MUX
	with P_RegDst select
		WRITE_REGISTER_MUX <= IDataIn(20 downto 16) when '0',
					IDataIn(15 downto 11) when '1', 
					IDataIn(15 downto 11) when others;
	-- MUX_ALU_IN
	with P_ALUSrc select
		MUX_ALU_IN <= P_Rd2 when '0',
					  SIGN_EXTEND_OUT when '1',
					SIGN_EXTEND_OUT when others;
	
	-- data memory
	DAddr <= P_Result;
	DDataOut <= P_Rd2;
	DRdEn <= P_MemRead;
	DWrEn <= P_MemWrite;
	
	-- WRITE_DATA_MUX  
	with P_MemToReg select
		WRITE_DATA_MUX <= P_Result when '0',
				DDataIn when '1',
				DDataIn when others;
	
	
	-- IFID
	process(Clk, Reset)
		begin
		if Reset = '1' then
			PC_ADD4_IFID <= (others => '0');
			INSTRUCTION_MEMORY_IFID <= (others => '0');
		elsif rising_edge(Clk) and E_IFID then
			PC_ADD4_IFID <= PC_ADD4;
			INSTRUCTION_MEMORY_IFID <= INSTRUCTION_MEMORY;
		end if;
	end process;
	
	
	-- IDEX
	process(Clk, Reset)
		begin
		if Reset = '1' then
			PC_ADD4_IDEX <= (others => '0');	
		elsif rising_edge(Clk) and E_IFID then
			PC_ADD4_IDEX <= PC_ADD4_IFID;
		end if;
	end process;
	
	
	
	
	
	
	
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
	-- mapeo de componentes a las seniales
	u1: reg_bank PORT MAP
	(
		 Clk   =>  Clk,
         Reset =>  Reset,
         A1    =>  INSTRUCTION_MEMORY_IFID(25 downto 21),
         Rd1   =>  P_Rd1,
         A2    =>  INSTRUCTION_MEMORY_IFID(20 downto 16),
         Rd2   =>  P_Rd2,
         A3    =>  WRITE_REGISTER_MUX,
         Wd3   =>  WRITE_DATA_MUX,
         We3   =>  P_RegWrite
	);
	
	u2: control_unit PORT MAP
	(
		 OpCode   => INSTRUCTION_MEMORY_IFID(31 downto 26),	
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
		 Funct      => INSTRUCTION_MEMORY_IFID(5 downto 0),
         ALUControl => P_ALUControl
	);
	
	u4: alu PORT MAP
	(
		 OpA     => P_Rd1,
         OpB     => MUX_ALU_IN,
         Control => P_ALUControl,
         Result  => P_Result,
         ZFlag   => P_ZFlag  
	);
	
end architecture;
