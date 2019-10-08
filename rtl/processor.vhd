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

--
--
--
-- TODO: hace falta meter el jump aqui que no sale en el dibujo
-- es solo un mux, pero hace falta.
--
--

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
         OpCode   : in  std_logic_vector (31 downto 0);
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
   
   -- control_unit
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
	
	-- pipelines IFID
	signal PC_ADD4_IFID : std_logic_vector(31 downto 0);
	signal INSTRUCTION_MEMORY_IFID : std_logic_vector(31 downto 0);


	-- pipelines IDEX
   -- control unit signals
   signal REGWRITE_IDEX : std_logic;
	signal MEMTOREG_IDEX : std_logic;
	signal BRANCH_IDEX   : std_logic;
	signal MEMREAD_IDEX  : std_logic;
	signal MEMWRITE_IDEX : std_logic;
	signal REGDST_IDEX   : std_logic;
	signal ALUOP_IDEX    : std_logic_vector (2 downto 0);
   signal ALUSRC_IDEX   : std_logic;
   -- PC 
   signal PC_ADD4_IDEX  : std_logic_vector(31 downto 0);
   -- the rest of IDEX
   signal RD1_IDEX      : std_logic_vector(31 downto 0);
   signal RD2_IDEX      : std_logic_vector(31 downto 0);
   signal SIGEXT_IDEX   : std_logic_vector(31 downto 0);
   signal MUXEX1_IDEX   : std_logic_vector(4 downto 0);
   signal MUXEX2_IDEX   : std_logic_vector(4 downto 0);
	
	-- pipelines EXMEM
   -- control unit signals
   signal REGWRITE_EXMEM : std_logic;
	signal MEMTOREG_EXMEM : std_logic;
	signal BRANCH_EXMEM   : std_logic;
	signal MEMREAD_EXMEM  : std_logic;
	signal MEMWRITE_EXMEM : std_logic;
   -- the rest of EXMEM
   signal ADDRES_EXMEM   : std_logic_vector(31 downto 0);
   signal ZEROFLAG_EXMEM : std_logic;
   signal ALURES_EXMEM   : std_logic_vector(31 downto 0);
   signal RD2_EXMEM      : std_logic_vector(31 downto 0);
   signal MUXRES_EXMEM   : std_logic_vector(4 downto 0);
	
	-- pipelines MEMWB
   -- control unit signals
   signal REGWRITE_MEMWB : std_logic;
	signal MEMTOREG_MEMWB : std_logic;
   -- the rest of EXMEM
   signal READDATA_MEMWB : std_logic_vector(31 downto 0);
   signal ALURES_MEMWB   : std_logic_vector(31 downto 0);
   signal MUXRES_MEMWB   : std_logic_vector(4 downto 0);

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
	ALURESULT_ADD <= PC_ADD4_IDEX + (SIGEXT_IDEX(29 downto 0) & "00" );
	
	-- Sign extend
	with INSTRUCTION_MEMORY_IFID(15) select SIGN_EXTEND_OUT(31 downto 0) <= 
      "0000000000000000" & INSTRUCTION_MEMORY_IFID(15 downto 0) when '0',
      "1111111111111111" & INSTRUCTION_MEMORY_IFID(15 downto 0) when '1',
      "1111111111111111" & INSTRUCTION_MEMORY_IFID(15 downto 0) when others;
	
	-- pc
	process(Clk, Reset) 
	begin
		if Reset = '1' then
			IAddr <= (others => '0');	
		elsif rising_edge(Clk) then
			IAddr <= MUX_OUT;
		end if;
	end process;
	
	-- INSTRUCTION MEMORY
	INSTRUCTION_MEMORY <= IDataIn;
	
	-- DATA MEMORY
	DAddr    <= ALURES_EXMEM;
	DDataOut <= RD2_EXMEM;
	DRdEn    <= MEMREAD_EXMEM;
   DWrEn    <= MEMWRITE_EXMEM;
	
	-- MUX BRANCH

   with (BRANCH_EXMEM and ZEROFLAG_EXMEM) select MUX_OUT <= 
      PC_ADD4 when '0',
      ADDRES_EXMEM when '1',
      ADDRES_EXMEM when others; -- ERROR

	-- MUX REG DST
	with REGDST_IDEX select WRITE_REGISTER_MUX <= 
      MUXEX1_IDEX when '0',
      MUXEX2_IDEX when '1', 
      MUXEX2_IDEX when others; -- ERROR
   
   -- MUX ALU SOURCE
	with ALUSRC_IDEX select MUX_ALU_IN <=
      RD2_IDEX when '0',
		SIGEXT_IDEX when '1',
		SIGEXT_IDEX when others; -- ERROR
	
	-- MUX MEM TO REG  
   with MEMTOREG_MEMWB select WRITE_DATA_MUX <=
      ALURES_MEMWB when '0',
		READDATA_MEMWB when '1',
		READDATA_MEMWB when others;
   
-- ==================================================================
-- PIPELINES AND ALL THAT
-- ==================================================================

	-- IF/ID
	process(Clk, Reset)
	begin
		if Reset = '1' then
			PC_ADD4_IFID <= (others => '0');
			INSTRUCTION_MEMORY_IFID <= (others => '0');
		elsif rising_edge(Clk) then
			PC_ADD4_IFID <= PC_ADD4;
			INSTRUCTION_MEMORY_IFID <= INSTRUCTION_MEMORY;
		end if;
	end process;
	
	
	-- ID/EX
	process(Clk, Reset)
	begin
		if Reset = '1' then
         -- Control unit
         REGWRITE_IDEX <= '0';
         MEMTOREG_IDEX <= '0';
         BRANCH_IDEX   <= '0';
         MEMREAD_IDEX  <= '0';
         MEMWRITE_IDEX <= '0';
         REGDST_IDEX   <= '0';
         ALUOP_IDEX    <= (others => '1');
         ALUSRC_IDEX   <= '0';

         -- Current direction
         PC_ADD4_IDEX  <= (others => '0');
         
         -- Register box
         RD1_IDEX      <= (others => '0');
         RD2_IDEX      <= (others => '0');

         -- Sign extend and others
         SIGEXT_IDEX   <= (others => '0');
         MUXEX1_IDEX   <= (others => '0');
         MUXEX2_IDEX   <= (others => '0');
      elsif rising_edge(Clk) then
         -- Control unit
         REGWRITE_IDEX <= P_RegWrite;
         MEMTOREG_IDEX <= P_MemToReg;
         BRANCH_IDEX   <= P_Branch;
         MEMREAD_IDEX  <= P_MemRead;
         MEMWRITE_IDEX <= P_MemWrite;
         REGDST_IDEX   <= P_RegDst;
         ALUOP_IDEX    <= P_ALUOp;
         ALUSRC_IDEX   <= P_ALUSrc;

         -- Current direction
			PC_ADD4_IDEX  <= PC_ADD4_IFID;

         -- Register box
         RD1_IDEX      <= P_Rd1;
         RD2_IDEX      <= P_Rd2;

         -- Sign extend and others
         SIGEXT_IDEX   <= SIGN_EXTEND_OUT;
         MUXEX1_IDEX   <= INSTRUCTION_MEMORY_IFID(20 downto 16);
         MUXEX2_IDEX   <= INSTRUCTION_MEMORY_IFID(15 downto 11);
		end if;
   end process;
   
   -- EX/MEM
   process(Clk, Reset)
	begin
      if Reset = '1' then
         -- control unit signals
         REGWRITE_EXMEM <= '0';
         MEMTOREG_EXMEM <= '0';
         BRANCH_EXMEM   <= '0';
         MEMREAD_EXMEM  <= '0';
         MEMWRITE_EXMEM <= '0';

         -- the rest of EXMEM
         ADDRES_EXMEM   <= (others => '0');
         ZEROFLAG_EXMEM <= '0';
         ALURES_EXMEM   <= (others => '0');
         RD2_EXMEM      <= (others => '0');
         MUXRES_EXMEM   <= (others => '0');
         
      elsif rising_edge(Clk) then
         -- control unit signals (just move them)
         REGWRITE_EXMEM <= REGWRITE_IDEX;
         MEMTOREG_EXMEM <= MEMTOREG_IDEX;
         BRANCH_EXMEM   <= BRANCH_IDEX;
         MEMREAD_EXMEM  <= MEMREAD_IDEX;
         MEMWRITE_EXMEM <= MEMWRITE_IDEX;
         
         -- the rest of EXMEM
         ADDRES_EXMEM   <= ALURESULT_ADD;
         ZEROFLAG_EXMEM <= P_ZFlag;
         ALURES_EXMEM   <= P_Result;
         RD2_EXMEM      <= RD2_IDEX;
         MUXRES_EXMEM   <= WRITE_REGISTER_MUX;
         
		end if;
   end process;
   
   -- MEM/WB
   process(Clk, Reset)
	begin
      if Reset = '1' then
         -- control unit signals
         REGWRITE_MEMWB <= '0';
         MEMTOREG_MEMWB <= '0';
   
         -- data memory
         READDATA_MEMWB <= (others => '0');
         ALURES_MEMWB   <= (others => '0');
         MUXRES_MEMWB   <= (others => '0');
         
      elsif rising_edge(Clk) then
         -- control unit signals
         REGWRITE_MEMWB <= REGWRITE_EXMEM;
         MEMTOREG_MEMWB <= MEMTOREG_EXMEM;
   
         -- data memory
         READDATA_MEMWB <= DDataIn;
         ALURES_MEMWB   <= ALURES_EXMEM;
         MUXRES_MEMWB   <= MUXRES_EXMEM;
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
      We3   =>  REGWRITE_MEMWB
	);
	
	u2: control_unit PORT MAP
	(
      OpCode   => INSTRUCTION_MEMORY_IFID,	
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
      ALUOp      => ALUOP_IDEX,
      Funct      => SIGEXT_IDEX(5 downto 0),
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
