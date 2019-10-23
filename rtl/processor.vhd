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
			Clk   : in std_logic; -- Reloj activo en flanco de subida
			Reset : in std_logic; -- Reset asoncrono a nivel alto
			A1    : in std_logic_vector(4 downto 0); -- Direccion para el puerto Rd1
			Rd1   : out std_logic_vector(31 downto 0); -- Dato del puerto Rd1
			A2    : in std_logic_vector(4 downto 0); -- Direccion para el puerto Rd2
			Rd2   : out std_logic_vector(31 downto 0); -- Dato del puerto Rd2
			A3    : in std_logic_vector(4 downto 0); -- Direccion para el puerto Wd3
			Wd3   : in std_logic_vector(31 downto 0); -- Dato de entrada Wd3
			We3   : in std_logic -- Habilitacion de la escritura de Wd3
		);
	end component;

	component control_unit -- Unidad de control principal
		port (
			OpCode   : in  std_logic_vector (31 downto 0);
			Branch   : out std_logic; -- 1 = Ejecutandose instruccion branch
			Jump     : out std_logic; -- 1 = Ejecutandose instruccion jump
			MemToReg : out std_logic; -- 1 = Escribir en registro la salida de la mem.
			MemWrite : out std_logic; -- Escribir la memoria
			MemRead  : out std_logic; -- Leer la memoria
			ALUSrc   : out std_logic; -- 0 = oper.B es registro, 1 = es valor inm.
			ALUOp    : out std_logic_vector (2 downto 0); -- Tipo operacion para control de la ALU
			RegWrite : out std_logic; -- 1=Escribir registro
			RegDst   : out std_logic -- 0=Reg. destino es rt, 1=rd
		);
	end component;

	component alu_control
		port (
			ALUOp  : in std_logic_vector (2 downto 0); -- Codigo de control desde la unidad de control
			Funct  : in std_logic_vector (5 downto 0); -- Campo "funct" de la instruccion
			ALUControl : out std_logic_vector (3 downto 0) -- Define operacion a ejecutar por la ALU
		);
	end component;

	component alu
		port (
			OpA     : in  std_logic_vector (31 downto 0); -- Operando A
			OpB     : in  std_logic_vector (31 downto 0); -- Operando B
			Control : in  std_logic_vector ( 3 downto 0); -- Codigo de control=op. a ejecutar
			Result  : out std_logic_vector (31 downto 0); -- Resultado
			ZFlag   : out std_logic -- Flag Z
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

	-- alu
	signal P_OpA     : std_logic_vector(31 downto 0);
	signal P_OpB     : std_logic_vector(31 downto 0);
	signal P_Control : std_logic_vector(3 downto 0);
	signal P_Result  : std_logic_vector(31 downto 0);
	signal P_ZFla    : std_logic;

	signal PC_ADD4 : std_logic_vector(31 downto 0);

	signal ADDRESULT : std_logic_vector(31 downto 0);

	-- sign extend out
	signal SIGN_EXTEND_OUT :	std_logic_vector(31 downto 0);

	-- mux_out
	signal PC_SRC_MUX :	std_logic_vector(31 downto 0);

	-- REG_DST_MUX
	signal REG_DST_MUX : std_logic_vector(4 downto 0);

	-- WRITE_DATA_MUX
	signal WRITE_DATA_MUX : std_logic_vector(31 downto 0);

	-- ALU_IN_MUX
	signal ALU_IN_MUX : std_logic_vector(31 downto 0);

	-- IAddr_SIGNAL
	signal IAddr_SIGNAL : std_logic_vector(31 downto 0);

	-- P_ZFlag
	signal P_ZFlag : std_logic;

	-- P_ALUControl
	signal P_ALUControl : std_logic_vector(3 downto 0);

	-- pipelines IFID
	signal ENABLE_IFID : std_logic;
	signal PC_ADD4_IFID : std_logic_vector(31 downto 0);
	signal INSTRUCTION_MEMORY_IFID : std_logic_vector(31 downto 0);
	

	-- pipelines IDEX
	signal ENABLE_IDEX : std_logic;
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
	signal RS_IDEX       : std_logic_vector(4 downto 0);
	signal RT_IDEX       : std_logic_vector(4 downto 0);

	-- pipelines EXMEM
	signal ENABLE_EXMEM : std_logic;
	-- control unit signals
	signal REGWRITE_EXMEM : std_logic;
	signal MEMTOREG_EXMEM : std_logic;
	signal BRANCH_EXMEM   : std_logic;
	signal MEMREAD_EXMEM  : std_logic;
	signal MEMWRITE_EXMEM : std_logic;
	-- the rest of EXMEM
	signal ADDRESULT_EXMEM   : std_logic_vector(31 downto 0);
	signal ZEROFLAG_EXMEM : std_logic;
	signal ALURES_EXMEM   : std_logic_vector(31 downto 0);
	signal RD2_EXMEM      : std_logic_vector(31 downto 0);
	signal REG_DST_MUX_EXMEM   : std_logic_vector(4 downto 0);

	-- pipelines MEMWB
	signal ENABLE_MEMWB : std_logic;
	-- control unit signals
	signal REGWRITE_MEMWB : std_logic;
	signal MEMTOREG_MEMWB : std_logic;
	-- the rest of EXMEM
	signal READDATA_MEMWB : std_logic_vector(31 downto 0);
	signal ALURES_MEMWB   : std_logic_vector(31 downto 0);
	signal REG_DST_MUX_MEMWB   : std_logic_vector(4 downto 0);
	
	-- forwarding unit signals
	signal FORWARD_A         : std_logic_vector(1 downto 0);
	signal FORWARD_B         : std_logic_vector(1 downto 0);
	signal MUX_FORWARD_A     : std_logic_vector(31 downto 0);
	signal MUX_FORWARD_B     : std_logic_vector(31 downto 0);
	
	-- hazard unit
	signal HAZARD_PC : std_logic;
	signal HAZARD_MUX : std_logic;
	
	
begin
-- ==================================================================
-- ZONA IF
-- ==================================================================

	-- MUX PC SRC (falta meter aqui el OR para que salte los jumps tambien)
	with (BRANCH_EXMEM and ZEROFLAG_EXMEM) select PC_SRC_MUX <= 
		PC_ADD4 when '0', -- PC + 4
		ADDRESULT_EXMEM when '1', -- Resultado de la ALU
		ADDRESULT_EXMEM when others; -- ERROR

	-- Cada clk metemos en IAddr_SIGNAL el valor PC_SRC_MUX
	process(Clk, Reset)
	begin
		if Reset = '1' then
			IAddr_SIGNAL <= (others => '0');
		elsif rising_edge(Clk) then
			IAddr_SIGNAL <= PC_SRC_MUX;
			IAddr <= PC_SRC_MUX;
		end if;
	end process;

	-- Metemos PC + 4 en PC_ADD4 (que tambien entra en IF/ID)
	PC_ADD4 <= IAddr_SIGNAL + 4;
   
	-- Pipeline IF/ID
	process(Clk, Reset, ENABLE_IFID)
	begin
		if Reset = '1' then
			PC_ADD4_IFID <= (others => '0');
			INSTRUCTION_MEMORY_IFID <= (others => '0');
		elsif rising_edge(Clk) and ENABLE_IFID= '1' then
			PC_ADD4_IFID <= PC_ADD4; -- Guardamos PC + 4
			INSTRUCTION_MEMORY_IFID <= IDataIn; -- Guardamos la instruccion leida
		end if;
	end process;
   
-- ==================================================================
-- ZONA ID
-- ==================================================================
   
   -- Extendemos el signo de la instruccion desde 15-0 a SIGN_EXTEND_OUT
	with INSTRUCTION_MEMORY_IFID(15) select SIGN_EXTEND_OUT <= 
		"0000000000000000" & INSTRUCTION_MEMORY_IFID(15 downto 0) when '0', -- Extender 0
		"1111111111111111" & INSTRUCTION_MEMORY_IFID(15 downto 0) when '1', -- Extender 1
		"1111111111111111" & INSTRUCTION_MEMORY_IFID(15 downto 0) when others;
	
   -- Pipeline ID/EX
	process(Clk, Reset, ENABLE_IDEX)
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
			
			RS_IDEX <= (others => '0');
			RT_IDEX <= (others => '0');
		elsif rising_edge(Clk) and ENABLE_IDEX= '1' then
			-- Guardamos las salidas mapeadas del control unit en ID/EX
			REGWRITE_IDEX <= P_RegWrite;
			MEMTOREG_IDEX <= P_MemToReg;
			BRANCH_IDEX   <= P_Branch;
			MEMREAD_IDEX  <= P_MemRead;
			MEMWRITE_IDEX <= P_MemWrite;
			REGDST_IDEX   <= P_RegDst;
			ALUOP_IDEX    <= P_ALUOp;
			ALUSRC_IDEX   <= P_ALUSrc;

			-- Movemos el PC + 4 directamente
			PC_ADD4_IDEX  <= PC_ADD4_IFID;

			-- Metemos los registros leidos en ID/EX
			RD1_IDEX      <= P_Rd1;
			RD2_IDEX      <= P_Rd2;

			-- Guardamos la instruccion con signo extendido
			SIGEXT_IDEX   <= SIGN_EXTEND_OUT;

			-- Guardamos dos trozos de instruccion para el mux reg dst
			MUXEX1_IDEX   <= INSTRUCTION_MEMORY_IFID(20 downto 16);
			MUXEX2_IDEX   <= INSTRUCTION_MEMORY_IFID(15 downto 11);
			
			RS_IDEX <= INSTRUCTION_MEMORY_IFID(25 downto 21);
			RT_IDEX <= INSTRUCTION_MEMORY_IFID(20 downto 16);
		end if;
	end process;

-- ==================================================================
-- ZONA EX
-- ==================================================================
  
	-- sumador del PC + 4 y un shift left
	ADDRESULT <= PC_ADD4_IDEX + (SIGEXT_IDEX(29 downto 0) & "00" );

	-- mux activado por el cable alu src
	with ALUSRC_IDEX select ALU_IN_MUX <=
		RD2_IDEX when '0',
		SIGEXT_IDEX when '1',
		SIGEXT_IDEX when others; -- ERROR

	-- mux activado por el cable reg dst
	with REGDST_IDEX select REG_DST_MUX <= 
		MUXEX1_IDEX when '0',
		MUXEX2_IDEX when '1', 
		MUXEX2_IDEX when others; -- ERROR

   -- Pipeline EX/MEM
	process(Clk, Reset, ENABLE_EXMEM)
	begin
		if Reset = '1' then
			-- control unit signals
			REGWRITE_EXMEM <= '0';
			MEMTOREG_EXMEM <= '0';
			BRANCH_EXMEM   <= '0';
			MEMREAD_EXMEM  <= '0';
			MEMWRITE_EXMEM <= '0';

			-- the rest of EXMEM
			ADDRESULT_EXMEM   <= (others => '0');
			ZEROFLAG_EXMEM    <= '0';
			ALURES_EXMEM      <= (others => '0');
			RD2_EXMEM         <= (others => '0');
			REG_DST_MUX_EXMEM <= (others => '0');

		elsif rising_edge(Clk) and ENABLE_EXMEM= '1' then
			-- Movemos directamente las signals de control unit
			REGWRITE_EXMEM <= REGWRITE_IDEX;
			MEMTOREG_EXMEM <= MEMTOREG_IDEX;
			BRANCH_EXMEM   <= BRANCH_IDEX;
			MEMREAD_EXMEM  <= MEMREAD_IDEX;
			MEMWRITE_EXMEM <= MEMWRITE_IDEX;

			-- Resultado de la sumadora del PC + 4 y el shift left 2
			ADDRESULT_EXMEM <= ADDRESULT;

			-- Flag zero de la alu (para el branch). Sacada del port map
			ZEROFLAG_EXMEM <= P_ZFlag;

			-- Resultado de la ALU (sacado del port map)
			ALURES_EXMEM   <= P_Result;

			-- Valor del registro 2 leido del Registers
			RD2_EXMEM      <= RD2_IDEX;

			-- Resultado del Reg Dst MUX
			REG_DST_MUX_EXMEM   <= REG_DST_MUX;
		end if;
	end process;

-- ==================================================================
-- ZONA MEM
-- ==================================================================

	-- Entradas a Data memory
	DAddr    <= ALURES_EXMEM; -- Metemos el resultado de la ALU en Data Memory
	DDataOut <= RD2_EXMEM; -- Escribimos el dato leido del registro 2 
	DRdEn    <= MEMREAD_EXMEM; -- Metemos el enable read
	DWrEn    <= MEMWRITE_EXMEM; -- Metemos el enable write

   -- Pipeline MEM/WB
	process(Clk, Reset, ENABLE_MEMWB)
	begin
		if Reset = '1' then
			-- control unit signals
			REGWRITE_MEMWB <= '0';
			MEMTOREG_MEMWB <= '0';

			-- data memory
			READDATA_MEMWB <= (others => '0');
			ALURES_MEMWB   <= (others => '0');
			REG_DST_MUX_MEMWB   <= (others => '0');
		elsif rising_edge(Clk) and ENABLE_MEMWB= '1' then
			-- Movemos las signals del control unit a la pipeline
			REGWRITE_MEMWB <= REGWRITE_EXMEM;
			MEMTOREG_MEMWB <= MEMTOREG_EXMEM;

			-- Dato leido de Data Memory
			READDATA_MEMWB <= DDataIn;

			-- Guardamos el resultado de la alu
			ALURES_MEMWB   <= ALURES_EXMEM;

			-- Resultado del REG_DST_MUX (lo volvemos a mover sin hacer nada)
			REG_DST_MUX_MEMWB   <= REG_DST_MUX_EXMEM;
		end if;
	end process;

-- ==================================================================
-- ZONA WB
-- ==================================================================

	-- MUX MEM TO REG  
	with MEMTOREG_MEMWB select WRITE_DATA_MUX <=
		ALURES_MEMWB when '0',
		READDATA_MEMWB when '1',
		READDATA_MEMWB when others;
		
		
-- ==================================================================
-- FORWARDING UNIT
-- ==================================================================
	
	FORWARD_A <= "10" when (REGWRITE_EXMEM and (RD2_EXMEM/=0) and (RD2_EXMEM=RS_IDEX)) else
				 "01" when (REGWRITE_MEMWB and (RD2_MEMWB/=0) and (RD2_MEMWB=RS_IDEX)) else
				 "00";
				 
	FORWARD_B <= "10" when (REGWRITE_EXMEM and (RD2_EXMEM/=0) and (RD2_EXMEM=RT_IDEX)) else
				 "01" when (REGWRITE_MEMWB and (RD2_MEMWB/=0) and (RD2_MEMWB=RT_IDEX)) else
				 "00";
	
	with FORWARD_A select MUX_FORWARD_A <=
		RD1_IDEX when "00",
		WRITE_DATA_MUX when "01",
		ALURES_EXMEM when "10",
		ALURES_EXMEM when others;
		
	with FORWARD_B select MUX_FORWARD_B <=
		RD2_IDEX when "00",
		ALURES_EXMEM when "01",
		WRITE_DATA_MUX when "10",
		WRITE_DATA_MUX when others;
	
	
-- ==================================================================
-- HAZARD DETECTION UNIT
-- ==================================================================

	process(Reset, MEMREAD_IDEX, RT_IDEX, INSTRUCTION_MEMORY_IFID)
	begin
		if (Reset = '1' or ((INSTRUCTION_MEMORY_IFID(25 downto 21) = INSTRUCTION_MEMORY_IFID(20 downto 16)) or ((MEMREAD_IDEX = '1') and (RT_IDEX = INSTRUCTION_MEMORY_IFID(20 downto 16)))))then
			HAZARD_PC   <= '0';
			HAZARD_MUX  <= '0';
			ENABLE_IFID <= '0';
 		else 
			HAZARD_PC   <= '1';
			HAZARD_MUX  <= '1';
			ENABLE_IFID <= '1';
		end if;
	end process;
   
-- ==================================================================
-- MAPEO DE SIGNALS
-- ==================================================================

	u1: reg_bank PORT MAP
	(
		Clk   =>  Clk, -- Entrada
		Reset =>  Reset, -- Entrada
		A1    =>  INSTRUCTION_MEMORY_IFID(25 downto 21), -- Entrada
		Rd1   =>  P_Rd1, -- Salida
		A2    =>  INSTRUCTION_MEMORY_IFID(20 downto 16), -- Entrada
		Rd2   =>  P_Rd2, -- Salida
		A3    =>  REG_DST_MUX_MEMWB, -- Entrada
		Wd3   =>  WRITE_DATA_MUX, -- Entrada
		We3   =>  REGWRITE_MEMWB -- Entrada
	);
	
	u2: control_unit PORT MAP
	(
		OpCode   => INSTRUCTION_MEMORY_IFID, -- Entrada
		Branch   => P_Branch, -- Salida
		Jump     => P_Jump, -- Salida
		MemToReg => P_MemToReg, -- Salida
		MemWrite => P_MemWrite, -- Salida
		MemRead  => P_MemRead, -- Salida
		ALUSrc   => P_ALUSrc, -- Salida
		ALUOp    => P_ALUOp, -- Salida
		RegWrite => P_RegWrite, -- Salida
		RegDst   => P_RegDst   -- Salida
	);
	
	u3: alu_control PORT MAP
	(
		ALUOp      => ALUOP_IDEX, -- Entrada
		Funct      => SIGEXT_IDEX(5 downto 0), -- Entrada
		ALUControl => P_ALUControl -- Salida
	);
	
	u4: alu PORT MAP
	(
		OpA     => RD1_IDEX, -- Entrada
		OpB     => ALU_IN_MUX, -- Entrada
		Control => P_ALUControl, -- Entrada
		Result  => P_Result, -- Salida
		ZFlag   => P_ZFlag -- Salida
	);
	
end architecture;
