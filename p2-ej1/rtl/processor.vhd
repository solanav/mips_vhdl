--------------------------------------------------------------------------------
-- Procesador MIPS con pipeline curso Arquitectura 2019-2020
--
-- Juan Martin y Antonio Solana
--
--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY processor IS
	PORT (
		Clk : IN std_logic; -- Reloj activo en flanco subida
		Reset : IN std_logic; -- Reset asincrono activo nivel alto
		-- Instruction memory
		IAddr : OUT std_logic_vector(31 DOWNTO 0); -- Direccion Instr
		IDataIn : IN std_logic_vector(31 DOWNTO 0); -- Instruccion leida
		-- Data memory
		DAddr : OUT std_logic_vector(31 DOWNTO 0); -- Direccion
		DRdEn : OUT std_logic; -- Habilitacion lectura
		DWrEn : OUT std_logic; -- Habilitacion escritura
		DDataOut : OUT std_logic_vector(31 DOWNTO 0); -- Dato escrito
		DDataIn : IN std_logic_vector(31 DOWNTO 0) -- Dato leido 
	);
END processor;

ARCHITECTURE rtl OF processor IS
	COMPONENT reg_bank -- Banco de registros
		PORT (
			Clk : IN std_logic; -- Reloj activo en flanco de subida
			Reset : IN std_logic; -- Reset asoncrono a nivel alto
			A1 : IN std_logic_vector(4 DOWNTO 0); -- Direccion para el puerto Rd1
			Rd1 : OUT std_logic_vector(31 DOWNTO 0); -- Dato del puerto Rd1
			A2 : IN std_logic_vector(4 DOWNTO 0); -- Direccion para el puerto Rd2
			Rd2 : OUT std_logic_vector(31 DOWNTO 0); -- Dato del puerto Rd2
			A3 : IN std_logic_vector(4 DOWNTO 0); -- Direccion para el puerto Wd3
			Wd3 : IN std_logic_vector(31 DOWNTO 0); -- Dato de entrada Wd3
			We3 : IN std_logic -- Habilitacion de la escritura de Wd3
		);
	END COMPONENT;

	COMPONENT control_unit -- Unidad de control principal
		PORT (
			OpCode : IN std_logic_vector (31 DOWNTO 0);
			Branch : OUT std_logic; -- 1 = Ejecutandose instruccion branch
			Jump : OUT std_logic; -- 1 = Ejecutandose instruccion jump
			MemToReg : OUT std_logic; -- 1 = Escribir en registro la salida de la mem.
			MemWrite : OUT std_logic; -- Escribir la memoria
			MemRead : OUT std_logic; -- Leer la memoria
			ALUSrc : OUT std_logic; -- 0 = oper.B es registro, 1 = es valor inm.
			ALUOp : OUT std_logic_vector (2 DOWNTO 0); -- Tipo operacion para control de la ALU
			RegWrite : OUT std_logic; -- 1=Escribir registro
			RegDst : OUT std_logic -- 0=Reg. destino es rt, 1=rd
		);
	END COMPONENT;

	COMPONENT alu_control
		PORT (
			ALUOp : IN std_logic_vector (2 DOWNTO 0); -- Codigo de control desde la unidad de control
			Funct : IN std_logic_vector (5 DOWNTO 0); -- Campo "funct" de la instruccion
			ALUControl : OUT std_logic_vector (3 DOWNTO 0) -- Define operacion a ejecutar por la ALU
		);
	END COMPONENT;

	COMPONENT alu
		PORT (
			OpA : IN std_logic_vector (31 DOWNTO 0); -- Operando A
			OpB : IN std_logic_vector (31 DOWNTO 0); -- Operando B
			Control : IN std_logic_vector (3 DOWNTO 0); -- Codigo de control=op. a ejecutar
			Result : OUT std_logic_vector (31 DOWNTO 0); -- Resultado
			ZFlag : OUT std_logic -- Flag Z
		);
	END COMPONENT;

	-- reg_bank
	SIGNAL P_Rd1 : std_logic_vector(31 DOWNTO 0);
	SIGNAL P_Rd2 : std_logic_vector(31 DOWNTO 0);

	-- control_unit
	SIGNAL P_Branch : std_logic;
	SIGNAL P_Jump : std_logic;
	SIGNAL P_MemToReg : std_logic;
	SIGNAL P_MemWrite : std_logic;
	SIGNAL P_MemRead : std_logic;
	SIGNAL P_ALUSrc : std_logic;
	SIGNAL P_ALUOp : std_logic_vector(2 DOWNTO 0);
	SIGNAL P_RegWrite : std_logic;
	SIGNAL P_RegDst : std_logic;

	-- alu
	SIGNAL P_OpA : std_logic_vector(31 DOWNTO 0);
	SIGNAL P_OpB : std_logic_vector(31 DOWNTO 0);
	SIGNAL P_Control : std_logic_vector(3 DOWNTO 0);
	SIGNAL P_Result : std_logic_vector(31 DOWNTO 0);
	SIGNAL P_ZFla : std_logic;

	SIGNAL PC_ADD4 : std_logic_vector(31 DOWNTO 0);
	SIGNAL ADDRESULT : std_logic_vector(31 DOWNTO 0);

	-- sign extend out
	SIGNAL SIGN_EXTEND_OUT : std_logic_vector(31 DOWNTO 0);

	-- mux_out
	SIGNAL PC_SRC_MUX : std_logic_vector(31 DOWNTO 0);

	-- REG_DST_MUX
	SIGNAL REG_DST_MUX : std_logic_vector(4 DOWNTO 0);

	-- WRITE_DATA_MUX
	SIGNAL WRITE_DATA_MUX : std_logic_vector(31 DOWNTO 0);

	-- ALU_IN_MUX
	SIGNAL ALU_IN_MUX : std_logic_vector(31 DOWNTO 0);

	-- IAddr_SIGNAL
	SIGNAL IAddr_SIGNAL : std_logic_vector(31 DOWNTO 0);

	-- P_ZFlag
	SIGNAL P_ZFlag : std_logic;

	-- P_ALUControl
	SIGNAL P_ALUControl : std_logic_vector(3 DOWNTO 0);

	-- pipelines IFID
	SIGNAL PC_ADD4_IFID : std_logic_vector(31 DOWNTO 0);
	SIGNAL INSTRUCTION_MEMORY_IFID : std_logic_vector(31 DOWNTO 0);

	-- pipelines IDEX
	-- control unit signals
	SIGNAL REGWRITE_IDEX : std_logic;
	SIGNAL MEMTOREG_IDEX : std_logic;
	SIGNAL BRANCH_IDEX : std_logic;
	SIGNAL MEMREAD_IDEX : std_logic;
	SIGNAL MEMWRITE_IDEX : std_logic;
	SIGNAL REGDST_IDEX : std_logic;
	SIGNAL ALUOP_IDEX : std_logic_vector (2 DOWNTO 0);
	SIGNAL ALUSRC_IDEX : std_logic;
	-- PC 
	SIGNAL PC_ADD4_IDEX : std_logic_vector(31 DOWNTO 0);
	-- the rest of IDEX
	SIGNAL RD1_IDEX : std_logic_vector(31 DOWNTO 0);
	SIGNAL RD2_IDEX : std_logic_vector(31 DOWNTO 0);
	SIGNAL SIGEXT_IDEX : std_logic_vector(31 DOWNTO 0);
	SIGNAL MUXEX1_IDEX : std_logic_vector(4 DOWNTO 0);
	SIGNAL MUXEX2_IDEX : std_logic_vector(4 DOWNTO 0);
	SIGNAL RS_IDEX : std_logic_vector(4 DOWNTO 0);
	SIGNAL RT_IDEX : std_logic_vector(4 DOWNTO 0);

	-- pipelines EXMEM
	-- control unit signals
	SIGNAL REGWRITE_EXMEM : std_logic;
	SIGNAL MEMTOREG_EXMEM : std_logic;
	SIGNAL BRANCH_EXMEM : std_logic;
	SIGNAL MEMREAD_EXMEM : std_logic;
	SIGNAL MEMWRITE_EXMEM : std_logic;
	-- the rest of EXMEM
	SIGNAL ADDRESULT_EXMEM : std_logic_vector(31 DOWNTO 0);
	SIGNAL ALURES_EXMEM : std_logic_vector(31 DOWNTO 0);
	SIGNAL RD2_EXMEM : std_logic_vector(31 DOWNTO 0);
	SIGNAL REG_DST_MUX_EXMEM : std_logic_vector(4 DOWNTO 0);

	-- pipelines MEMWB
	-- control unit signals
	SIGNAL REGWRITE_MEMWB : std_logic;
	SIGNAL MEMTOREG_MEMWB : std_logic;
	-- the rest of MEMWB
	SIGNAL READDATA_MEMWB : std_logic_vector(31 DOWNTO 0);
	SIGNAL ALURES_MEMWB : std_logic_vector(31 DOWNTO 0);
	SIGNAL ZEROFLAG_EXMEM : std_logic;
	SIGNAL REG_DST_MUX_MEMWB : std_logic_vector(4 DOWNTO 0);

	-- forwarding unit signals
	SIGNAL FORWARD_A : std_logic_vector(1 DOWNTO 0);
	SIGNAL FORWARD_B : std_logic_vector(1 DOWNTO 0);
	SIGNAL MUX_FORWARD_A : std_logic_vector(31 DOWNTO 0);
	SIGNAL MUX_FORWARD_B : std_logic_vector(31 DOWNTO 0);

	-- hazard unit
	SIGNAL HAZARD_ACTIVE : std_logic;

	SIGNAL PCSRC : std_logic;

BEGIN
	-- ==================================================================
	-- ZONA IF
	-- ==================================================================

	-- Controlador para el mux de antes del PC
	PCSRC <= '1' when (BRANCH_EXMEM = '1' AND ZEROFLAG_EXMEM = '1') else '0';

	-- MUX PC SRC (falta meter aqui el OR para que salte los jumps tambien)
	WITH PCSRC SELECT PC_SRC_MUX <=
		PC_ADD4 WHEN '0', -- Si no hay branch o no es zero metemos PC + 4
		ADDRESULT_EXMEM WHEN '1', -- Si hay branch y son iguales (zero) metemos resultado de la ALU
		ADDRESULT_EXMEM WHEN OTHERS; -- No puede pasar ERROR

	-- Cada clk metemos en IAddr_SIGNAL el valor PC_SRC_MUX
	PROCESS (Clk, Reset, HAZARD_ACTIVE, PC_SRC_MUX)
	BEGIN
		IF Reset = '1' THEN
			IAddr_SIGNAL <= (OTHERS => '0');
			IAddr <= (OTHERS => '0');
		ELSIF rising_edge(Clk) AND HAZARD_ACTIVE = '0' THEN
			IAddr_SIGNAL <= PC_SRC_MUX;
			IAddr <= PC_SRC_MUX;
		END IF;
	END PROCESS;

	-- Metemos PC + 4 en PC_ADD4 (que tambien entra en IF/ID)
	PC_ADD4 <= IAddr_SIGNAL + 4;

	-- Pipeline IF/ID
	PROCESS (Clk, Reset)
	BEGIN
		IF Reset = '1' THEN
			PC_ADD4_IFID <= (OTHERS => '0');
			INSTRUCTION_MEMORY_IFID <= (OTHERS => '0');
		ELSIF rising_edge(Clk) AND HAZARD_ACTIVE = '0' THEN
			PC_ADD4_IFID <= PC_ADD4; -- Guardamos PC + 4
			INSTRUCTION_MEMORY_IFID <= IDataIn; -- Guardamos la instruccion leida
		END IF;
	END PROCESS;

	-- ==================================================================
	-- ZONA ID
	-- ==================================================================

	-- sumador del PC + 4 y un shift left
	WITH P_Jump SELECT ADDRESULT <=
		PC_ADD4_IFID + (SIGN_EXTEND_OUT(29 DOWNTO 0) & "00") WHEN '0',
		PC_ADD4_IFID(31 DOWNTO 28) & (INSTRUCTION_MEMORY_IFID(25 DOWNTO 0) & "00") WHEN '1',
		PC_ADD4_IFID(31 DOWNTO 28) & (INSTRUCTION_MEMORY_IFID(25 DOWNTO 0) & "00") WHEN OTHERS;

	-- Extendemos el signo de la instruccion desde 15-0 a SIGN_EXTEND_OUT
	WITH INSTRUCTION_MEMORY_IFID(15) SELECT SIGN_EXTEND_OUT <=
		"0000000000000000" & INSTRUCTION_MEMORY_IFID(15 DOWNTO 0) WHEN '0', -- Extender 0
		"1111111111111111" & INSTRUCTION_MEMORY_IFID(15 DOWNTO 0) WHEN '1', -- Extender 1
		"1111111111111111" & INSTRUCTION_MEMORY_IFID(15 DOWNTO 0) WHEN OTHERS;

	-- Pipeline ID/EX
	PROCESS (Clk, Reset)
	BEGIN
		IF Reset = '1' THEN
			-- Control unit
			REGWRITE_IDEX <= '0';
			MEMTOREG_IDEX <= '0';
			MEMREAD_IDEX <= '0';
			MEMWRITE_IDEX <= '0';
			REGDST_IDEX <= '0';
			ALUOP_IDEX <= (OTHERS => '1');
			ALUSRC_IDEX <= '0';

			-- Current direction
			PC_ADD4_IDEX <= (OTHERS => '0');

			-- Register box
			RD1_IDEX <= (OTHERS => '0');
			RD2_IDEX <= (OTHERS => '0');

			-- Sign extend and others
			SIGEXT_IDEX <= (OTHERS => '0');
			MUXEX1_IDEX <= (OTHERS => '0');
			MUXEX2_IDEX <= (OTHERS => '0');

			RS_IDEX <= (OTHERS => '0');
			RT_IDEX <= (OTHERS => '0');
		ELSIF rising_edge(Clk) THEN
			-- Movemos el PC + 4 directamente
			PC_ADD4_IDEX <= PC_ADD4_IFID;

			-- Metemos los registros leidos en ID/EX
			RD1_IDEX <= P_Rd1;
			RD2_IDEX <= P_Rd2;

			-- Guardamos la instruccion con signo extendido
			SIGEXT_IDEX <= SIGN_EXTEND_OUT;

			-- Guardamos dos trozos de instruccion para el mux reg dst
			MUXEX1_IDEX <= INSTRUCTION_MEMORY_IFID(20 DOWNTO 16);
			MUXEX2_IDEX <= INSTRUCTION_MEMORY_IFID(15 DOWNTO 11);

			RS_IDEX <= INSTRUCTION_MEMORY_IFID(25 DOWNTO 21);
			RT_IDEX <= INSTRUCTION_MEMORY_IFID(20 DOWNTO 16);

			-- Multiplexor despues de la hazard unit y la unidad de control 
			IF HAZARD_ACTIVE = '1' THEN
				REGWRITE_IDEX <= '0';
				MEMTOREG_IDEX <= '0';
				MEMREAD_IDEX <= '0';
				MEMWRITE_IDEX <= '0';
				REGDST_IDEX <= '0';
				ALUOP_IDEX <= (OTHERS => '1');
				ALUSRC_IDEX <= '0';
			ELSE
				-- Guardamos las salidas mapeadas del control unit en ID/EX
				REGWRITE_IDEX <= P_RegWrite;
				MEMTOREG_IDEX <= P_MemToReg;
				MEMREAD_IDEX <= P_MemRead;
				MEMWRITE_IDEX <= P_MemWrite;
				REGDST_IDEX <= P_RegDst;
				ALUOP_IDEX <= P_ALUOp;
				ALUSRC_IDEX <= P_ALUSrc;
			END IF;
		END IF;
	END PROCESS;

	-- ==================================================================
	-- ZONA EX
	-- ==================================================================

	-- sumador del PC + 4 y un shift left
	ADDRESULT <= PC_ADD4_IDEX + (SIGEXT_IDEX(29 DOWNTO 0) & "00");

	-- mux activado por el cable alu src
	WITH ALUSRC_IDEX SELECT ALU_IN_MUX <=
		MUX_FORWARD_B WHEN '0',
		SIGEXT_IDEX WHEN '1',
		SIGEXT_IDEX WHEN OTHERS; -- ERROR

	-- mux activado por el cable reg dst
	WITH REGDST_IDEX SELECT REG_DST_MUX <=
		MUXEX1_IDEX WHEN '0',
		MUXEX2_IDEX WHEN '1',
		MUXEX2_IDEX WHEN OTHERS; -- ERROR

	-- Pipeline EX/MEM
	PROCESS (Clk, Reset)
	BEGIN
		IF Reset = '1' THEN
			-- control unit signals
			REGWRITE_EXMEM <= '0';
			MEMTOREG_EXMEM <= '0';
			BRANCH_EXMEM <= '0';
			MEMREAD_EXMEM <= '0';
			MEMWRITE_EXMEM <= '0';

			-- the rest of EXMEM
			ADDRESULT_EXMEM <= (OTHERS => '0');
			ZEROFLAG_EXMEM <= '0';
			ALURES_EXMEM <= (OTHERS => '0');
			RD2_EXMEM <= (OTHERS => '0');
			REG_DST_MUX_EXMEM <= (OTHERS => '0');

		ELSIF rising_edge(Clk) THEN
			-- Movemos directamente las signals de control unit
			REGWRITE_EXMEM <= REGWRITE_IDEX;
			MEMTOREG_EXMEM <= MEMTOREG_IDEX;
			BRANCH_EXMEM <= BRANCH_IDEX;
			MEMREAD_EXMEM <= MEMREAD_IDEX;
			MEMWRITE_EXMEM <= MEMWRITE_IDEX;

			-- Resultado de la sumadora del PC + 4 y el shift left 2
			ADDRESULT_EXMEM <= ADDRESULT;

			-- Flag zero de la alu (para el branch). Sacada del port map
			ZEROFLAG_EXMEM <= P_ZFlag;

			-- Resultado de la ALU (sacado del port map)
			ALURES_EXMEM <= P_Result;

			-- Valor del registro 2 leido del Registers
			RD2_EXMEM <= RD2_IDEX;

			-- Resultado del Reg Dst MUX
			REG_DST_MUX_EXMEM <= REG_DST_MUX;
		END IF;
	END PROCESS;

	-- ==================================================================
	-- ZONA MEM
	-- ==================================================================

	-- Entradas a Data memory
	DAddr <= ALURES_EXMEM; -- Metemos el resultado de la ALU en Data Memory
	DDataOut <= RD2_EXMEM; -- Escribimos el dato leido desde el mux forwarding B
	DRdEn <= MEMREAD_EXMEM; -- Metemos el enable read
	DWrEn <= MEMWRITE_EXMEM; -- Metemos el enable write

	-- Pipeline MEM/WB
	PROCESS (Clk, Reset)
	BEGIN
		IF Reset = '1' THEN
			-- control unit signals
			REGWRITE_MEMWB <= '0';
			MEMTOREG_MEMWB <= '0';

			-- data memory
			READDATA_MEMWB <= (OTHERS => '0');
			ALURES_MEMWB <= (OTHERS => '0');
			REG_DST_MUX_MEMWB <= (OTHERS => '0');
		ELSIF rising_edge(Clk) THEN
			-- Movemos las signals del control unit a la pipeline
			REGWRITE_MEMWB <= REGWRITE_EXMEM;
			MEMTOREG_MEMWB <= MEMTOREG_EXMEM;

			-- Dato leido de Data Memory
			READDATA_MEMWB <= DDataIn;

			-- Guardamos el resultado de la alu
			ALURES_MEMWB <= ALURES_EXMEM;

			-- Resultado del REG_DST_MUX (lo volvemos a mover sin hacer nada)
			REG_DST_MUX_MEMWB <= REG_DST_MUX_EXMEM;
		END IF;
	END PROCESS;

	-- ==================================================================
	-- ZONA WB
	-- ==================================================================

	-- MUX MEM TO REG  
	WITH MEMTOREG_MEMWB SELECT WRITE_DATA_MUX <=
		ALURES_MEMWB WHEN '0',
		READDATA_MEMWB WHEN '1',
		READDATA_MEMWB WHEN OTHERS;

	-- ==================================================================
	-- FORWARDING UNIT
	-- ==================================================================

	FORWARD_A <= "10" WHEN ((REGWRITE_EXMEM = '1') AND (REG_DST_MUX_EXMEM /= "00000") AND (REG_DST_MUX_EXMEM = RS_IDEX)) ELSE
		"01" WHEN (REGWRITE_MEMWB = '1' AND (REG_DST_MUX_MEMWB /= "00000") AND (REG_DST_MUX_MEMWB = RS_IDEX)) ELSE
		"00";

	FORWARD_B <= "10" WHEN (REGWRITE_EXMEM = '1' AND (REG_DST_MUX_EXMEM /= "00000") AND (REG_DST_MUX_EXMEM = RT_IDEX)) ELSE
		"01" WHEN (REGWRITE_MEMWB = '1' AND (REG_DST_MUX_MEMWB /= "00000") AND (REG_DST_MUX_MEMWB = RT_IDEX)) ELSE
		"00";

	WITH FORWARD_A SELECT MUX_FORWARD_A <=
		RD1_IDEX WHEN "00",
		WRITE_DATA_MUX WHEN "01",
		ALURES_EXMEM WHEN "10",
		ALURES_EXMEM WHEN OTHERS;

	WITH FORWARD_B SELECT MUX_FORWARD_B <=
		RD2_IDEX WHEN "00",
		WRITE_DATA_MUX WHEN "01",
		ALURES_EXMEM WHEN "10",
		ALURES_EXMEM WHEN OTHERS;

	-- ==================================================================
	-- HAZARD DETECTION UNIT
	-- ==================================================================

	-- Si hay un 1 dentro, hay un peligro, debemos parar el pipeline
	HAZARD_ACTIVE <= '1' WHEN MEMREAD_IDEX = '1' AND ((MUXEX1_IDEX = INSTRUCTION_MEMORY_IFID(20 DOWNTO 16) AND MUXEX1_IDEX /= "00000") or (MUXEX1_IDEX = INSTRUCTION_MEMORY_IFID(25 DOWNTO 21) AND MUXEX1_IDEX /= "00000"))
	else '0';

	-- ==================================================================
	-- MAPEO DE SIGNALS
	-- ==================================================================

	u1 : reg_bank PORT MAP
	(
		Clk => Clk, -- Entrada
		Reset => Reset, -- Entrada
		A1 => INSTRUCTION_MEMORY_IFID(25 DOWNTO 21), -- Entrada
		Rd1 => P_Rd1, -- Salida
		A2 => INSTRUCTION_MEMORY_IFID(20 DOWNTO 16), -- Entrada
		Rd2 => P_Rd2, -- Salida
		A3 => REG_DST_MUX_MEMWB, -- Entrada
		Wd3 => WRITE_DATA_MUX, -- Entrada
		We3 => REGWRITE_MEMWB -- Entrada
	);

	u2 : control_unit PORT MAP
	(
		OpCode => INSTRUCTION_MEMORY_IFID, -- Entrada
		Branch => P_Branch, -- Salida
		Jump => P_Jump, -- Salida
		MemToReg => P_MemToReg, -- Salida
		MemWrite => P_MemWrite, -- Salida
		MemRead => P_MemRead, -- Salida
		ALUSrc => P_ALUSrc, -- Salida
		ALUOp => P_ALUOp, -- Salida
		RegWrite => P_RegWrite, -- Salida
		RegDst => P_RegDst -- Salida
	);

	u3 : alu_control PORT MAP
	(
		ALUOp => ALUOP_IDEX, -- Entrada
		Funct => SIGEXT_IDEX(5 DOWNTO 0), -- Entrada
		ALUControl => P_ALUControl -- Salida
	);

	u4 : alu PORT MAP
	(
		OpA => MUX_FORWARD_A, -- Entrada
		OpB => ALU_IN_MUX, -- Entrada
		Control => P_ALUControl, -- Entrada
		Result => P_Result, -- Salida
		ZFlag => P_ZFlag -- Salida
	);

END ARCHITECTURE;