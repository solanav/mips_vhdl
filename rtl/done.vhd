--------------------------------------------------------------------------------
-- Procesador MIPS con pipeline curso Arquitectura 2018-19
--
-- Autores:
--	Pablo Diez
-- Ignacio Rabunnal
-- Grupo 1302
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity processor is
   port(
      Clk         : in  std_logic; -- Reloj activo flanco subida
      Reset       : in  std_logic; -- Reset asincrono activo nivel alto
      -- Instruction memory
      IAddr      : out std_logic_vector(31 downto 0); -- Direccion
      IDataIn    : in  std_logic_vector(31 downto 0); -- Dato leido
      -- Data memory
      DAddr      : out std_logic_vector(31 downto 0); -- Direccion
      DRdEn      : out std_logic;                     -- Habilitacion lectura
      DWrEn      : out std_logic;                     -- Habilitacion escritura
      DDataOut   : out std_logic_vector(31 downto 0); -- Dato escrito
      DDataIn    : in  std_logic_vector(31 downto 0)  -- Dato leido
   );
end processor;
architecture rtl of processor is 

	component reg_bank --Declaracion del componente del banco de registros.
		port (
			Clk		: in std_logic;						
			Reset 	: in std_logic;							
			A1 		: in std_logic_vector(4 downto 0);		
			Rd1 	: out std_logic_vector(31 downto 0);	
			A2 		: in std_logic_vector(4 downto 0);		
			Rd2 	: out std_logic_vector(31 downto 0);	
			A3 		: in std_logic_vector(4 downto 0);		
			Wd3 	: in std_logic_vector(31 downto 0);		
			We3 	: in std_logic							
		); 
	end component;
	
	component control_unit --Declaracion del componente Unidad de Control
		port(
			OpCode  : in  std_logic_vector (31 downto 0);
			Branch	: out  std_logic;	
			MemToReg: out  std_logic;	
			MemWrite: out  std_logic;	
			MemRead : out  std_logic;	
			ALUSrc 	: out  std_logic;	
			ALUOp  	: out  std_logic_vector (2 downto 0);
			RegWrite: out  std_logic;	
			RegDst  : out  std_logic;
			Jump	: out  std_logic
		);
	end component;
	
	component alu_control --Declaracion del componente ALU CONTROL
		port (
			ALUOp  	: in std_logic_vector (2 downto 0);	
			Funct  	: in std_logic_vector (5 downto 0);
			ALUControl : out std_logic_vector (3 downto 0) 
		);
	end component;
	
	component alu --Declaración del componente ALU
		port(
			OpA     : in  std_logic_vector (31 downto 0); 
			OpB     : in  std_logic_vector (31 downto 0);
			Control : in  std_logic_vector ( 3 downto 0); 
			Result  : out std_logic_vector (31 downto 0); 
			ZFlag   : out std_logic                       
		);
	end component;
	

	--Seniales auxiliares para el procesador
	--Unidad de Control
	signal signalRegDst_ID : std_logic;
	signal signalRegDst_EX : std_logic;
	signal signalBranch_ID : std_logic;
	signal signalBranch_EX : std_logic;
	signal signalBranch_MEM : std_logic;
	signal signalMemRead_ID : std_logic;
	signal signalMemRead_EX : std_logic;
	signal signalMemRead_MEM : std_logic;
	signal signalMemToReg_ID : std_logic;
	signal signalMemToReg_EX : std_logic;
	signal signalMemToReg_MEM : std_logic;
	signal signalMemToReg_WB : std_logic;
	signal signalALUOp_ID : std_logic_vector (2 downto 0);
	signal signalALUOp_EX : std_logic_vector (2 downto 0);
	signal signalMemWrite_ID : std_logic;
	signal signalMemWrite_EX : std_logic;
	signal signalMemWrite_MEM : std_logic;
	signal signalALUSrc_ID : std_logic;
	signal signalALUSrc_EX : std_logic; 	
	signal signalRegWrite_ID : std_logic;
	signal signalRegWrite_EX : std_logic;
	signal signalRegWrite_WB : std_logic;
	signal signalRegWrite_MEM : std_logic;
	signal signalJump_ID : std_logic;
	signal signalJump_EX : std_logic;
	signal signalJump_MEM : std_logic;
	signal singal_NOP_ID: std_logic;
	signal singal_NOP_EX: std_logic;
	signal singal_NOP_MEM: std_logic;
	signal singal_NOP_WB: std_logic;
	
	--Banco de registros
	signal signalRd1_ID : std_logic_vector(31 downto 0);
	signal signalRd1_EX : std_logic_vector(31 downto 0);
	signal signalRd2_ID : std_logic_vector(31 downto 0);
	signal signalRd2_EX : std_logic_vector(31 downto 0);
	signal signalRd2_MEM : std_logic_vector(31 downto 0);
	signal signalA3 : std_logic_vector(4 downto 0);
	signal signalWd3 : std_logic_vector(31 downto 0);
	
	--ALU
	signal signalALUResult_EX : std_logic_vector(31 downto 0);
	signal signalALUResult_MEM : std_logic_vector(31 downto 0);
	signal signalALUResult_WB : std_logic_vector(31 downto 0);
	signal signalZFlag_EX : std_logic;
	signal signalZFlag_MEM : std_logic;
	
	--ALUControl
	signal signalALUControl : std_logic_vector(3 downto 0);
	
	--And que se encuentra en el procesador
	signal signalAND: std_logic;
	
	--Multiplexores presentes en el procesador
	signal MUX_RegDest_EX : std_logic_vector(4 downto 0);
	signal MUX_RegDest_MEM : std_logic_vector(4 downto 0);
	signal MUX_RegDest_WB : std_logic_vector(4 downto 0);
	signal MUX_ALUSrc : std_logic_vector(31 downto 0);
	signal MUX_AND : std_logic_vector(31 downto 0);
	signal MUX_MemToReg : std_logic_vector(31 downto 0);
	
	--Desplazamientos que realiza el procesador
	signal desplazamientoJump : std_logic_vector(27 downto 0);
	signal desplazamientosignalExtend : std_logic_vector(31 downto 0);

	--Extension de signo
	signal signalExtend_ID : std_logic_vector(31 downto 0);
	signal signalExtend_EX : std_logic_vector(31 downto 0);
	
	--Sseñales para el Jump
	signal signalDirJump_ID : std_logic_vector(31 downto 0);
	signal signalDirJump_EX : std_logic_vector(31 downto 0);
	signal signalDirJump_MEM : std_logic_vector(31 downto 0);
	
	--Sumadores del procesador
	signal sumadorAddr4_IF : std_logic_vector(31 downto 0); 
	signal sumadorAddr4_ID : std_logic_vector(31 downto 0); 
	signal sumadorAddr4_EX : std_logic_vector(31 downto 0); 
	signal sumadorSumDesp_EX : std_logic_vector(31 downto 0); 
	signal sumadorSumDesp_MEM : std_logic_vector(31 downto 0); 
	--Memoria de Instrucciones
	signal signalIAddr : std_logic_vector(31 downto 0); 
	signal signalInstruction_IF : std_logic_vector(31 downto 0); 
	signal signalInstruction_ID : std_logic_vector(31 downto 0); 
	signal signalInstruction_EX : std_logic_vector(31 downto 0); 
	
	--Memoria de Datos
	signal signalDDataIn_MEM : std_logic_vector(31 downto 0); 
	signal signalDDataIn_WB : std_logic_vector(31 downto 0); 
	
	--Enables registros del pippeline
	signal enable_IF_ID: std_logic;
	signal enable_ID_EX: std_logic;
	signal enable_EX_MEM: std_logic;
	signal enable_MEM_WB: std_logic;
	
	--Hazard detection unit
	signal signalHazard_PC : std_logic;
	signal signalHazard : std_logic;

	--Forwarding unit
	signal forwardA: std_logic_vector(1 downto 0);
	signal forwardB: std_logic_vector(1 downto 0);
	signal MUX_forwardA: std_logic_vector(31 downto 0);
	signal MUX_forwardB: std_logic_vector(31 downto 0);
	
	
	begin
	
	--Inicializacion enables a 1
	enable_IF_ID <= '1';
	enable_ID_EX <= '1';
	enable_EX_MEM <= '1';
	enable_MEM_WB <= '1';
			
	--ETAPA IF del pippeline
	--Proceso del reloj del procesador
	process(Clk, Reset) 
	begin
		if Reset = '1' then
			signalIAddr <= (others => '0');	
		elsif rising_edge(Clk) then
			signalIAddr <= MUX_AND;
		end if;
	end process;
	
	--Multiplexor PCSrc
	process (signalJump_MEM,signalDirJump_MEM, signalZFlag_MEM, signalBranch_MEM, sumadorAddr4_IF, sumadorSumDesp_MEM) 
		begin
			if signalJump_MEM = '1' then
				MUX_AND <= signalDirJump_MEM;
			elsif (signalZFlag_MEM AND signalBranch_MEM) = '1' then
				 MUX_AND <= sumadorSumDesp_MEM;
			else 
				MUX_AND <= sumadorAddr4_IF;
			end if;
	end process;
	
	--Sumador del pc
	sumadorAddr4_IF <= signalIAddr + 4;
	
	--Memoria de Instrucciones
	IAddr <= signalIAddr;	--Indica la direccion de la instruccion
	signalInstruction_IF <= IDataIn; 	--Instruccion propiamente dichar
	
	--Proceso sincrono que crea el registro IF/ID
	process(Clk, Reset)
	begin 
		if Reset = '1' then  --Propagacion de valores
			sumadorAddr4_ID <= (others => '0');
			signalInstruction_ID <= (others => '0');
		elsif rising_edge(Clk) and enable_IF_ID='1' then--Inicializacion del registro
			sumadorAddr4_ID <= sumadorAddr4_IF;
			signalInstruction_ID <= signalInstruction_IF;
		end if;
	end process;
	
	
	--Etapa ID del pippeline
	--Proceso para la extension del signo
	process(signalInstruction_ID)
		begin
			--Si el numero es negativo, se ponen los 16 bits de mas peso a 1
			
			if signalInstruction_ID(15) = '1' then 
				signalExtend_ID(31 downto 16) <= (others => '1');
				signalExtend_ID(15 downto 0) <= signalInstruction_ID(15 downto 0);
			--Si el numero es positivo se ponen los 16 bits con mas peso a 0
			else 
				signalExtend_ID(31 downto 16) <= (others => '0');
				signalExtend_ID(15 downto 0) <= signalInstruction_ID(15 downto 0);
			end if;
	end process;
	
		--Hazard detection
	process(signalMemRead_EX, signalInstruction_ID, signalInstruction_EX, Reset)
	begin
		if Reset = '1' then
			signalHazard_PC <= '0';
			enable_IF_ID <= '0';
			signalHazard <= '0';
		elsif ((signalMemRead_EX = '1') and(signalInstruction_EX(20 downto 16) = signalInstruction_ID(20 downto 16)) 
		or (signalInstruction_EX(20 downto 16) = signalInstruction_ID(25 downto 21))) then 
			signalHazard_PC <= '0';
			enable_IF_ID <= '0';
			signalHazard <= '0';
		else
			signalHazard_PC <= '1';
			enable_IF_ID <= '1';
			signalHazard <= '1';
		end if;
	end process;
	
	--Desplaza para el Jump
	desplazamientoJump <= signalInstruction_ID(25 downto 0) & "00"; 
	
	--Concatena el sumador y el desplazamiento para calcular la direccion efectiva de salto
	signalDirJump_ID <= sumadorAddr4_ID(31 downto 28) & desplazamientoJump(27 downto 0); 
	
	--Proceso sincrono que crea el registro ID/EX
	process(Clk, Reset)
		begin
		if Reset='1' then
			signalRegDst_EX <=  '0';
			signalBranch_EX <=  '0';
			signalMemRead_EX <='0';
			signalMemToReg_EX <= '0';
			signalALUOp_EX <= (others => '0');
			signalMemWrite_EX <= '0';
			signalALUSrc_EX <=  '0';
			signalRegWrite_EX <= '0';
			signalJump_EX <= '0';
			signalRd1_EX <= (others => '0');
			signalRd2_EX <= (others => '0');
			signalExtend_EX <= (others => '0');
			signalDirJump_EX <= (others => '0');
			sumadorAddr4_EX <= (others => '0');
			signalInstruction_EX <= (others => '0');
		elsif rising_edge(Clk) and enable_ID_EX='1' then
			signalRd1_EX <= signalRd1_ID;
			signalRd2_EX <= signalRd2_ID;
			signalExtend_EX <= signalExtend_ID;
			sumadorAddr4_EX <= sumadorAddr4_ID;
			signalInstruction_EX <= signalInstruction_ID;
			if enable_IF_ID = '1' then --Revisar si hay que controlar signalHazard tambien
				signalRegDst_EX <= signalRegDst_ID;
				signalBranch_EX <= signalBranch_ID;
				signalMemRead_EX <= signalMemRead_ID;
				signalMemToReg_EX <= signalMemToReg_ID;
				signalALUOp_EX <= signalALUOp_ID;
				signalMemWrite_EX <= signalMemWrite_ID;
				signalALUSrc_EX <= signalALUSrc_ID;
				signalRegWrite_EX <= signalRegWrite_ID;
				signalJump_EX <= signalJump_ID;
				signalDirJump_EX <= signalDirJump_ID;
			elsif signalHazard = '1' then
				signalRegDst_EX <= '0';
				signalBranch_EX <= '0';
				signalMemRead_EX <= '0';
				signalMemToReg_EX <= '0';
				signalALUOp_EX <= (others => '0');
				signalMemWrite_EX <= '0';
				signalALUSrc_EX <= '0';
				signalRegWrite_EX <= '0';
				signalJump_EX <= '0';
				signalDirJump_EX <= (others => '0');
			end if;
		end if;
	end process;
	
	--Etapa EX del pipelinne
	--Sumador que acumula el valor del PC+4 y el signo desplazado extendido
	sumadorSumDesp_EX <= sumadorAddr4_EX + desplazamientosignalExtend;
	
	--Desplazamiento de la extension de signo
	desplazamientosignalExtend <= signalExtend_EX(29 downto 0) & "00";
	
	--Multiplexor ALUSrc:
	process ( signalALUSrc_EX, signalExtend_EX, signalRd2_EX) 
		begin
			--Si ALUSrc es 1 coge la señal de la extension de signo
			if signalALUSrc_EX = '1' then 
				MUX_ALUSrc <= signalExtend_EX;
			--Si ALUSrc es 0 coge el operando dos
			else 
				MUX_ALUSrc <= signalRd2_EX;
			end if;
	end process;
			
	--Multiplexor RegDest:
	process (signalRegDst_EX, signalInstruction_EX) 
		begin
			--Si la señal que indica el registro destino es 1 coge los bits del 15 al 11 de la instruccion
			if signalRegDst_EX = '1' then 
				MUX_RegDest_EX <= signalInstruction_EX(15 downto 11);
			--Si la señal que indica el registro destino es 0 coge los bits 20 al 16 de la instruccion
			else 
				MUX_RegDest_EX <= signalInstruction_EX(20 downto 16);
			end if;
	end process;
	
		--Forwarding unit
	process(signalRegWrite_WB, MUX_RegDest_WB,signalRegWrite_MEM,MUX_RegDest_MEM, signalInstruction_EX)
	
	begin
		forwardA <= "00";
		forwardB <= "00";
		--MEM->Primero MEM por si fuese necesario utilizar el adelantamiento EX, como es secuencial seria este 1º
		if(signalRegWrite_WB='1') and (MUX_RegDest_WB = signalInstruction_EX(25 downto 21)) and (MUX_RegDest_WB /= "00000") then
			if (signalRegWrite_MEM = '1') and (MUX_RegDest_MEM = signalInstruction_EX(25 downto 21)) and (MUX_RegDest_MEM /= "00000") then	
				forwardA <= "00";
			else
				forwardA <= "01";
			end if;
		end if;	
		if(signalRegWrite_WB='1') and (MUX_RegDest_WB = signalInstruction_EX(20 downto 16)) and (MUX_RegDest_WB /= "00000") then
			if (signalRegWrite_MEM = '1') and (MUX_RegDest_MEM = signalInstruction_EX(20 downto 16)) and (MUX_RegDest_MEM /= "00000") then 	
				forwardB <= "00";
			else
				forwardB <= "01";
			end if;
		end if;	
		--EX
		if (signalRegWrite_MEM = '1') and (MUX_RegDest_MEM = signalInstruction_EX(25 downto 21)) and (MUX_RegDest_MEM /= "00000") then	
			forwardA <= "10";
		end if;
		if (signalRegWrite_MEM = '1') and (MUX_RegDest_MEM = signalInstruction_EX(20 downto 16)) and (MUX_RegDest_MEM /= "00000") then 	
			  forwardB <= "10";
		end if;
	end process;	
	
		--Multiplexor forwardA
	process (signalRd1_EX, MUX_MemToReg, signalALUResult_MEM, forwardA) 
	begin
		if forwardA = "01" then 
			MUX_forwardA <= MUX_MemToReg;
		elsif forwardA = "10" then 
			MUX_forwardA <= signalALUResult_MEM;
		else 
			MUX_forwardA <= signalRd1_EX;
		end if;
	end process;
	
	--Multiplexor forwardB
	process (signalRd2_EX, MUX_MemToReg, signalALUResult_MEM, forwardB) 
	begin
		if forwardB = "01" then 
			MUX_forwardB <= MUX_MemToReg;
		elsif forwardB = "10" then 
			MUX_forwardB <= signalALUResult_MEM;
		else 
			MUX_forwardB <= signalRd2_EX;
		end if;
	end process;
	

	
	----Proceso sincrono que crea el registro EX/MEM
	process(Clk, Reset)
		begin
		if Reset='1' then
			signalBranch_MEM <= '0';
			signalMemRead_MEM <= '0';
			signalMemToReg_MEM <= '0';
			signalMemWrite_MEM <= '0';
			signalRegWrite_MEM <= '0';
			signalJump_MEM <= '0';
			signalRd2_MEM <= (others => '0');
			signalALUResult_MEM <= (others => '0');
			signalZFlag_MEM <= '0';
			MUX_RegDest_MEM <= (others => '0');
			signalDirJump_MEM <= (others => '0');
			sumadorSumDesp_MEM <= (others => '0');
		elsif rising_edge(Clk) and enable_EX_MEM='1' then
			signalBranch_MEM <= signalBranch_EX;
			signalMemRead_MEM <= signalMemRead_EX;
			signalMemToReg_MEM <= signalMemToReg_EX;
			signalMemWrite_MEM <= signalMemWrite_EX;
			signalRegWrite_MEM <= signalRegWrite_EX;
			signalJump_MEM <= signalJump_EX;
			signalRd2_MEM <= signalRd2_EX;
			signalALUResult_MEM <= signalALUResult_EX;
			signalZFlag_MEM <= signalZFlag_EX;
			MUX_RegDest_MEM <= MUX_RegDest_EX;
			signalDirJump_MEM <= signalDirJump_EX;
			sumadorSumDesp_MEM <= sumadorSumDesp_EX;
		end if;
	end process;
	
	--Etapa MEM del pippeline
	DAddr <= signalALUResult_MEM; -- Direccion para la memoria
	DRdEn <= signalMemRead_MEM; -- Permite la lectura
	DWrEn <= signalMemWrite_MEM; -- Permite la escritura
	signalDDataIn_MEM <= DDataIn;	--Indica el dato a leer
	DDataOut <= signalRd2_MEM;	--
	
	
	----Proceso sincrono que crea el registro MEM/WB
	process(Clk, Reset)
		begin
		if Reset='1' then
			signalALUResult_WB <= (others => '0');
			signalDDataIn_WB <=(others => '0');
			signalMemToReg_WB <= '0';
			MUX_RegDest_WB <=(others => '0');
			signalRegWrite_WB <= '0';
		elsif rising_edge(Clk) and enable_MEM_WB='1' then
			signalALUResult_WB <= signalALUResult_MEM;
			signalDDataIn_WB <= signalDDataIn_MEM;
			signalMemToReg_WB <= signalMemToReg_MEM;
			MUX_RegDest_WB <= MUX_RegDest_MEM;
			signalRegWrite_WB <= signalRegWrite_MEM;
		end if;
	end process;
	
	--Etapa WB del pippeline
	--Multiplexor MemToReg:
	process (signalMemToReg_WB, signalDDataIn_WB, signalALUResult_WB ) 
		begin
			--Si la señal es 1 manda el dato a escribir de la memoria de datos
			if signalMemToReg_WB = '1' then 
				MUX_MemToReg <= signalDDataIn_WB;
			--Si la señal es 0 manda el dato a escribir en el registro el dato que viene de la ALU	
			else 
				MUX_MemToReg <= signalALUResult_WB;
				
			end if;
	end process;
	
		--PORT MAP de los componentes
	BR: reg_bank port map (
			Clk => Clk, 
			Reset => Reset, 
			A1 => signalInstruction_ID(25 downto 21), 
			A2 => signalInstruction_ID(20 downto 16),
			Rd1 => signalRd1_ID, 
			Rd2 => signalRd2_ID, 
			A3 => MUX_RegDest_WB, 
			Wd3 => MUX_MemToReg, 
			We3 => signalRegWrite_WB);
			
			
	UC: control_unit port map(
			OpCode => signalInstruction_ID, 
			Branch => signalBranch_ID, 
			MemToReg => signalMemToReg_ID, 
			MemWrite => signalMemWrite_ID, 
			MemRead => signalMemRead_ID, 
			ALUSrc => signalALUSrc_ID, 
			ALUOp => signalALUOp_ID, 
			RegWrite => signalRegWrite_ID, 
			RegDst => signalRegDst_ID, 
			Jump => signalJump_ID); 
	
	ALUO:
		alu port map(
			OpA => signalRd1_EX,
			OpB =>  MUX_ALUSrc,
			Control => signalALUControl,
			Result => signalALUResult_EX,
			ZFlag => signalZFlag_EX);
	
	ALUC:
		alu_control port map(
			ALUOp => signalALUOp_EX,
			Funct => signalInstruction_EX(5 downto 0),
			ALUControl => signalALUControl);

	
	
end architecture;