
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity sort is
    Generic (WIDTH : integer := 8);
    Port ( x_in : in STD_LOGIC_VECTOR(WIDTH-1 downto 0);
           n_in : in STD_LOGIC_VECTOR(WIDTH-1 downto 0);
           dir_in : in STD_LOGIC; -- 0 za rastuce sortiranje, 1 za opadajuce
           x_out : out STD_LOGIC_VECTOR(WIDTH-1 downto 0);
           adr_out : out STD_LOGIC_VECTOR(WIDTH-1 downto 0);
           wr_out : out STD_LOGIC; -- 0 za citanje, 1 za upis
           start : in STD_LOGIC;
           ready : out STD_LOGIC;
           clk : in STD_LOGIC;
           en_out : out STD_LOGIC;
           reset : in STD_LOGIC);
end sort;

architecture Behavioral of sort is
    type state_type is (idle, lab1, lab2, lab2b, rast, opad, upis, upis2, final);
    signal state_reg, state_next : state_type;
    signal i_reg, i_next, j_reg, j_next : std_logic_vector(WIDTH-1 downto 0);
    signal x_out1_reg, x_out1_next : std_logic_vector(WIDTH-1 downto 0);
    signal x_out2_reg, x_out2_next : std_logic_vector(WIDTH-1 downto 0);
    signal sel_mem : std_logic; --za mux koji bira koji reg je povezan s memorijom, 0 za xout1, 1 za xout2
    
    --dodatni signal za optimizaciju - DELJENJE RESURSA
    signal mux_DR : std_logic_vector(WIDTH-1 downto 0);
    
    -- Atributes that need to be defined so Vivado synthesizer maps appropriate
    -- code to DSP cells
    attribute use_dsp : string;
    attribute use_dsp of Behavioral : architecture is "yes";
begin

    --dozvola koriscenja bram
    en_out <= '1';
    
    --State and data registers
    process (clk, reset) 
    begin
        if (rising_edge(clk)) then
            if (reset = '1') then
                state_reg <= idle;
                i_reg <= (others => '0');
                j_reg <= (others => '0');
                x_out1_reg <= (others => '0');
                x_out2_reg <= (others => '0');
            else
                state_reg <= state_next;
                i_reg <= i_next;
                j_reg <= j_next;
                x_out1_reg <= x_out1_next;
                x_out2_reg <= x_out2_next;
            end if;
        end if;
    end process;

    --dodatno kolo za optimizaciju - DELJENJE RESURSA
    
    mux_j_next: process (i_reg, j_reg, state_reg) is
    begin
        if state_reg = lab1 then
            mux_DR <= i_reg;
        else 
            mux_DR <= j_reg;
        end if;
    end process;
    
    --Combinatorial circuits
        
    process (i_reg, j_reg, x_out1_reg, x_out2_reg, start, mux_DR, state_reg, sel_mem, x_in, dir_in, j_next, i_next, n_in)
    begin
        --Default reg
        i_next <= i_reg;
        j_next <= j_reg;
        x_out1_next <= x_out1_reg;
        x_out2_next <= x_out2_reg;
        state_next <= idle;
        --Default out
        adr_out <= (others => '0');
        --adr1_out <= j_reg;
        --adr2_out <= i_reg;
        wr_out <= '0';
        --wr1_out <= '0';
        --wr2_out <= '0';
        ready <= '0';
        sel_mem <= '0';
        
        case state_reg is
            when idle =>
                ready <= '1';
                if start = '1' then
                    i_next <= (others => '0');
                    state_next <= lab1;
                else
                    state_next <= idle;
                end if;
            when lab1 =>
                j_next <= std_logic_vector(to_unsigned(to_integer(unsigned(mux_DR)) + 1, WIDTH));
                state_next <= lab2;
            when lab2 =>
                adr_out <= j_reg;
                --vrednost j je upisana u registar, i time i na adrese j i i
                --wr su na 0 po defaultu
                --moze iscitavanje u registre
                x_out1_next <= x_in;
                
                state_next <= lab2b;
           when lab2b =>
                adr_out <= i_reg;
                x_out2_next <= x_in;
                
                if (dir_in = '0') then
                    state_next <= rast;
                else 
                    state_next <= opad;
                end if;
            when rast =>
                if (x_out1_reg < x_out2_reg) then
                    x_out1_next <= x_out2_reg;
                    x_out2_next <= x_out1_reg;
                    
                    state_next <= upis;
                else
                    state_next <= final;
                end if;
            when opad =>
                if (x_out1_reg > x_out2_reg) then
                    x_out1_next <= x_out2_reg;
                    x_out2_next <= x_out1_reg;
       
                    state_next <= upis;
                else
                    state_next <= final;
                end if;
            when upis =>
                adr_out <= j_reg;
                --spremni su registri za upis
                --sel_mem je '0' po defaultu
                wr_out <= '1';
                
                state_next <= upis2;
           when upis2 => 
                sel_mem <= '1';
                adr_out <= i_reg;
                --spremni su registri za upis
                wr_out <= '1';
                
                state_next <= final;
            when final =>
                j_next <= std_logic_vector(to_unsigned(to_integer(unsigned(mux_DR)) + 1, WIDTH));
                
                if (j_next = n_in) then
                    i_next <= std_logic_vector(to_unsigned(to_integer(unsigned(i_reg)) + 1, WIDTH));
                    if (i_next = std_logic_vector(to_unsigned(to_integer(unsigned(n_in)) - 1, WIDTH))) then
                        state_next <= idle;
                    else
                        state_next <= lab1;
                    end if;
                else 
                    state_next <= lab2;
                end if;
            when others =>
        end case;
    end process;
    
    x_out <= x_out1_reg when sel_mem = '0' else
             x_out2_reg;
    
end Behavioral;

