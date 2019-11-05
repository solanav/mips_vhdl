if [file exists work] {
   vdel -lib work -all
}
vlib work

vcom -work work -2002 -explicit -check_synthesis ../rtl/reg_bank.vhd
vcom -work work -2002 -explicit -check_synthesis ../rtl/alu.vhd
vcom -work work -2002 -explicit -check_synthesis ../rtl/alu_control.vhd
vcom -work work -2002 -explicit -check_synthesis ../rtl/control_unit.vhd
vcom -work work -2002 -explicit -check_synthesis ../rtl/processor.vhd