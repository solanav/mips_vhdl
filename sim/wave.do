--------------------------------------------------------------------------------
# Arq2019-2020. En principio solo clk y reset. Agregar señales relevantes
--------------------------------------------------------------------------------
add wave -noupdate /processor_tb/i_processor/Clk
add wave -noupdate /processor_tb/i_processor/Reset

add wave -position end sim:/processor_tb/i_processor/u4/*
add wave -position end sim:/processor_tb/i_processor/u2/*
add wave -position end sim:/processor_tb/i_processor/u3/*
add wave -position end sim:/processor_tb/i_processor/u1/*

add wave -position 2  sim:/processor_tb/iAddr
add wave -position 3  sim:/processor_tb/dDataOut
add wave -position 3  sim:/processor_tb/dDataIn
add wave -position 4  sim:/processor_tb/i_processor/READDATA_MEMWB