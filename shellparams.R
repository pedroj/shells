#------------------------------------------------------------------------------
# Set parameters (see mathart::mollusc() documentation for details)
# The shell math model is from the paper by M.B. Cortie (1989)
# https://www.researchgate.net/publication/238757952_Models_for_mollusc_shell_shape
# Table 1. Parameter values.
# shell_parameters_table1_Cortie.txt
# Dataset
#------------------------------------------------------------------------------
shell.params<- read.table("shell_parameters_table1_Cortie.txt", header = T, sep = "\t", 
		   dec = ".", as.is = FALSE, na.strings = "NA",
		   colClasses = NA, nrows = -1,
		   skip = 0, check.names = TRUE,
		   strip.white = FALSE,
		   comment.char = "#")
