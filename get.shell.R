# Input parameters dataset.
# param<- read.table(here("shell_parameters_table1_Cortie.txt"), 
#                    header=T, dec= ",", sep="\t")

# Set parameters (see mathart::mollusc() documentation for details)
# Parameters are in file shell_parameters_table1_Cortie.R

#-----------------------------------------------------------------------------
# Description:
#      Generates data for plotting mollusc shells and stores it in a data
#      frame with (x, y, z) coordinates. The shell model is described in
#      the paper "Models for mollusc shell shape" by M.B. Cortie (1989).
# Arguments:
#      n_s: Number of points to generate with respect to s
#      n_t: Number of points to generate with respect to theta
#    alpha: Equiangular angle of spiral (degrees)
#     beta: Angle between z-axis and line from aperture local origin to
#           xyz origin (degrees)
#      phi: Tilt of ellipse major axis from horizontal plane (degrees)
#       mu: Amount of "leaning over" of aperture (degrees)
#    Omega: Amount of azimuthal rotation of aperture (degrees)
#    s_min: Angle at which aperture-generating curve begins (degrees)
#    s_max: Angle at which aperture-generating curve ends (degrees)
#        A: Distance from main origin to local origin of aperture at
#           theta=0
#        a: Major radius of ellipse at theta=0
#        b: Minor radius of ellipse at theta=0
#        P: Position of nodule in terms of the angle, s (degrees)
#      W_1: Width of nodule in s-direction (degrees)
#      W_2: Width of nodule in theta-direction (degrees)
# #        N: Number of nodules per whorl
# #        L: Height of nodule at theta=0
# #        D: Sense of coiling; 1=dextral, -1=sinistral
# # theta_start: Required for shells such as Dentalium or Diodora, which
#           grow at one end while dissolving at the other. In all other
#           cases set to -Inf
# # theta_end: Not important for self-similar shells, except very small or
#           large values can cause computational problems
# ----------------------------------------------------------------------------
# Load packagess
library(mathart) # devtools::install_github("marcusvolz/mathart)
library(tidyverse)
library(ggforce)
library(Rcpp)
#------------------------------------------------------------------------------
get.shell(
	# Shell shell_1531.R
	n_s = 650L,
	n_t = 2000L,
	n = 1000,
	alpha = 99.6,
	beta = 0.515,
	phi = 1.3,
	mu = 5,
	Omega = 2,
	s_min = -193.8,
	s_max = 69.4,
	A = 7.031,
	a = 2.377,
	b = 6.42,
	P = 0,
	W_1 = 0,
	W_2 = 0,
	N = 0,
	L = 0,
	D = 1,
	theta_start = 0,
	theta_end = 10*pi,
    #-------------------------------------------
		  sp= "random_1531_#01",
		  col1= "#FFEE00",   # Shell color.
		  col2= "#1D1F5E")    # Bckgnd color.
	#-------------------------------------------
#------------------------------------------------------------------------------




#-----------------------------------------------------------------------------
# Function to generate shells.
#-----------------------------------------------------------------------------
get.shell <- function(n_s= 650L, n_t= 2000L,n= 1000, alpha, beta, phi, mu, Omega, s_min, s_max, A, a, b, P, W_1= 1, W_2= 1, N= 0, L= 0, D= 1, theta_start= -Inf, theta_end= 8*pi, x, z, sp, col1, col2, myplot= 0) {
	#-----------------------------------------------------------------------------
	TIME <- Sys.time()
	#-----------------------------------------------------------------------------
	# Generate data
	df <- mathart::mollusc(n_s = n_s, n_t = n_t,
  					   alpha = alpha, beta = beta, phi = phi, mu = mu, 
  					   Omega = Omega, s_min = s_min, s_max = s_max,
  					   A = A, a = a, b = b, P = P, W_1 = W_1, W_2 = W_2, 
  					   N = N, L = L, D = D,
  					   theta_start = theta_start, theta_end = theta_end)
	#-----------------------------------------------------------------------------
	# Create plot
	sp=   sp
	col1= col1  # Shell color.
	col2= col2  # Background color.
	outfile= paste("./images/", sp, col1, ".png", sep="")
	outfile2= paste("./images/", sp, col1, "_3", ".png", sep="")
	#-----------------------------------------------------------------------------
  
	p <- ggplot() +
  	geom_point(aes(x, z), df, size = 0.05, alpha = 0.05, color= col1) +
  	geom_path(aes(x, z), df, size = 0.03, alpha = 0.03, color= col1) +
  	coord_equal() +
  	theme_blankcanvas(margin_cm = 0) +
  	theme(plot.background = element_rect(fill = col2))

		# p
  
	# Save plot
	ggsave(outfile, p, width = 50, height = 50, units = "cm", dpi= 300)
	#-----------------------------------------------------------------------------
	Sys.time() - TIME
	#-----------------------------------------------------------------------------
}

