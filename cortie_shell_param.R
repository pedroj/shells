# shell.R
# The shell math model is from the paper by M.B. Cortie (1989)
# https://www.researchgate.net/publication/238757952_Models_for_mollusc_shell_shape

# Load packagess
library(mathart) # devtools::install_github("marcusvolz/mathart)
library(tidyverse)
library(ggforce)
library(Rcpp)

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
#-----------------------------------------------------------------------------
TIME <- Sys.time()
#-----------------------------------------------------------------------------
# Conus
n_s = 650L
n_t = 2000L
n = 1000
alpha = 86
beta = 8
phi = 78
mu = 0
Omega = 0
s_min = -180
s_max = 2
A = 7
a = 4.3
b = 1.0
P = 0
W_1 = 1
W_2 = 1
N = 0
L = 0
D = 1
theta_start = 0
theta_end = 30*pi

# Generate data
df <- mathart::mollusc(n_s = n_s, n_t = n_t,
              alpha = alpha, beta = beta, phi = phi, mu = mu, 
			  Omega = Omega, s_min = s_min, s_max = s_max,
              A = A, a = a, b = b, P = P, W_1 = W_1, W_2 = W_2, 
			  N = N, L = L, D = D,
              theta_start = theta_start, theta_end = theta_end)

#-----------------------------------------------------------------------------
# Create plot
sp=   "conus_11"
col1= "#000000ff"  # Shell color.
col2= "#EBFFEBC3"  # Background color.
outfile= paste("./images/", sp, col1, ".png", sep="")
outfile2= paste("./images/", sp, col1, "_3", ".png", sep="")
#-----------------------------------------------------------------------------

p <- ggplot() +
  geom_point(aes(x, z), df, size = 0.03, alpha = 0.03, color= col1) +
  geom_path(aes(x, z), df, size = 0.03, alpha = 0.03, color= col1) +
  coord_equal() +
  theme_blankcanvas(margin_cm = 0) +
  theme(plot.background = element_rect(fill = col2))

# p

# Save plot
ggsave(outfile, p, width = 60, height = 60, units = "cm", dpi= 300)

#-----------------------------------------------------------------------------
Sys.time() - TIME
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# Plot the three X, Y, Z views of the shell
# df <- mollusc()
df1 <- df %>% mutate(id = 1)
df2 <- df %>% mutate(id = 2)
df3 <- df %>% mutate(id = 3)

p <- ggplot() +
	geom_point(aes(x, y), df1, size = 0.03, alpha = 0.03, color= col1) +
	geom_path( aes(x, y), df1, size = 0.03, alpha = 0.03, color= col1) +
	geom_point(aes(x, z), df2, size = 0.03, alpha = 0.03, color= col1) +
	geom_path( aes(x, z), df2, size = 0.03, alpha = 0.03, color= col1) +
	geom_point(aes(y, z), df3, size = 0.03, alpha = 0.03, color= col1) +
	geom_path( aes(y, z), df3, size = 0.03, alpha = 0.03, color= col1) +
	facet_wrap(~id, nrow = 2, scales = "free") +
	theme_blankcanvas(margin_cm = 0.5)	 +
    theme(plot.background = element_rect(fill = col2))
# p

ggsave(outfile2, width = 90, height = 110, units = "cm", dpi= 300)

#-----------------------------------------------------------------------------
Sys.time() - TIME
#-----------------------------------------------------------------------------
