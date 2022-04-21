# shell_1531.R
# The shell math model is from the paper by M.B. Cortie (1989)
# https://www.researchgate.net/publication/238757952_Models_for_mollusc_shell_shape

# Load packages
library(mathart) # devtools::install_github("marcusvolz/mathart)
library(tidyverse)
library(ggforce)
library(Rcpp)

# Input parameters dataset.
param<- read.table("shell_parameters_table1_Cortie.txt", 
                   header=T, dec= ",", sep="\t")

# Set parameters (see mathart::mollusc() documentation for details)
# Parameters are in file shell_parameters_table1_Cortie.R

#-----------------------------------------------------------------------------
TIME <- Sys.time()
#-----------------------------------------------------------------------------

# Random
n_s <- 1300L
n_t <- 4000L
n <- 1000
alpha <- 70
beta <- 35
phi <- 68
mu <- 0
Omega <- 10
s_min <- -180
s_max <- 2
A <- 7
a <- 4.3
b <- 1.0
P <- 0
W_1 <- 1
W_2 <- 1
N <- 0
L <- 0
D <- 1
theta_start <- 0
theta_end <- 15*pi

# Generate data
df <- mathart::mollusc(n_s = n_s, n_t = n_t,
              alpha = alpha, beta = beta, phi = phi, mu = mu, 
			  Omega = Omega, s_min = s_min, s_max = s_max,
              A = A, a = a, b = b, P = P, W_1 = W_1, W_2 = W_2, 
			  N = N, L = L, D = D,
              theta_start = theta_start, theta_end = theta_end)

#-----------------------------------------------------------------------------
# Create plot
sp=   "random03"
col1= "#662E2E"  # Shell color.
col2= "#FFD1D1"  # Background color.
outfile= paste("./images/", sp, col1, ".png", sep="")
outfile2= paste("./images/", sp, col1, "_3", ".png", sep="")
#-----------------------------------------------------------------------------

p <- ggplot() +
  geom_point(aes(x, z), df, size = 0.03, alpha = 0.03, color= col1) +
  geom_path(aes(x, z), df, size = 0.03, alpha = 0.03, color= col1) +
  coord_equal() +
  theme_blankcanvas(margin_cm = 0) +
  theme(plot.background = element_rect(fill = col2))

p

# Save plot
ggsave(outfile, p, width = 50, height = 50, units = "cm", dpi= 300)

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
p

ggsave(outfile2, width = 90, height = 90, units = "cm", dpi= 300)

#-----------------------------------------------------------------------------
Sys.time() - TIME
#-----------------------------------------------------------------------------
