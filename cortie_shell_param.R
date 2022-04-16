# shell_1531.R
# The shell math model is from the paper by M.B. Cortie (1989)
# https://www.researchgate.net/publication/238757952_Models_for_mollusc_shell_shape

# Load packages
library(mathart) # devtools::install_github("marcusvolz/mathart)
library(tidyverse)
library(ggforce)
library(Rcpp)

# Set parameters (see mathart::mollusc() documentation for details)
# Parameters are in file shell_parameters_table1_Cortie.R

# Oxystele
n_s <- 650L
n_t <- 2000L
n <- 1000
alpha <- 84.9
beta <- 7
phi <- -36
mu <- 0
Omega <- 0
s_min <- -70
s_max <- 70
A <- 47
a <- 40
b <- 19
P <- 0
W_1 <- 1
W_2 <- 1
N <- 0
L <- 0
D <- 1
theta_start <- 0
theta_end <- 18*pi

# Generate data
df <- mollusc(n_s = n_s, n_t = n_t,
              alpha = alpha, beta = beta, phi = phi, mu = mu, 
			  Omega = Omega, s_min = s_min, s_max = s_max,
              A = A, a = a, b = b, P = P, W_1 = W_1, W_2 = W_2, 
			  N = N, L = L, D = D,
              theta_start = theta_start, theta_end = theta_end)

# Create plot
p <- ggplot() +
  geom_point(aes(x, z), df, size = 0.03, alpha = 0.3, color= "#812527") +
  geom_path(aes(x, z), df, size = 0.03, alpha = 0.3, color= "#812527") +
  coord_equal() +
  theme_blankcanvas(margin_cm = 0) +
  theme(plot.background = element_rect(fill = "#E8D0D1"))

# p

# Save plot
ggsave("./images/oxystele01#812527_1.png", p, 
	   width = 40, height = 40, units = "cm", dpi= 300)

#-----------------------------------------------------------------------------
# Plot the three X, Y, Z views of the shell
# df <- mollusc()
df1 <- df %>% mutate(id = 1)
df2 <- df %>% mutate(id = 2)
df3 <- df %>% mutate(id = 3)

p <- ggplot() +
	geom_point(aes(x, y), df1, size = 0.03, alpha = 0.03) +
	geom_path( aes(x, y), df1, size = 0.03, alpha = 0.03) +
	geom_point(aes(x, z), df2, size = 0.03, alpha = 0.03) +
	geom_path( aes(x, z), df2, size = 0.03, alpha = 0.03) +
	geom_point(aes(y, z), df3, size = 0.03, alpha = 0.03) +
	geom_path( aes(y, z), df3, size = 0.03, alpha = 0.03) +
	facet_wrap(~id, nrow = 2, scales = "free") +
	theme(plot.background = element_rect(fill = "#E8D0D1")) +
	theme_blankcanvas(margin_cm = 0.5)
p

ggsave("./images/oxystele01#812527_3.png", 
	   width = 80, height = 80, units = "cm", dpi= 300)

