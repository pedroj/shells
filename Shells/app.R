#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mathart) # devtools::install_github("marcusvolz/mathart)
library(tidyverse)
library(ggforce)
library(Rcpp)

# Define functions
#------------------------------------------------------------------------------
# Function to generate shells.
#------------------------------------------------------------------------------
get.shell <- function(n_s= 650L, n_t= 2000L,n= 1000, alpha, beta, phi, mu, 
					  Omega, s_min, s_max, A, a, b, P, W_1= 1, W_2= 1, N= 0, 
					  L= 0, D= 1, theta_start= -Inf, theta_end= 8*pi, x, z, 
					  sp, col1, col2, myplot= 0) {
	#--------------------------------------------------------------------------
	TIME <- Sys.time()
	#--------------------------------------------------------------------------
	# Generate data
	df <- mathart::mollusc(n_s = n_s, n_t = n_t,
						   alpha = alpha, beta = beta, phi = phi, mu = mu, 
						   Omega = Omega, s_min = s_min, s_max = s_max,
						   A = A, a = a, b = b, P = P, W_1 = W_1, W_2 = W_2, 
						   N = N, L = L, D = D,
						   theta_start = theta_start, theta_end = theta_end)
	#--------------------------------------------------------------------------
	# Create plot
	sp=   sp
	col1= col1  # Shell color.
	col2= col2  # Background color.
	#outfile= paste("./images/", sp, col1, ".png", sep="")
	#outfile2= paste("./images/", sp, col1, "_3", ".png", sep="")
	#--------------------------------------------------------------------------
	
	distPlot <- ggplot() +
		geom_point(aes(x, z), df, size = 0.05, alpha = 0.05, color= col1) +
		geom_path(aes(x, z), df, linewidth = 0.03, alpha = 0.03, color= col1) +
		coord_equal() +
		theme_blankcanvas(margin_cm = 0) +
		theme(plot.background = element_rect(fill = col2))
	#p
	
	# Save plot
	# ggsave(outfile, p, width = 60, height = 60, units = "cm", dpi= 300)
	#--------------------------------------------------------------------------
	Sys.time() - TIME
	#--------------------------------------------------------------------------
}

theme_blankcanvas <- function(bg_col = "transparent", margin_cm = 2.5) {
	theme(axis.title = element_blank(),
		  axis.text = element_blank(),
		  axis.ticks = element_blank(),
		  axis.line = element_blank(),
		  legend.position = "none",
		  panel.background = element_rect(fill = bg_col, colour = bg_col),
		  panel.border = element_blank(),
		  panel.grid = element_blank(),
		  plot.background = element_rect(fill = bg_col, colour = bg_col),
		  plot.margin = unit(rep(margin_cm, 4), "cm"), # top, right, bottom, left
		  strip.background = element_blank(),
		  strip.text = element_blank())
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Models for Shells"),
    plotOutput('distPlot'),
    
    hr(),
    
    fluidRow(
    	column(3,
    	#	   h4("Shell parameters"),
    		   sliderInput("alpha",
    		   			"Equiangular angle of spiral (degrees):",
    		   			min = 1,
    		   			max = 100,
    		   			value = 80)
    	),
    	column(4, offset = 1,
    		   sliderInput("beta",
    		   			"Angle between z-axis and line from aperture 
    		   			local origin to xyz origin (degrees):",
    		   			min = 1,
    		   			max = 100,
    		   			value = 80),
    	),
    	column(4,
    		   sliderInput("phi",
    		   			"Tilt of ellipse major axis from horizontal 
    		   			plane (degrees):",
    		   			min = 1,
    		   			max = 100,
    		   			value = 80),
    	),
    	column(4,
    		   sliderInput("mu",
    		   			"Amount of "leaning over" of aperture (degrees):",
    		   			min = 1,
    		   			max = 100,
    		   			value = 80),
    	),
    	column(4,
    		   sliderInput("Omega",
    		   			"Amount of azimuthal rotation of aperture (degrees):",
    		   			min = 1,
    		   			max = 100,
    		   			value = 80),
    	),
    	column(4,
    		   sliderInput("s_min",
    		   			"Angle at which aperture-generating curve 
    		   			begins (degrees):",
    		   			min = 1,
    		   			max = 100,
    		   			value = 80),
    	),
    	column(4,
    		   sliderInput("s_max",
    		   			"Angle at which aperture-generating curve 
    		   			ends (degrees):",
    		   			min = 1,
    		   			max = 100,
    		   			value = 80),
    	),
    	column(4,
    		   sliderInput("A",
    		   			"Distance from main origin to local origin of 
    		   			aperture at theta=0:",
    		   			min = 1,
    		   			max = 100,
    		   			value = 80),
    	),
    	column(4,
    		   sliderInput("phi",
    		   			"Tilt of ellipse major axis from horizontal 
    		   			plane (degrees):",
    		   			min = 1,
    		   			max = 100,
    		   			value = 80),
    	),
    	column(4,
    		   sliderInput("phi",
    		   			"Tilt of ellipse major axis from horizontal 
    		   			plane (degrees):",
    		   			min = 1,
    		   			max = 100,
    		   			value = 80),
    	),
    	column(4,
    		   sliderInput("phi",
    		   			"Tilt of ellipse major axis from horizontal 
    		   			plane (degrees):",
    		   			min = 1,
    		   			max = 100,
    		   			value = 80),
    	),
    )
    )
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

# Define server logic required to draw the shells
server <- function(input, output) {

    output$distPlot <- renderPlot({
    	#-----------------------------------
    	get.shell(
    		# lyria
    		n_s = 650L,
    		n_t = 2000L,
    		n = 1000,
    		alpha = 83.9,
    		beta = -19,
    		phi = 45,
    		mu = 0,
    		Omega = 0,
    		s_min = -51,
    		s_max = 9,
    		A = 50,
    		a = 40,
    		b = 14,
    		P = 0,
    		W_1 = 6,
    		W_2 = 27,
    		N = 8,
    		L = 4,
    		D = 1,
    		theta_start = 0,
    		theta_end = 14.2*pi,
    		#--------------------------------
    		sp= "lyria08",
    		col1= "#EAE22D", # Shell color. 
    		col2= "#06056D") # Bckgnd color.
    	#------------------------------------
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
