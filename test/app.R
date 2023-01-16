#------------------------------------------------------------------------------	
library(shiny)
#------------------------------------------------------------------------------
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Models of shells (Mollusca)"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        column(3,
        		   numericInput("n_s",
        				 "n_s: Number of points to generate with respect to s:",
        				 value = 650)
        ),
        column(3,
        	   numericInput("n_t",
        	   			 "n_t: Number of points to generate with respect to theta:",
        	   			 value = 2000)
        ),
        column(3,
        	   numericInput("n",
        	   			 "n:",
        	   			 value = 1000)
        ),
        column(3,
        	   sliderInput("alpha",
        	   			"alpha: Equiangular angle of spiral (degrees):",
        	   			min = 1,
        	   			max = 100,
        	   			value = 87)
        ),
        column(3, # offset = 1,
        	   sliderInput("beta",
        	   			"beta: Angle between z-axis and line from aperture 
    		   			local origin to xyz origin (degrees):",
    		   			min = 1,
    		   			max = 100,
    		   			value = 7),
        ),
        column(3,
        	   sliderInput("phi",
        	   			"phi: Tilt of ellipse major axis from horizontal 
    		   			plane (degrees):",
    		   			min = 1,
    		   			max = 100,
    		   			value = 78),
        ),
        column(3,
        	   sliderInput("mu",
        	   			"mu: Amount of leaning over of aperture (degrees):",
        	   			min = 0,
        	   			max = 100,
        	   			value = 0),
        ),
        column(3,
        	   sliderInput("Omega",
        	   			"Omega: Amount of azimuthal rotation of aperture (degrees):",
        	   			min = 0,
        	   			max = 100,
        	   			value = 0),
        ),
        column(3,
        	   sliderInput("s_min",
        	   			"s_min: Angle at which aperture-generating curve 
    		   			begins (degrees):",
    		   			min = -300,
    		   			max = 300,
    		   			value = -180),
        ),
        column(3,
        	   sliderInput("s_max",
        	   			"s_max: Angle at which aperture-generating curve 
    		   			ends (degrees):",
    		   			min = 1,
    		   			max = 100,
    		   			value = 2),
        ),
        column(3,
        	   sliderInput("A",
        	   			"A: Distance from main origin to local origin of 
    		   			aperture at theta=0:",
    		   			min = 1,
    		   			max = 100,
    		   			value = 7),
        ),
        column(3,
        	   sliderInput("a",
        	   			"a: Major radius of ellipse at theta=0:",
        	   			min = 1,
        	   			max = 100,
        	   			value = 4.3),
        ),
        column(3,
        	   sliderInput("b",
        	   			"b: Minor radius of ellipse at theta=0:",
        	   			min = 1,
        	   			max = 100,
        	   			value = 1.0),
        ),
        column(3,
        	   numericInput("P",
        	   			 "P: Position of nodule in terms of the 
    		   			angle, s (degrees):",
    		   			value = 80, min = 1,max = 100)
        ),
        column(3,
        	   numericInput("W_1",
        	   			 "W_1: Width of nodule in s-direction (degrees):",
        	   			 min = 1,
        	   			 max = 100,
        	   			 value = 0),
        ),
        column(3,
        	   numericInput("W_2",
        	   			 "W_2: Width of nodule in theta-direction (degrees):",
        	   			 min = 1,
        	   			 max = 100,
        	   			 value = 1),
        ),
        column(3,
        	   numericInput("N",
        	   			 "N: Number of nodules per whorl:",
        	   			 min = 1,
        	   			 max = 100,
        	   			 value = 1),
        ),
        column(3,
        	   numericInput("L",
        	   			 "L: Height of nodule at theta=0:",
        	   			 min = 1,
        	   			 max = 100,
        	   			 value = 0),
        ),
        column(3,
        	   numericInput("D",
        	   			 "D: Sense of coiling; 1=dextral, -1=sinistral:",
        	   			 min = -1,
        	   			 max = 1,
        	   			 value = 1),
        ),
        column(3,
        	   numericInput("theta_start",
        	   			 "theta_start: Required for shells such as Dentalium 
        	   			  or Diodora, which grow at one end while dissolving 
        	   			  at the other. 
    		   			  In all other cases set to -Inf:",
    		   			min = 0,
    		   			max = 100,
    		   			value = 0),
        ),
        column(3,
        	   numericInput("theta_end",
        	   			 "theta_end: Not important for self-similar shells, 
        	   			  except very small or large values can cause 
        	   			  computational problems:",
    		   			min = 15.708,
    		   			max = 282.74,
    		   			value = 94.248),
        )),
        # Show a plot of the generated model
        mainPanel(
        column(8,
        	   plotOutput("distPlot"))
        )
))
#------------------------------------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output) {
	#--------------------------------------------------------------------------
	# Code for shells.
	#--------------------------------------------------------------------------
	# Load packages
	library(mathart) # devtools::install_github("marcusvolz/mathart)
	library(tidyverse)
	library(ggforce)
	library(Rcpp)
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
	sp=   "conus_11"
	col1= "#000000ff"  # Shell color.
	col2= "#EBFFEBC3"  # Background color.
	#outfile= paste("./images/", sp, col1, ".png", sep="")
	#outfile2= paste("./images/", sp, col1, "_3", ".png", sep="")
	#--------------------------------------------------------------------------
	ggplot() +
		geom_point(aes(x, z), df, size = 0.03, alpha = 0.03, color= col1) +
		geom_path(aes(x, z), df, size = 0.03, alpha = 0.03, color= col1) +
		coord_equal() +
		theme_blankcanvas(margin_cm = 0) +
		theme(plot.background = element_rect(fill = col2))
	
	withProgress(message = 'Calculation in progress',
				 detail = 'This may take a while...', value = 0, {
				 	for (i in 1:15) {
				 		incProgress(1/15)
				 		Sys.sleep(0.25)
				 	}
				 })
	#--------------------------------------------------------------------------
}
# Run the application 
shinyApp(ui = ui, server = server)
#------------------------------------------------------------------------------
#



