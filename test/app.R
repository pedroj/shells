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
        column(4,
        		   numericInput("n_s",
        				 "Number of points to generate with respect to s:",
        				 value = 650L)
        ),
        column(4,
        	   numericInput("n_t",
        	   			 "Number of points to generate with respect to theta:",
        	   			 value = 2000L)
        ),
        column(4,
        	   numericInput("n",
        	   			 "n:",
        	   			 value = 1000)
        ),
        column(4,
        	   sliderInput("alpha",
        	   			"Equiangular angle of spiral (degrees):",
        	   			min = 1,
        	   			max = 100,
        	   			value = 87)
        ),
        column(4, # offset = 1,
        	   sliderInput("beta",
        	   			"Angle between z-axis and line from aperture 
    		   			local origin to xyz origin (degrees):",
    		   			min = 1,
    		   			max = 100,
    		   			value = 7),
        ),
        column(4,
        	   sliderInput("phi",
        	   			"Tilt of ellipse major axis from horizontal 
    		   			plane (degrees):",
    		   			min = 1,
    		   			max = 100,
    		   			value = 78),
        ),
        column(4,
        	   sliderInput("mu",
        	   			"Amount of leaning over of aperture (degrees):",
        	   			min = 0,
        	   			max = 100,
        	   			value = 0),
        ),
        column(4,
        	   sliderInput("Omega",
        	   			"Amount of azimuthal rotation of aperture (degrees):",
        	   			min = 0,
        	   			max = 100,
        	   			value = 0),
        ),
        column(4,
        	   sliderInput("s_min",
        	   			"Angle at which aperture-generating curve 
    		   			begins (degrees):",
    		   			min = -300,
    		   			max = 300,
    		   			value = -180),
        ),
        column(4,
        	   sliderInput("s_max",
        	   			"Angle at which aperture-generating curve 
    		   			ends (degrees):",
    		   			min = 1,
    		   			max = 100,
    		   			value = 2),
        ),
        column(4,
        	   sliderInput("A",
        	   			"Distance from main origin to local origin of 
    		   			aperture at theta=0:",
    		   			min = 1,
    		   			max = 100,
    		   			value = 7),
        ),
        column(4,
        	   sliderInput("a",
        	   			"Major radius of ellipse at theta=0:",
        	   			min = 1,
        	   			max = 100,
        	   			value = 4.3),
        ),
        column(4,
        	   sliderInput("b",
        	   			"Minor radius of ellipse at theta=0:",
        	   			min = 1,
        	   			max = 100,
        	   			value = 1.0),
        ),
        column(4,
        	   numericInput("P",
        	   			 "Position of nodule in terms of the 
    		   			angle, s (degrees):",
    		   			value = 80, min = 1,max = 100)
        ),
        column(4,
        	   numericInput("W_1",
        	   			 "Width of nodule in s-direction (degrees):",
        	   			 min = 1,
        	   			 max = 100,
        	   			 value = 0),
        ),
        column(4,
        	   numericInput("W_1",
        	   			 "Width of nodule in theta-direction (degrees):",
        	   			 min = 1,
        	   			 max = 100,
        	   			 value = 1),
        ),
        column(4,
        	   numericInput("N",
        	   			 "Number of nodules per whorl:",
        	   			 min = 1,
        	   			 max = 100,
        	   			 value = 1),
        ),
        column(4,
        	   numericInput("L",
        	   			 "Height of nodule at theta=0:",
        	   			 min = 1,
        	   			 max = 100,
        	   			 value = 0),
        ),
        column(4,
        	   numericInput("D",
        	   			 "Sense of coiling; 1=dextral, -1=sinistral:",
        	   			 min = -1,
        	   			 max = 1,
        	   			 value = 1),
        ),
        column(4,
        	   numericInput("theta_start",
        	   			 "Required for shells such as Dentalium or Diodora, 
    		   			which grow at one end while dissolving at the other. 
    		   			In all other cases set to -Inf:",
    		   			min = 0,
    		   			max = 100,
    		   			value = 0),
        ),
        column(4,
        	   numericInput("theta_end",
        	   			 "Not important for self-similar shells, except very 
    		   			small or large values can cause computational 
    		   			problems:",
    		   			min = 5*pi,
    		   			max = 90*pi,
    		   			value = 30*pi),
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
	#
	library(mathart) # devtools::install_github("marcusvolz/mathart)
	library(tidyverse)
	library(ggforce)
	library(Rcpp)
	#
	#------------------------------------------------------------------------------
	# Code for shells.
	#------------------------------------------------------------------------------
		# Generate data
		df <- mathart::mollusc(n_s = n_s, n_t = n_t,
							   alpha = alpha, beta = beta, phi = phi, mu = mu, 
							   Omega = Omega, s_min = s_min, s_max = s_max,
							   A = A, a = a, b = b, P = P, W_1 = W_1, W_2 = W_2, 
							   N = N, L = L, D = D,
							   theta_start = theta_start, theta_end = theta_end)
		#-------------------------------------------
		sp= "lyria08"
		col1= "#EAE22D" # Shell color. 
			col2= "#06056D"      #) # Bckgnd color.
		#-------------------------------------------
		#--------------------------------------------------------------------------
		# Create plot
		sp=   sp
		col1= col1  # Shell color.
		col2= col2  # Background color.
		#outfile= paste("./images/", sp, col1, ".png", sep="")
		#outfile2= paste("./images/", sp, col1, "_3", ".png", sep="")
		#--------------------------------------------------------------------------
		ggplot() +
			geom_point(aes(x, z), df, size = 0.05, alpha = 0.05, color= col1) +
			geom_path(aes(x, z), df, linewidth = 0.03, alpha = 0.03, color= col1) +
			coord_equal() +
			mytheme_blankcanvas(margin_cm = 0) +
			theme(plot.background = element_rect(fill = col2))
		# Save plot
		# ggsave(outfile, p, width = 60, height = 60, units = "cm", dpi= 300)
		#--------------------------------------------------------------------------
	}
	mytheme_blankcanvas <- function(bg_col = "transparent", margin_cm = 2.5) {
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
#------------------------------------------------------------------------------
# Run the application 
shinyApp(ui = ui, server = server)
#------------------------------------------------------------------------------
#



