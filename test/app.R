#------------------------------------------------------------------------------	
library(shiny)
# Load packages
library(mathart) # devtools::install_github("marcusvolz/mathart)
library(tidyverse)
library(ggforce)
library(Rcpp)
#------------------------------------------------------------------------------
# Define UI for application that draws a shell
ui <- fluidPage(
    # Application title
    titlePanel("Models of shells (Mollusca)"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        column(3,
        	   numericInput("n_s",
        				 "n_s: No. points relative to s:",
        				 value = 350)
        ),
        column(3,
        	   numericInput("n_t",
        	   			 "n_t: No. points relative to theta:",
        	   			 value = 1000)
        ),
        column(3,
        	   numericInput("n",
        	   			 "n:",
        	   			 value = 1000)
        ),
        column(3,
        	   numericInput("alpha",
        	   			"alpha: Equiangular angle, spiral (deg):",
        	   			min = 1,
        	   			max = 150,
        	   			value = 87)
        ),
        column(3, # offset = 1,
        	   numericInput("beta",
        	   			"beta: Angle z-axis - line from aperture 
    		   			 local origin to xyz origin (deg):",
    		   			min = 1,
    		   			max = 150,
    		   			value = 7),
        ),
        column(3,
        	   numericInput("phi",
        	   			"phi: Tilt of ellipse major axis 
        	   			 from horizontal plane (deg):",
    		   			min = 1,
    		   			max = 300,
    		   			value = 78),
        ),
        column(3,
        	   numericInput("mu",
        	   			"mu: Leaning over of aperture (deg):",
        	   			min = 0,
        	   			max = 300,
        	   			value = 0),
        ),
        column(3,
        	   numericInput("Omega",
        	   			"Omega: Amzimuthal rotation, aperture (deg):",
        	   			min = 0,
        	   			max = 300,
        	   			value = 0),
        ),
        column(3,
        	   numericInput("s_min",
        	   			"s_min: Angle at which aperture-generating 
        	   			 curve begins (deg):",
    		   			min = -300,
    		   			max = 300,
    		   			value = -180),
        ),
        column(3,
        	   numericInput("s_max",
        	   			"s_max: Angle at which aperture-generating 
        	   			 curve ends (deg):",
    		   			min = 1,
    		   			max = 300,
    		   			value = 2),
        ),
        column(3,
        	   numericInput("A",
        	   			"A: Distance from main origin to local
        	   			 origin of aperture at theta=0:",
    		   			min = 1,
    		   			max = 500,
    		   			value = 7),
        ),
        column(3,
        	   numericInput("a",
        	   			"a: Major radius, ellipse at theta=0:",
        	   			min = 1,
        	   			max = 500,
        	   			value = 4.3),
        ),
        column(3,
        	   numericInput("b",
        	   			"b: Minor radius, ellipse at theta=0:",
        	   			min = 0,
        	   			max = 500,
        	   			value = 1.0),
        ),
        column(3,
        	   numericInput("P",
        	   			 "P: Position of nodule in 
    		   			  angle, s (degrees):",
    		   			value = 80, min = 1,max = 180)
        ),
        column(3,
        	   numericInput("W_1",
        	   			 "W_1: Width of nodule, 
        	   			  s-direction (deg):",
        	   			 min = 0,
        	   			 max = 180,
        	   			 value = 0),
        ),
        column(3,
        	   numericInput("W_2",
        	   			 "W_2: Width of nodule, 
        	   			  theta-direction (deg):",
        	   			 min = 1,
        	   			 max = 100,
        	   			 value = 1),
        ),
        column(3,
        	   numericInput("N",
        	   			 "N: No. nodules per whorl:",
        	   			 min = 0,
        	   			 max = 100,
        	   			 value = 0),
        ),
        column(3,
        	   numericInput("L",
        	   			 "L: Height of nodule at theta=0:",
        	   			 min = 0,
        	   			 max = 100,
        	   			 value = 0),
        ),
        column(3,
        	   numericInput("D",
        	   			 "D: Sense of coiling; 
        	   			  1=dextral, -1=sinistral:",
        	   			 min = -1,
        	   			 max = 1,
        	   			 value = 1),
        ),
        column(3,
        	   numericInput("theta_start",
        	   			 "theta_start: For Dentalium or Diodora. 
    		   			  In all other cases set to -Inf:",
    		   			value = 0),
        ),
        column(3,
        	   numericInput("theta_end",
        	   			 "theta_end: Unimportant for 
        	   			  self-similar shells:",
    		   			value = 30*pi),
       br(),
	   column(2,
        	   	   submitButton("Update View", icon("refresh")),
        	   ),
        )),
        # Show a plot of the generated model
        mainPanel(
        	column(8,
        		   plotOutput("distPlot"))
        )
))
#------------------------------------------------------------------------------
# Define server logic required to draw a shell
server <- function(input, output, session) {
	output$distPlot <- renderPlot({
	#--------------------------------------------------------------------------
	# Code for shells.
	#--------------------------------------------------------------------------
	# Generate data
	df <- mathart::mollusc(n_s = input$n_s, n_t = input$n_t,
						   alpha = input$alpha, beta = input$beta, 
						   phi = input$phi, mu = input$mu, 
						   Omega = input$Omega, s_min = input$s_min, 
						   s_max = input$s_max,
						   A = input$A, a = input$a, b = input$b, P = input$P, 
						   W_1 = input$W_1, W_2 = input$W_2, 
						   N = input$N, L = input$L, D = input$D,
						   theta_start = input$theta_start, 
						   theta_end = input$theta_end)
	#--------------------------------------------------------------------------
	# Create plot
	sp=   "conus_12"
	col1= "#00000000"  # Shell color.
	col2= "#FFFFFFFF"  # Background color.
	outfile= paste("./images/", sp, col1, ".png", sep="")
	outfile2= paste("./images/", sp, col1, "_3", ".png", sep="")
	#--------------------------------------------------------------------------
	#
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
#	options(repr.plot.width= 80, repr.plot.height= 80)
	ggplot() +
		geom_point(aes(x, z), df, size = 0.03, alpha = 0.03, color= col1) +
		geom_path(aes(x, z), df, linewidth = 0.03, alpha = 0.03, color= col1) +
		coord_equal() +
		theme_blankcanvas(margin_cm = 0) +
		theme(plot.background = element_rect(fill = col2))
#	distPlot # The plot.
	# Save plot
	#ggsave(outfile, distPlot, width = 60, height = 60, units = "cm", dpi= 300)
	#--------------------------------------------------------------------------
	},   width = 750, height = 750, res = 150)
}
# Run the application 
shinyApp(ui = ui, server = server)
#------------------------------------------------------------------------------
#



