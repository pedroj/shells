# Dcouemnatition for Shells.

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
	titlePanel("Documentation for Shells app          "),
	sidebarLayout(
		sidebarPanel(
			img(src = "shell.png", height = 570, width = 570),
			br(),
			img(src = "params.png", height = 480, width = 540)
                    ), 
	mainPanel(
			column(8,
				   h2("Shell parameters"),
				   p("These are the 17 parameters used for modelling shells."),
				   p("The shell math model is from the paper by ",
				     a("M.B. Cortie (1989)", 
				       href = "https://www.researchgate.net/publication/238757952_Models_for_mollusc_shell_shape")),
				   br(),
				   p("Generates data for plotting mollusc shells and stores it in a data
     frame with (x, y, z) coordinates. The shell model is described in
     the paper 'Models for mollusc shell shape' by M.B. Cortie (1989)."),
     p("Arguments:"),
     p("     n_s: Number of points to generate with respect to s"),
     p("     n_t: Number of points to generate with respect to theta"),
     p("   alpha: Equiangular angle of spiral (degrees)"),
     p("    beta: Angle between z-axis and line from aperture local origin to
          xyz origin (degrees)"),
     p("     phi: Tilt of ellipse major axis from horizontal plane (degrees)"),
     p("      mu: Amount of leaning over of aperture (degrees)"),
     p("   Omega: Amount of azimuthal rotation of aperture (degrees)"),
     p("   s_min: Angle at which aperture-generating curve begins (degrees)"),
     p("   s_max: Angle at which aperture-generating curve ends (degrees)"),
     p("       A: Distance from main origin to local origin of aperture at
          theta=0"),
     p("       a: Major radius of ellipse at theta=0"),
     p("       b: Minor radius of ellipse at theta=0"),
     p("       P: Position of nodule in terms of the angle, s (degrees)"),
     p("     W_1: Width of nodule in s-direction (degrees)"),
     p("     W_2: Width of nodule in theta-direction (degrees)"),
     p("        N: Number of nodules per whorl"),
     p("        L: Height of nodule at theta=0"),
     p("        D: Sense of coiling; 1=dextral, -1=sinistra"),
     p("theta_start: Required for shells such as Dentalium or Diodora, which
          grow at one end while dissolving at the other. In all other
          cases set to -Inf"),
     p("theta_end: Not important for self-similar shells, except very small or
          large values can cause computational problems")
		)
	)
))
# Define server logic ----
server <- function(input, output) {
	}

# Run the application 
shinyApp(ui = ui, server = server)
# Run in a dialog within R Studio
#runGadget(ui, server, viewer = dialogViewer("Shells", width = 1800, height = 1800))
