library(shiny)
source('../load_libraries.R')
#library(scales)

source('../funktionen_helper_winkel.R')
source('../berechne_sonnenposition.R')
source('../berechne_sonneneinstrahlung.R')
source('../berechne_gesamte_strahlungsenergie.R')
source('../funktionen_winkel_optimierung.R')
source('../funktionen_winkel_solarpanel.R')
source('../generiere_zeitreihe.R')


ui <- fluidPage( # Define UI
    
    titlePanel("Optimierung des Panelwinkels bei Solaranlagen"),
    
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("daterange", "Datumsauswahl:",
                           language = "de",
                           start  = Sys.Date(),
                           end    = Sys.Date()+1,
                           min    = "2001-01-01",
                           weekstart = 1,
                           format = "dd-mm-yyyy",
                           
                           sep = "bis"),
            
            numericInput("lat", "Breitengrad: (Dezimalgrad)", 53.6332, min = -90, max = 90),
            numericInput("long", "Längengrad: (Dezimalgrad)", 9.9881, min = -180, max = 180)
        ),
        
        mainPanel(
            # Output: Verbatim text for data summary ----
            verbatimTextOutput("summary"),
            plotOutput("distPlot"),
            verbatimTextOutput("angles"),
            verbatimTextOutput("relative_gain")
        )
    )
)


server <- function(input, output) { # Define server logic
    
    optimisation_result <- reactive({berechne_optimale_panelwinkel_gesamt(input$daterange[1],
                                                                          input$daterange[2],
                                                                          c(input$lat, input$long))
    })
    
    optim_angles <- reactive({optimisation_result()$winkel})
    sim_data <- reactive({optimisation_result()$data})
    gain <- reactive({optimisation_result()$relative_gain})
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        plot <- ggplot(sim_data(), aes(x = datetime)) +
            geom_line(aes(y = sonnen_strahlung, colour = "Flach")) +
            geom_line(aes(y = eingefangene_strahlung, colour = "Optimal Ausgerichtet")) +
            labs(
                x = "Zeitpunkt (UTC)",
                y = "Strahlungsstärke [W/m^2]",
                colour = "Ausrichtung des Panels"
            ) +
            theme(legend.position = "bottom")
        
        print(plot)
    })
    
    output$summary <- renderPrint({
        start <- input$daterange[1]
        end <- input$daterange[2]
        
        print(c(start, end))
    })
    
    output$angles <- renderText({
        paste(c("Optimale Azimuth- und Höhenwinkel:", round(optim_angles(), digits = 1)))
    })
    
    output$relative_gain <- renderText({
        paste(round(gain(), digits = 2), "% Verbesserung gegenüber flach liegend", sep = "")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
