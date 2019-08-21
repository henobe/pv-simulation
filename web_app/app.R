library(shiny)
source('load_libraries.R')
source('funktionen_helper_winkel.R')
source('berechne_sonnenposition.R')
source('berechne_sonneneinstrahlung.R')
source('berechne_gesamte_strahlungsenergie.R')
source('funktionen_winkel_optimierung.R')
source('funktionen_winkel_solarpanel.R')
source('generiere_zeitreihe.R')
source('visualisiere_koordinaten.R')


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
            numericInput("long", "Längengrad: (Dezimalgrad)", 9.9881, min = -180, max = 180),
            
            h3("Info:"),
            p("Die Berechnung vergleicht die Strahlung auf eine fest gewinkelte Fläche mit einer flach am Boden liegende. 
              Die Simulation geht ausschließlich von optimalen Bedingungen aus (keine Wolken oder Schatten).
              Die prozentuale Verbesserung ist dementsprechend eine Obergrenze."),
            p("Bei der Positionseingabe wird die Zeitzone automatisch erkannt, 
              so dass die ausgewählten Tage stets von 0 Uhr bis 0 Uhr simuliert werden."),
            strong("Die Berechnung ist nur eine Annäherung und basiert auf der Simulation von 10-minütigen Werten. 
              Diese Auflösung reicht um die optimalen Winkel im hunderstel Bereich zu errechnen.
              Die Berechnung langer Zeiträume kann allerdings mehrere Minuten dauern!"),
            br(),
            p("Die Sprünge der Sonneneinstrahlung im Fühling und Herbst ergeben 
              sich durch eine Anpassung der Parameter für Sommer und Winter. 
              Der genutzte Berechnungsalgorithmus hat eine höhere Genauigkeit für die nördliche Halbkugel.")
        ),
        
        mainPanel(
            # Output: Verbatim text for data summary ----
            h2("Kennzahlen"),
            textOutput("angles"),
            textOutput("relative_gain"),
            h2("Visualisierung"),
            plotOutput("distPlot"),
            h2("Kartenansicht"),
            plotOutput("map")
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
                x = "Zeitpunkt UTC",
                y = "Strahlungsstärke [W/m^2]",
                colour = "Ausrichtung des Panels"
            ) +
            theme(text = element_text(size=20),
                  legend.position = "bottom")
        print(plot)
    })
    
    output$angles <- renderText({
        paste(c("Optimale Azimuth- und Höhenwinkel:", round(optim_angles(), digits = 1)))
    })
    
    output$relative_gain <- renderText({
        paste(round(gain(), digits = 2), "% Verbesserung gegenüber flach liegend", sep = "")
    })
    
    output$map <- renderPlot({
        # generate bins based on input$bins from ui.R
        plot <- visualisere_koordinaten(latitude = input$lat,
                                        longitude = input$long)
        plot <- plot + theme(text = element_text(size=20))
        print(plot)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
