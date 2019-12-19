library(shiny)
source('load_libraries.R')
source('funktionen_helper_winkel.R')
source('berechne_sonnenposition.R')
source('berechne_sonneneinstrahlung.R')
source('funktionen_einstrahlung_panel.R')
source('funktionen_winkel_optimierung.R')
source('funktionen_winkel_solarpanel.R')
source('generiere_zeitreihe.R')
source('ergebnis_visualisierung.R')


ui <- fluidPage( # Define UI
    
    titlePanel("Optimierung des Panelwinkels bei Solaranlagen"),
    
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("daterange", "Datumsauswahl:",
                           language = "de",
                           start  = Sys.Date(),
                           end    = Sys.Date() + 1,
                           min    = "2001-01-01",
                           weekstart = 1,
                           format = "dd-mm-yyyy",
                           sep = "bis"),
            numericInput("lat", "Breitengrad: (-90° bis 90°)",
                         53.6332,
                         min = -90,
                         max = 90),
            numericInput("lon", "Längengrad: (-180° bis 180°)",
                         9.9881,
                         min = -180,
                         max = 180),
            
            h3("Eigener Winkel"),
            numericInput("hardangle", "Winkel des Panels: (0° bis 90°)",
                         0,
                         min = 0,
                         max = 90),
            
            h3("Info:"),
            p("Welches ist der optimale Kippwinkel eines Solarpanels? Diese Webseite stellt ein Tool zur Verfügung, um, 
               abhängig von Zeitraum und Position, diese Frage zu beantworten.
               Die Berechnung vergleicht die Strahlung auf eine fest gewinkelte Fläche mit einer flach am Boden liegenden. 
               Die Simulation geht ausschließlich von optimalen Bedingungen aus (keine Wolken oder Schatten).
               Die prozentuale Verbesserung ist dementsprechend eine Obergrenze."),
               strong("Die Berechnung ist nur eine Annäherung und basiert auf einer Simulation.
                    Es können außerdem Abweichungen von bis zu 0.5° im optimalen Kippwinkel durch Rundungen auftreten.
                    Je kleiner der ausgewählte Zeitraum ist, desto genauer ist die Simulation."),
            br(),
            p(),
            p("Die Einstellung des eigenen Winkels kann genutzt werden,
               um den Ertrag des optimierten Panels mit anderen Konfigurationen
               zu vergleichen (bspw. der Winkel eines Hausdaches)."),
            p("Bei der Positionseingabe wird die Zeitzone automatisch erkannt, 
               so dass die ausgewählten Tage stets von 0 Uhr bis 0 Uhr simuliert werden."),
            p("Der genutzte Berechnungsalgorithmus ist für die nördliche Halbkugel ausgelegt und kann
               unzuverlässige Daten für sehr südliche Regionen liefern.")
        ),
        
        mainPanel(
            h2("Kennzahlen"),
            plotOutput("angles", height = "320px"),
            h2("Visualisierung"),
            plotOutput("distPlot"),
            h2("Kartenansicht"),
            plotOutput("map")
        )
    )
)


server <- function(input, output) { # Define server logic
    
    intervall_length <- reactive(
        get_optimised_intervall_length(input$daterange[1],
                                       input$daterange[2]))
    
    optimisation_result <- reactive({
        berechne_optimale_panelwinkel_gesamt(input$daterange[1],
                                             input$daterange[2],
                                             input$lat,
                                             input$lon,
                                             intervall_length())
    })
    
    optim_angles <- reactive({optimisation_result()$winkel})
    sim_data <- reactive({
        mutate(optimisation_result()$data,
               eingefangene_strahlung_hardangle = map2_dbl(
                   winkel_kartesisch,
                   sonnen_strahlung,
                   berechne_strahlungsenergie_bei_panelwinkel,
                   elevation = if_else(input$lat < 0,
                                       -input$hardangle,
                                       input$hardangle),
                   azimuth = 0))})

    comp_data <- reactive({pivot_irridation_data(sim_data())})
    
    output$distPlot <- renderPlot({
        print(visualisiere_ertrag(comp_data()))
    })
    
    output$angles <- renderPlot({
        print(visualisiere_kippung_steigerung(optim_angles()["elevation"],
                                              comp_data()))
    })

    output$map <- renderPlot({
        print({
            visualisiere_koordinaten(latitude = input$lat,
                                     longitude = input$lon) +
                theme(text = element_text(size=20))
        })
    })
}

shinyApp(ui = ui, server = server) # Run the application 
