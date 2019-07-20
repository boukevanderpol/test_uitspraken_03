#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Test - Op welke dag worden (gepubliceerde) uitspraken uitgesproken?"),
    textInput("text01", 
              label = "", 
              value = "Een test om eea zichtbaar te maken. NB: Gegevens moeten nog worden gevalideerd.",
              width = "800px"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("jaren",
                        "Jaar van de uitspraken:",
                        min = 2015,
                        max = 2019,
                        value = 2018,
                        step = 1,
                        ticks = FALSE),
            checkboxGroupInput(inputId = "rechtsgeb",
                               label = "Kies een of meerdere rechtsgebieden:",
                               choices = c("Civiel recht" = "Civiel recht",
                                           "Bestuursrecht" = "Bestuursrecht",
                                           "Strafrecht" = "Strafrecht"),
                               selected = c("Civiel recht",
                                            "Bestuursrecht",
                                            "Strafrecht")),
            checkboxGroupInput(inputId = "naam_gerecht",
                               label = "Kies een of meerdere gerechten:",
                               choices = c("Rechtbank Amsterdam" = "Rechtbank Amsterdam",
                                           "Rechtbank Den Haag" = "Rechtbank Den Haag",
                                           "Rechtbank Gelderland" = "Rechtbank Gelderland",
                                           "Rechtbank Limburg" = "Rechtbank Limburg",
                                           "Rechtbank Midden-Nederland" = "Rechtbank Midden-Nederland",
                                           "Rechtbank Noord-Holland" = "Rechtbank Noord-Holland",
                                           "Rechtbank Noord-Nederland" = "Rechtbank Noord-Nederland",
                                           "Rechtbank Oost-Brabant" = "Rechtbank Oost-Brabant",
                                           "Rechtbank Overijssel" = "Rechtbank Overijssel",
                                           "Rechtbank Rotterdam" = "Rechtbank Rotterdam",
                                           "Rechtbank Zeeland-West-Brabant" = "Rechtbank Zeeland-West-Brabant",
                                           "Gerechtshof Amsterdam" = "Gerechtshof Amsterdam",
                                           "Gerechtshof Arnhem-Leeuwarden" = "Gerechtshof Arnhem-Leeuwarden",
                                           "Gerechtshof Den Haag" = "Gerechtshof Den Haag",
                                           "Gerechtshof 's-Hertogenbosch" = "Gerechtshof 's-Hertogenbosch",
                                           "Centrale Raad van Beroep" = "Centrale Raad van Beroep",
                                           "College van Beroep voor het bedrijfsleven" = "College van Beroep voor het bedrijfsleven"),
                               selected = c("Rechtbank Amsterdam",
                                            "Rechtbank Den Haag",
                                            "Rechtbank Gelderland",
                                            "Rechtbank Limburg", 
                                            "Rechtbank Midden-Nederland",
                                            "Rechtbank Noord-Holland",
                                            "Rechtbank Noord-Nederland",
                                            "Rechtbank Oost-Brabant",
                                            "Rechtbank Overijssel",
                                            "Rechtbank Rotterdam",
                                            "Rechtbank Zeeland-West-Brabant",
                                            "Gerechtshof Amsterdam",
                                            "Gerechtshof Arnhem-Leeuwarden",
                                            "Gerechtshof Den Haag",
                                            "Gerechtshof 's-Hertogenbosch",
                                            "Centrale Raad van Beroep",
                                            "College van Beroep voor het bedrijfsleven"
                                            )
                               )

#            sliderInput("bins",
#                        "Number of bins:",
#                        min = 1,
#                        max = 50,
#                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("heatmap"),
           dataTableOutput("tabel")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # -----------------------
    # Inlezen van de gegevens
    # -----------------------
    b <- read.delim("D:/DummyDir/verz_gegevens/verz_geg_uitspraken_totaal2.txt",
                    header = TRUE,
                    sep = "|",
                    #                na.strings = "NA",
                    colClasses = c("character", "character", "Date", "character",
                                   "character", "character", "character", "character",
                                   "character", "integer", "character", "integer")
    )
    dag_levels <- c("ma", "di", "wo", "do", "vr", "za", "zo")
    b$Weekdag <- factor(b$Weekdag, levels = dag_levels)
    
    
    output$heatmap <- renderPlot({
        # --------------------------------------
        # Nadere selectie op basis van opgave UI
        # --------------------------------------
        weekdagen <- b %>%
            dplyr::filter(Jaar == input$jaren) %>% #2018) %>% # 
            dplyr::filter(Rechtsgebied %in% input$rechtsgeb) %>%
            dplyr::filter(Gerecht %in% input$naam_gerecht) %>%
            dplyr::select(Gerecht, Weekdag) %>%
            dplyr::group_by(Gerecht, Weekdag) %>%  
            dplyr::summarise(count = n()) %>%
            dplyr::mutate(percentage = ((count / sum(count)) * 100))
        

        # ---------------------------------------------------------------------
        # Heatmap 
        # ---------------------------------------------------------------------
        ggplot2::ggplot(data = weekdagen, 
                        aes(x = Weekdag, 
                            y = Gerecht, 
                            fill = percentage)) + 
            geom_tile() + 
            labs(x = "Dagen van de week", 
                 y = "Gerechten", 
                 title = "Relatieve verdeling van uitspraken per gerecht per dag van de week") +
            scale_fill_gradient(low = "green",
                                high = "red") +
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.background = element_blank() #, 
#                  axis.line = element_blank(),
#                  axis.ticks = element_blank(),
#                  axis.text.y = element_blank()
                  )
    })
    output$tabel <- renderDataTable({
        # --------------------------------------
        # Nadere selectie op basis van opgave UI
        # --------------------------------------
        b %>%
            dplyr::filter(Jaar == input$jaren) %>% #2018) %>% # 
            dplyr::filter(Rechtsgebied %in% input$rechtsgeb) %>%
            dplyr::filter(Gerecht %in% input$naam_gerecht) %>%
            dplyr::select(Gerecht, Weekdag) %>%
            dplyr::group_by(Gerecht, Weekdag) %>%  
            dplyr::summarise(count = n()) %>%
            dplyr::mutate(percentage = ((count / sum(count)) * 100))
        # ---------------------------------------------------------------------
        # Tabel 
        # ---------------------------------------------------------------------
    # weekdagen
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
