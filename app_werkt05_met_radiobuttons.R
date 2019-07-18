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
            radioButtons(inputId = "rechtsgeb",
                               label = "Kies een rechtsgebied",
                               choices = c("Civiel recht" = "Civiel recht",
                                           "Bestuursrecht" = "Bestuursrecht",
                                           "Strafrecht" = "Strafrecht"),
                               selected = "Strafrecht"),
            radioButtons(inputId = "naam_gerecht",
                               label = "Kies een of meerdere gerechten:",
                               choices = c("Rechtbank Amsterdam" = "Rechtbank Amsterdam",
                                           "Rechtbank Limburg" = "Rechtbank Limburg",
                                           "Rechtbank Rotterdam" = "Rechtbank Rotterdam",
                                           "Centrale Raad van Beroep" = "Centrale Raad van Beroep"),
                               selected = "Rechtbank Limburg")

#            sliderInput("bins",
#                        "Number of bins:",
#                        min = 1,
#                        max = 50,
#                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
 
# ===================================        
        # -----------------------
        # Inlezen van de gegevens
        # -----------------------
        b <- read.delim("D:/DummyDir/verz_gegevens/verz_geg_uitspraken_tot2.txt",
                        header = TRUE,
                        sep = "|",
                        #                na.strings = "NA",
                        colClasses = c("character", "character", "Date", "character",
                                       "character", "character", "character", "character",
                                       "integer", "integer", "character", "integer")
        )
        # --------------------------------------
        # Nadere selectie op basis van opgave
        # --------------------------------------
        weekdagen_hist <- b %>%
            dplyr::filter(Jaar == input$jaren) %>% #2018) %>% # 
            dplyr::filter(Rechtsgebied == input$rechtsgeb) %>%
            dplyr::filter(Gerecht == input$naam_gerecht) %>%
            # dplyr::filter(grepl(input$RbROT, Gerecht, ignore.case = TRUE)) %>%
            # dplyr::filter(grepl("rechtbank|gerechtshof|bedrijfsleven|centrale", Gerecht, ignore.case = TRUE)) %>%
            dplyr::select(Weekdag)
        # ---------------------------------------------------------------------
        # Volgende regel is noodzakelijk om van een numerieke vector te creeren
        # die input is van de histogram.
        # ---------------------------------------------------------------------
        wd1 <- weekdagen_hist$Weekdag
        
        #        jaren <- seq(min(wd1), max(wd1), length.out = input$jaren)
        #jaren <- seq(min(input$jaren), max(input$jaren), length.out = input$jaren)        
        eee <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5)
        hist(wd1, 
             breaks = eee, 
             col = 'darkgray',
             main = "",
             xlab = "Dagen van de week: 1 ma, 2 di, 3 wo, 4 do, 5 vr, 6 za, 7 zo",
             ylab = "Aantal gepubliceerde uitspraken")        
# ===================================
# ===================================
# KAN NU WEG
# ===================================
#        x    <- faithful[, 2]
#        bins <- seq(min(x), max(x), length.out = 1) #input$bins + 1)

        # draw the histogram with the specified number of bins
#        hist(x, breaks = 25, col = 'darkgray', border = 'white', xlab = "Dagen van de week: 1 ma, 2 di, 3 wo, 4 do, 5 vr, 6 za, 7 zo",
#             ylab = "Aantal gepubliceerde uitspraken")
# ===================================
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
