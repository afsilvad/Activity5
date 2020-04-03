library(shiny)
library(ade4)
library(tidyverse)
library(mice)
library(mvoutlier)

set.seed(197273)
dragons_imp <- read_csv("dragons_complete.csv")

niveau <- levels(factor(dragons_imp$Species))
row_out <- c()

for (k in niveau) {
    out_temp <- sign2(dragons_imp %>%
                          filter(Species == k) %>%
                          select(-Species), qcrit = 0.975)$wfinal01
    row_out <- append(row_out, out_temp)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(              
        tags$style(HTML("
.shiny-output-error-validation {
color: orange;
font-size: 300%;
text-align: center;
}
"))
    ),
    # Application title
    titlePanel("Valeurs aberrantes des variables du tableau Dragons"),
    
    fluidRow(
        column(6,
               sidebarPanel(
                   sliderInput("var_adjust",
                               label = "Variables à montrer:",
                               min = 1,
                               max = 11,
                               value = c(1, 5),
                               step = 1),
                   width = 12
               )
        ),
        column(6,
               checkboxGroupInput(inputId = "select_species",
                                  label = "Sélectionner les spèces à montrer",
                                  choices = levels(factor(dragons_imp$Species)),
                                  selected = levels(factor(dragons_imp$Species)))
        )
    ),
    strong("Les points rouges représentent les données aberrantes"),
    plotOutput("outPlot")
)

server <- function(input, output) {
    
    output$outPlot <- renderPlot({
        validate(
            need(input$select_species != "", "Choisissez au moins une espèce")
        )
        r1 <- input$var_adjust[1] + 2
        r2 <- input$var_adjust[2] + 2
        plot(dragons_imp %>%
                 filter(Species %in% input$select_species) %>%
                 select(all_of(r1):all_of(r2)), col = row_out + 2)
    }, height = 600)
}
# Run the application 
shinyApp(ui = ui, server = server)