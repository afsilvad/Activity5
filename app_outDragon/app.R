library(shiny)
library(ade4)
library(tidyverse)
library(mice)
library(mvoutlier)

set.seed(197273)
data_dragon <- read_csv("dragons.csv")


dragons_imp_final <- data.frame()

dragons_mice <- mice(data_dragon %>%
                         select(-ID), method = "rf")
dragons_imp <- complete(dragons_mice)
dragons_imp <- cbind("ID" = data_dragon$ID, dragons_imp)

niveau <- levels(factor(data_dragon$Species))
row_out <- c()

for (k in niveau) {
    out_temp <- sign2(dragons_imp %>%
                          filter(Species == k) %>%
                          select(-Species), qcrit = 0.975)$wfinal01
    row_out <- append(row_out, out_temp)
}


# Define UI for application that draws a histogram
ui <- fillPage(
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
    titlePanel("Outliers des variables du tableau Dragons"),
    
    fluidRow(
        column(6,
               sidebarPanel(
                   sliderInput("var_adjust",
                               label = "Variables à montrer:",
                               min = 3,
                               max = 11,
                               value = 5,
                               step = 1),
                   width = 12
               )
        ),
        column(6,
               checkboxGroupInput(inputId = "select_species",
                                  label = "Sélectionner les spèces à montrer",
                                  choices = levels(factor(data_dragon$Species)),
                                  selected = levels(factor(data_dragon$Species)))
        )
    ),
    plotOutput("outPlot")
)

server <- function(input, output) {
    
    output$outPlot <- renderPlot({
        validate(
            need(input$select_species != "", "Choisissez au moins une espèce")
        )
        d <- input$var_adjust + 2
        plot(dragons_imp %>%
                 filter(Species %in% input$select_species) %>%
                 select(3:d), col = row_out + 2)
    }, height = 600)
}
# Run the application 
shinyApp(ui = ui, server = server)