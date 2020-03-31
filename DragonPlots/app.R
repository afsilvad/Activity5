library(shiny)
library(ade4)
library(tidyverse)
library(mice)
library(mvoutlier)


set.seed(197273)
data_dragon <- read_csv("dragons.csv")
dragons_mice <- mice(data_dragon, method = "rf")
dragons_imp <- mvoutlier::complete(dragons_mice)

shiny::runGitHub( "Activity5", "afsilvad", subdir = "/DragonPlots")

is_out <- sign2(dragons_imp %>%
                    select(-Species), qcrit = 0.975)$wfinal01

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Outliers des variables du tableau Dragons"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("var_adjust",
                        label = "Variables à montrer:",
                        min = 3,
                        max = 11,
                        value = 5,
                        step = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

#renderPlot({
#    d <- input$var_adjust + 2
#    plot(dragons_imp[,3:d], col = is_out + 2)
#})
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        d <- input$var_adjust + 2
        plot(dragons_imp[,3:d], col = is_out + 2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
