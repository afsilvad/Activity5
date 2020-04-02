library(shiny)
library(ade4)
library(tidyverse)
library(mice)
library(mvoutlier)


set.seed(197273)
data_dragon <- read_csv("dragons.csv")

niveau <- levels(factor(data_dragon$Species))
dragons_imp_final <- data.frame()

for (i in niveau) {
    dragons_mice1 <- mice(data_dragon %>%
                              filter(Species == i), method = "rf")
    dragon_imp_temp <- complete(dragons_mice1)
    dragons_imp_final <- rbind(dragons_imp_final, dragon_imp_temp)
}

row_out <- c()

for (k in niveau) {
    out_temp <- sign2(dragons_imp_final %>%
                          filter(Species == k) %>%
                          select(-Species), qcrit = 0.975)$wfinal01
    row_out <- append(row_out, out_temp)
}


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
           plotOutput("outPlot")
           )
        )
    )

#renderPlot({
#    d <- input$var_adjust + 2
#    plot(dragons_imp[,3:d], col = is_out + 2)
#})
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$outPlot <- renderPlot({
        d <- input$var_adjust + 2
        plot(dragons_imp_final[,3:d], col = row_out + 2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
