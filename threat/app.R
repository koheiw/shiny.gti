#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(shinyBS)
require(LSX)
require(wireframe.lss)
require(ggplot2)

lss <- readRDS("/data/edgar1/kohei/demo/data/threat/lss.RDS")
dict_seed <- dict <- quanteda::dictionary(file = "seedwords.yml")
default <- TRUE

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Geopolitical Analysis through the New York Times"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h4("Seed words"),
            selectInput("seedword_type", "Polarity", names(dict_seed)),
            textAreaInput("seedwords_pos", "Positive", paste(dict_seed[[1]][[1]], collapse = ", ")),
            textAreaInput("seedwords_neg", "Negative", paste(dict_seed[[1]][[2]], collapse = ", ")),
            actionButton("update", "Update", icon = icon("rotate")),
            width = 3
        ),

        # Show a plot of the generated distribution
        mainPanel(
           bsAlert("alert"),
           plotOutput("plot_terms")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(input$seedword_type, {
        type <- input$seedword_type
        updateTextAreaInput(inputId = "seedwords_pos", value = paste(dict_seed[[type]][[1]], collapse = ", "))
        updateTextAreaInput(inputId = "seedwords_neg", value = paste(dict_seed[[type]][[2]], collapse = ", "))
    })
    
    observeEvent(input$update, {
        
        
        closeAlert(getDefaultReactiveDomain(), alertId = "ignore_seedwords")
        #type <- input$seedword_type
        #seed <- as.seedwords(dict_seed[[type]])
        lis <- stringi::stri_split_fixed(c(input$seedwords_pos, input$seedwords_neg), ",")
        seed <- as.seedwords(lapply(lis, stringi::stri_trim))
        lss <- as.textmodel_lss(lss, seed)
        #output$warning_pos <- renderText()
        output$plot_terms <- renderPlot({
            
            ignore <- setdiff(names(seed), colnames(lss$embedding))
            if (length(ignore)) {
                msg <- paste("Following seed words are ignored:", paste(ignore, collapse = ", "))
                createAlert(getDefaultReactiveDomain(), anchorId = "alert", alertId = "ignore_seedwords",
                            content = msg, dismiss = FALSE)
            }
            
            set.seed(1234)
            smp <- sample(names(lss$beta), 50, prob = abs(lss$beta) ^ 2) # random sample from extremes
            textplot_terms(lss, c(names(lss$seeds), smp)) 
        })
        
    })
    output$plot_terms <- renderPlot({

        set.seed(1234)
        smp <- sample(names(lss$beta), 50, prob = abs(lss$beta) ^ 2) # random sample from extremes
        textplot_terms(lss, c(names(lss$seeds), smp)) 
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
