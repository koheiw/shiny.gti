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
require(ggrepel)
source("functions.R")

lss <- readRDS("lss.RDS") # from /data/edgar1/kohei/demo/data/threat/lss.RDS
dict_seed <- quanteda::dictionary(file = "seedwords.yml")
event <- yaml::read_yaml("events.yml")

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
            tabsetPanel(
                id = "tabs",
                tabPanel(
                    title = "Polaity words",
                    value = "tab_terms",
                    plotOutput("plot_terms", height = 500)
                ),
                tabPanel(
                    title = "Historical trends",
                    value = "tab_documents",
                    plotOutput("plot_documents", height = 500)
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observeEvent(input$seedword_type, {
        type <- input$seedword_type
        updateTextAreaInput(session, "seedwords_pos", value = paste(dict_seed[[type]][[1]], collapse = ", "))
        updateTextAreaInput(session, "seedwords_neg", value = paste(dict_seed[[type]][[2]], collapse = ", "))
    })
    
    observeEvent(input$update, {
        
        closeAlert(session, alertId = "seedwords")
        updateTabsetPanel(session, 'tabs', selected = "tab_terms")
        
        lis <- stri_split_fixed(c(input$seedwords_pos, input$seedwords_neg), ",")
        lis <- lapply(lis, function(x) {
            x <- stri_trim(x)
            x[nzchar(x)]
        })
        seed <- as.seedwords(lis)
        
        ignore <- setdiff(names(seed), colnames(lss$embedding))
        if (length(ignore)) {
            if (length(ignore) == 1) {
                msg <- paste(paste0('"', ignore, '"', collapse = ", "), "is not found.")
            } else {
                msg <- paste(paste0('"', ignore, '"', collapse = ", "), "are not found.")
            }
            createAlert(session, anchorId = "alert", alertId = "seedwords",
                        content = msg, dismiss = FALSE)
        }
        
        lss <- tryCatch({
            as.textmodel_lss(lss, seed)
        }, error = function(e) {
            createAlert(session, anchorId = "alert", alertId = "seedwords",
                        content = "No seed words are provided.", style = "danger", dismiss = FALSE)
            return(NULL)
        })
        if (is.null(lss))
            return()

        #output$warning_pos <- renderText()
        output$plot_terms <- renderPlot({
            
            set.seed(1234)
            smp <- sample(names(lss$beta), 50, prob = abs(lss$beta) ^ 2) # random sample from extremes
            textplot_terms(lss, c(names(lss$seeds), smp)) +
                theme(axis.text = element_text(size = 13, colour = "black"),
                      text = element_text(size = 13, colour = "black"))
        })
        
        output$plot_documents <- renderPlot({
            
            dat <- quanteda::docvars(lss$data)
            dat$lss <- predict(lss)
            dat_gti <- get_gti(dat)
            plot_gti(dat_gti, event, NULL)
        })
        
    })
    
    # default plots
    output$plot_terms <- renderPlot({

        set.seed(1234)
        smp <- sample(names(lss$beta), 50, prob = abs(lss$beta) ^ 2) # random sample from extremes
        textplot_terms(lss, c(names(lss$seeds), smp)) +
            theme(axis.text = element_text(size = 13, colour = "black"),
                  text = element_text(size = 13, colour = "black"))
        
    })
    
    output$plot_documents <- renderPlot({
        
        dat <- quanteda::docvars(lss$data)
        dat$lss <- predict(lss)
        dat_gti <- get_gti(dat)
        plot_gti(dat_gti, event)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
