# Geoplitical Threat Index web version

# The Geopolitical Threat Index: A Text-Based Computational Approach to Identifying Foreign Threats
# Peter Trubowitz, Kohei Watanabe
# International Studies Quarterly, Volume 65, Issue 3, September 2021, Pages 852–865, https://doi.org/10.1093/isq/sqab029


require(shiny)
require(shinyBS)
require(LSX)
require(ggplot2)
require(ggrepel)
source("functions.R")

lss <- readRDS("lss.RDS") # from /data/edgar1/kohei/demo/data/threat/lss.RDS
dict_seed <- quanteda::dictionary(file = "seedwords.yml")
event <- yaml::read_yaml("events.yml")

# global variables
country_all <<- get_country(lss)
country_def <<- c("cn", "ru", "de")
result <<- data.frame() 

ui <- fluidPage(

    # Application title
    titlePanel("Geopolitical Analysis through the New York Times"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("seedword_type", "Seed words", names(dict_seed)),
            textAreaInput("seedwords_pos", "High", paste(dict_seed[[1]][[1]], collapse = ", ")),
            textAreaInput("seedwords_neg", "Low", paste(dict_seed[[1]][[2]], collapse = ", ")),
            actionButton("update", "Update", icon = icon("rotate")),
            hr(),
            selectInput("countries", "Countries", country_all, selected = country_def, multiple = TRUE),
            actionButton("select", "Select", icon = icon("check")),
            width = 3
        ),

        # Show a plot of the generated distribution
        mainPanel(
            bsAlert("alert"),
            tabsetPanel(
                id = "tabs",
                tabPanel(
                    title = "Polarity words",
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
    
    # initialize
    dat <- quanteda::docvars(lss$data)
    dat$lss <- predict(lss)
    result <<- get_gti(dat) # update the global variable
    
    # save for wireframe
    #saveRDS(result, "/data/lqe/kohei/demo/result/threat/data_index.RDS")
    
    output$plot_terms <- renderPlot({
        
        set.seed(1234)
        smp <- sample(names(lss$beta), 50, prob = abs(lss$beta) ^ 2) # random sample from extremes
        plot_terms(lss, c(names(lss$seeds), smp))
        
    })
    
    output$plot_documents <- renderPlot({
        
        if (length(input$countries)) {
            country <- input$countries
        } else {
            country <- NULL
        }
        plot_gti(result, event, country)
    })
    
    observeEvent(input$seedword_type, {
        type <- input$seedword_type
        updateTextAreaInput(session, "seedwords_pos", value = paste(dict_seed[[type]][[1]], collapse = ", "))
        updateTextAreaInput(session, "seedwords_neg", value = paste(dict_seed[[type]][[2]], collapse = ", "))
    })
    
    observeEvent(input$update, {
        
        closeAlert(session, alertId = "seedwords")
        #updateTabsetPanel(session, 'tabs', selected = "tab_terms")
        
        lis <- stri_split_fixed(c(input$seedwords_pos, input$seedwords_neg), ",")
        lis <- lapply(lis, function(x) {
            x <- stri_trim(x)
            x[nzchar(x)]
        })
        seed <- as.seedwords(lis, concatenator = " ")
        seeds_fixed <- object2fixed(names(seed), colnames(lss$embedding), valuetype = "glob")
        if (length(seeds_fixed) > 1000) {
            createAlert(session, anchorId = "alert", alertId = "seedwords",
                        content = "Too many seed words are provided.", style = "danger", dismiss = FALSE)
            return()
        }
        
        ignore <- setdiff(names(seed), names(seeds_fixed))
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
        
        dat <- quanteda::docvars(lss$data)
        dat$lss <- predict(lss)
        result <<- get_gti(dat) # update the global variable
        
        #output$warning_pos <- renderText()
        output$plot_terms <- renderPlot({
            
            set.seed(1234)
            smp <- sample(names(lss$beta), 50, prob = abs(lss$beta) ^ 2) # random sample from extremes
            plot_terms(lss, c(names(lss$seeds), smp))
        })
        
        output$plot_documents <- renderPlot({
            
            dat <- quanteda::docvars(lss$data)
            dat$lss <- predict(lss)
            result <<- get_gti(dat) # update the global variable
            if (length(input$countries)) {
                country <- input$countries
            } else {
                country <- NULL
            }
            plot_gti(result, event, country)
        })
        
    })
    
    observeEvent(input$select, {
        
        updateTabsetPanel(session, 'tabs', selected = "tab_documents")
        
        if (length(input$countries)) {
            country <- input$countries
        } else {
            country <- NULL
        }
        output$plot_documents <- renderPlot({
            plot_gti(result, event, country) # use the global variable
        })
    })    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
