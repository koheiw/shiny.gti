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
country_def <<- c("iq", "vn", "de")
result <<- data.frame() 

ui <- fluidPage(

    # Application title
    titlePanel("Geopolitical Threat Index"),
    p("Identifying foreign threats through an anaysis of New York Times articles on military over 160 years"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("seedword_type", "Seed words", names(dict_seed)),
            textAreaInput("seedwords_pos", "High", paste(dict_seed[[1]][[1]], collapse = ", ")),
            textAreaInput("seedwords_neg", "Low", paste(dict_seed[[1]][[2]], collapse = ", ")),
            actionButton("update", "Update", icon = icon("rotate")),
            hr(),
            selectInput("countries", "Countries", country_all, selected = country_def, multiple = TRUE),
            checkboxInput("labels", "Show event labels", value = TRUE),
            actionButton("select", "Select", icon = icon("check")),
            width = 3
        ),

        # Show a plot of the generated distribution
        mainPanel(
            bsAlert("alert"),
            tabsetPanel(
                id = "tabs",
                tabPanel(
                    title = "Introduction",
                    value = "tab_intro",
                    fluidRow(
                        h3("About the app"),
                        markdown("
                        This is an online app that allow you to compute, visualize and download the Geopolitical Threat Index (GTI) between 1981 and 2022.
                        The GTI is produce by applying Latent Semantic Scaling to a corpus of lead sentences from the of New York Times articles about militaries.
                        The original study (Trubowitz & Watanabe, 2021) covered only from 1981 to 2017 but this app extends to 2022.
                        It also allows users to enter custom seed words to experience the efficiency and the flexibility of the LSS algorithm (Watanabe, 2021).
                        "),
                        h3("How to use"),
                        markdown("
                        1. Select **Seed words**. Seed words in **High** and **Low** should be keywords related to the higher and lower GTI scores, respectively. 
                           We only used 'Hostility' in the original study but added 'Terrorism', 'Nuclear' and 'Regime' as examples.
                        1. Click **Update** to reflect changes in seed words.
                        1. Open the *Polarity words* tab and inspect the polarity of words.
                        1. Type the names of countries of your interest in **Countries**. If the box is blank, it shows the global index. 
                        1. Click **Select**.
                        1. Open the *Historical trends* tab and study the changes in the index.
                        1. Tick **Show event labels** to highlight key evens in the trend plot. 
                           We have event labels only for 14 countries discussed in the original paper 
                           (Russia, China, Germany, Japan, France, United Kingdom, Cuba, Spain, Mexico, Vietnam, Iran, Iraq, Afghanistan, Syria).
                        "),
                        h3("Feedback"),
                        markdown("
                        If you have questions or suggests, please write to [Kohei Watanabe](watanabe.kohei@gmail.com).
                        "),
                        h3("References"),
                        markdown("
                        - Trubowitz, P. & Watanabe, K. (2021). The Geopolitical Threat Index: A Text-Based Computational Approach to Identifying Foreign Threats.
                        *International Studies Quarterly*, https://doi.org/10.1093/isq/sqab029.
                        - Watanabe, K. (2021). Latent Semantic Scaling: A Semisupervised Text Analysis Technique for New Domains and Languages, 
                        *Communication Methods and Measures*, https://doi.org/10.1080/19312458.2020.1832976.
                        "),
                    style = "padding:20px")
                ),
                tabPanel(
                    title = "Polarity words",
                    value = "tab_terms",
                    plotOutput("plot_terms", height = 500)
                ),
                tabPanel(
                    title = "Historical trends",
                    value = "tab_trends",
                    plotOutput("plot_trends", height = 500)
                ),
                tabPanel(
                    title = "Download",
                    value = "tab_download",
                    fluidRow(
                        markdown("
                        You can download the raw data in the CSV format for your research. 
                        Please inlcude all the seed words your used to create the index when you publish your research.
                        "),
                        downloadButton("download", "Download GTI"),
                    style = "padding:20px")
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
    
    output$plot_trends <- renderPlot({
        
        if (length(input$countries)) {
            country <- input$countries
        } else {
            country <- NULL
        }
        plot_gti(result, event, country, show_label = input$labels)
    })
    
    observeEvent(input$seedword_type, {
        type <- input$seedword_type
        updateTextAreaInput(session, "seedwords_pos", value = paste(dict_seed[[type]][[1]], collapse = ", "))
        updateTextAreaInput(session, "seedwords_neg", value = paste(dict_seed[[type]][[2]], collapse = ", "))
    })
    
    observeEvent(input$update, {
        
        closeAlert(session, alertId = "seedwords")
        #if (input$tabs == "tab_intro")
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
        
        output$plot_trends <- renderPlot({
            
            dat <- quanteda::docvars(lss$data)
            dat$lss <- predict(lss)
            result <<- get_gti(dat) # update the global variable
            if (length(input$countries)) {
                country <- input$countries
            } else {
                country <- NULL
            }
            plot_gti(result, event, country, show_label = input$labels)
        })
        
    })
    
    observeEvent(input$select, {
        
        updateTabsetPanel(session, 'tabs', selected = "tab_trends")
        
        if (length(input$countries)) {
            country <- input$countries
        } else {
            country <- NULL
        }
        output$plot_trends <- renderPlot({
            plot_gti(result, event, country, show_label = input$labels) # use the global variable
        })
    })
    
    output$download <- downloadHandler(
        filename = function() {
            paste("gti.csv", sep = "")
        },
        content = function(file) {
            write.csv(result, file, row.names = FALSE)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
