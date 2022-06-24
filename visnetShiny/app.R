#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(visNetwork)
library(igraph)
library(DT)
library(shinycssloaders)



load("2021-06-24_visNetwork.RData")
Tumors <- read.csv2("Tumors.csv", stringsAsFactors = F)
Tumors <- Tumors[,c(2,1)]
colnames(Tumors) <- c("Acronym", "Tumor")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Paper: Identifying Lethal Dependencies with HUGE Predictive Power from Large-Scale Functional Genomic Screens"),

        # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 4,
            selectInput("I_tumor", label = "Select Tumor", choices = names(graphs_list), selected = "AML"),
            # actionButton("I_plot", label = "Make graph"),
            br(),
            downloadButton("O_esGenes_download","Download table (DEMETER)"),
            downloadButton("O_esGenes_download_2","Download table (CERES)"),
            br(),
            h3("Legend"),
            shiny::tableOutput(outputId = "O_tb_Tum")
        ),

        # Show a plot of the generated distribution
        mainPanel(width = 8,
           visNetworkOutput("distPlot", width = "100%", height = "600px") %>% withSpinner(color="#0dc5c1")
        )
    ),
    
    source("ui-footer.R",local=TRUE)$value
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # observeEvent("I_plot", {
    #     g <- graphs_list[[input$I_tumor]]
    #     vn <- visNetwork::toVisNetworkData(g)
    # })
    vn <- reactive({

        g <- graphs_list[[input$I_tumor]]
        visNetwork::toVisNetworkData(g)

    })
    
    output$O_tb_Tum <- renderTable(Tumors)
    
    # output$O_esGenes_download <- downloadHandler(filename = function(){"TranscriptAchilles_EssentialGenes.csv"},
    #                                              content = function(fname){
    #                                                write.csv2(esGene(), fname, quote = F)})
    
    output$O_esGenes_download <- downloadHandler(filename = "DEMETER-HUGE.xlsx",
                                              content = function(fname){file.copy(from = "data/2022-06-21_Excell_Shiny_DEMETER-HUGE.xlsx", to = fname, overwrite = T)})
    
    output$O_esGenes_download_2 <- downloadHandler(filename = "CERES-HUGE.xlsx",
                                                 content = function(fname){file.copy(from = "data/2022-06-21_Excell_Shiny_CERES-HUGE.xlsx", to = fname, overwrite = T)})
    
    
    output$distPlot <- renderVisNetwork({
        visNetwork(vn()$nodes,
                   vn()$edges,
                   main = paste0("Mutation vs Essential Gene interactions\nin ", input$I_tumor)) %>%
            
            visIgraphLayout(layout = "layout_with_kk") %>% #layout = "layout_with_kk"
            
            visGroups(
                groupname = "Mutation",
                shape = "square",
                icon = list(color = "red", size = 200)
            ) %>%
            visGroups(
                groupname = "Essential\nGene",
                shape = "triangle",
                icon = list(color = "darkblue", size = 55)
            ) %>%
            
            visLegend(
                width = 0.1,
                position = "right",
                main = "Group",
                addEdges = data.frame(color = c("blue"), label = "Biomarker\n\nof Essentiality")
            ) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
