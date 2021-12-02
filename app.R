#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(DT)


Stationary = read.csv(paste0("WGFP_Raw_20211130.csv"))
Mobile = read.csv("WGFP_MobileDetections.csv", colClasses=c(rep("character",10)))
Biomark <- read.csv("Biomark_Raw_20211109_1.csv", dec = ",")
Release = read.csv("WGFP_ReleaseData_Master.csv",colClasses=c(rep("character",18)))


source("WGFP_EncounterHistoriesFunction.R")

df_list <- WGFP_Encounter_FUN(Stationary = Stationary, Mobile = Mobile, Release= Release, Biomark = Biomark)

ENC_Release2_1 <-  df_list$ENC_Release2
All_Detections_1 <- df_list$All_Detections
WGFP_Clean_1 <- df_list$WGFP_Clean
unknown_tags_1 <-df_list$Unknown_Tags

                    

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(
            tabPanel("How to Use",
                     includeHTML(paste0("www/", "WGFP_dataclean_vis_about.html"))),
            tabPanel("Stationary Clean",
                     withSpinner(DT::dataTableOutput("stationary1"))),
            tabPanel("Biomark",
                     withSpinner(DT::dataTableOutput("biomark1"))),
            tabPanel("Mobile",
                     withSpinner(DT::dataTableOutput("mobile1")))
            
            
        ), #end of tabset panel
        
        ) #end of main panel
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    stationarycleandata <- reactive({
        WGFP_Clean_1
    })

    output$stationary1 <- renderDataTable({
        
        stationarycleandata()
    })
    
    biomarkdata <- reactive({
        Biomark
    })
    
    output$biomark1 <- renderDataTable({
        
        biomarkdata()
    })
    
    mobiledata <- reactive({
        Mobile
    })
    output$mobile1 <- renderDataTable({
        
        mobiledata()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
