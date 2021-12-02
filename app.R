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

#want to put this in the function when ready
# Enc_release_data <- ENC_Release2_1 %>%
#     mutate(Date = ifelse(str_detect(Date, "/"),
#                          as.character(mdy(Date)),
#                          Date))
                    

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("WGFP Data Exploration"),

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
                     withSpinner(DT::dataTableOutput("mobile1"))),
            tabPanel("All Detections",
                     withSpinner(DT::dataTableOutput("alldetections1"))),
            tabPanel("Encounter Release History",
                     withSpinner(DT::dataTableOutput("enc_release1")))
            
            
        ), #end of tabset panel
        
        ) #end of main panel
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #enc_releae_data wasn't registering bc i used reactive() instead of reactive ({}).
    #i guess reactive ({}) makes it so you can make multiple expressions within a reactive contect whereas reactive() can only do 1
    data_list <- reactive({
        #making release data dates in same format
        Enc_release_data <- df_list$ENC_Release2 %>%
            mutate(Date = ifelse(str_detect(Date, "/"),
                               as.character(mdy(Date)),
                               Date))


        
        d_list <- list(
            "stationarycleandata" = df_list$WGFP_Clean,
            "biomarkdata" = Biomark,
            "mobiledata" = Mobile,
            "all_det_data" = df_list$All_Detections,
            "enc_release_data" = Enc_release_data
        )
        
        return(d_list)
    }
        
        
        
    )
    
    # stationarycleandata <- reactive({
    #     WGFP_Clean_1
    # })
    # 
    # biomarkdata <- reactive({
    #     Biomark
    # })
    # 
    # mobiledata <- reactive({
    #     Mobile
    # })

    output$stationary1 <- renderDataTable({
        
        data_list()$stationarycleandata
    })
    
    
    output$biomark1 <- renderDataTable({
        
        data_list()$biomarkdata
    })
    
    
    output$mobile1 <- renderDataTable({
        
        data_list()$mobiledata
    })
    
    output$alldetections1 <- renderDataTable({
        
        data_list()$all_det_data
    })
    
    
    output$enc_release1 <- renderDataTable({
        
        data_list()$enc_release_data
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
