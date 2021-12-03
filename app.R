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
library(shinyWidgets)


Stationary = read.csv(paste0("WGFP_Raw_20211130.csv"))
Mobile = read.csv("WGFP_MobileDetections.csv", colClasses=c(rep("character",10)))
Biomark <- read.csv("Biomark_Raw_20211109_1.csv", dec = ",")
Release = read.csv("WGFP_ReleaseData_Master.csv",colClasses=c(rep("character",18)))

Mobile <- Mobile %>%
    mutate(MobileDate = as.character(mdy(MobileDate)))

source("WGFP_EncounterHistoriesFunction.R")

df_list <- WGFP_Encounter_FUN(Stationary = Stationary, Mobile = Mobile, Release= Release, Biomark = Biomark)

All_Detections_1 <- df_list$All_Detections
WGFP_Clean_1 <- df_list$WGFP_Clean
unknown_tags_1 <-df_list$Unknown_Tags

Enc_release_data <- df_list$ENC_Release2 %>%
    mutate(Date = ifelse(str_detect(Date, "/"),
                         as.character(mdy(Date)),
                         Date))

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
            textInput("textinput1", "Filter by Tag"),
            dateRangeInput("drangeinput1", "Select a Date Range:",
                           start = "2020-09-03", 
                           end = max(df_list$All_Detections$Scan_Date)), #end of date range input
            checkboxGroupInput("checkboxgroup1", "Select Antennas", 
                               choices = unique(df_list$All_Detections$Site_Code),
                               selected = unique(df_list$All_Detections$Site_Code)),
            actionLink("selectall","Select All"),
            checkboxInput("checkbox1", "Remove Duplicate Days")
        ), #end of sidebar panel

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
    )#end of sidebarLayout
)

# Define server logic
#Warning: Error in validate_session_object: object 'session' not found solved by adding session to the part up here
server <- function(input, output, session) {
    
    observe({
        if(input$selectall == 0) return(NULL) 
        else if (input$selectall%%2 == 0)
        {
            updateCheckboxGroupInput(session,"checkboxgroup1","Select Antennas",choices=unique(df_list$All_Detections$Site_Code))
        }
        else
        {
            updateCheckboxGroupInput(session,"checkboxgroup1","Select Antennas",choices=unique(df_list$All_Detections$Site_Code),selected=unique(df_list$All_Detections$Site_Code))
        }
    })
    
    #enc_releae_data wasn't registering bc i used reactive() instead of reactive ({}).
    #i guess reactive ({}) makes it so you can make multiple expressions within a reactive contect whereas reactive() can only do 1
    data_list <- reactive({
        stationary_filtered <- df_list$WGFP_Clean %>%
            filter(DTY >= input$drangeinput1[1] & DTY <= input$drangeinput1[2],
                   TAG == input$textinput1
                   )
        
        biomark_filtered <- Biomark %>%
            filter(Scan.Date >= input$drangeinput1[1] & Scan.Date <= input$drangeinput1[2])
        
        mobile_filtered <- Mobile %>%
            filter(MobileDate >= input$drangeinput1[1] & MobileDate <= input$drangeinput1[2])
        
        all_det_filtered <- df_list$All_Detections %>%
            filter(Scan_Date >= input$drangeinput1[1] & Scan_Date <= input$drangeinput1[2],
                   Site_Code %in% input$checkboxgroup1)
        # error below solved because I wasn't using the correct variable names for each dataset
        # x `Site_Code` not found in `.data`.
        # x `Scan_Date` not found in `.data` 
        
        # this part doesn't work because if the box is checked, the rest of the filters don't apply
    if (input$checkbox1 == TRUE) {
        stationary_filtered <- df_list$WGFP_Clean %>%
            distinct(TAG, SCD, DTY, .keep_all = TRUE)
        
        biomark_filtered <- Biomark %>%
            distinct(DEC.Tag.ID, Reader.ID, Scan.Date, .keep_all = TRUE)
        
        mobile_filtered <- Mobile %>%
            distinct(TAG, MobileAnt, MobileDate, .keep_all = TRUE)
        
        all_det_filtered <- df_list$All_Detections %>%
            distinct(TAG, Site_Code, Scan_Date, .keep_all = TRUE)
        
    }
      
        d_list <- list(
            "stationarycleandata" = stationary_filtered,
            "biomarkdata" = biomark_filtered,
            "mobiledata" = mobile_filtered,
            "all_det_data" = all_det_filtered,
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
