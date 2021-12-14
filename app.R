library(shiny)
library(shinycssloaders)
library(tidyverse) #error has occ
library(lubridate)
library(DT)
library(shinyWidgets) # for pickerinput
library(shinythemes)
library(bslib)
#Biomark is temporarily labelled as B3 and B4 to make data filtering easier
# tieh the site_code %in% picker1 line, because B1 and B2 are technically "in" RB1 and Rb2, it would include them to be part of it 
# so for now this is easier
# cntrl + shft + A to reformat chunks of code
# rsconnect::showLogs(appName="WGFP_dataclean_vis",streaming=TRUE) will show logs when trying to load app browser
# had "application failed to start" error and fixed both times with above command. both times because packages in local environment (tidyverse and lubridate) weren't called with library() command 
# runs slow right now anyway. might be worth putting at least release data outside reactive context

# Data Read Ins -----------------------------------------------------------


# if column names change in any of these read-ins, might require some modification to code to get them to combine
Stationary <- read.csv(paste0("WGFP_Raw_20211130.csv"))
Mobile <- read.csv("WGFP_MobileDetections.csv", colClasses=c(rep("character",10)))
Biomark <- read.csv("Biomark_Raw_20211109_1.csv", dec = ",")
Release <- read.csv("WGFP_ReleaseData_Master.csv",colClasses=c(rep("character",8), "numeric", "numeric",rep("character",8) ))
Recaptures <- read.csv("WGFP_RecaptureData_Master.csv", colClasses = c(rep("character", 9), rep("numeric", 2), rep("character", 8)))

  
Mobile <- Mobile %>%
    mutate(MobileDate = as.character(mdy(MobileDate)))

source("WGFP_EncounterHistoriesFunction.R")

df_list <- WGFP_Encounter_FUN(Stationary = Stationary, Mobile = Mobile, Release= Release, Biomark = Biomark, Recaptures = Recaptures)

WGFP_Clean_1 <- df_list$WGFP_Clean
unknown_tags_1 <-df_list$Unknown_Tags
all_events <- df_list$All_Events

Enc_release_data <- df_list$ENC_Release2 %>%
    mutate(Date = ifelse(str_detect(Date, "/"),
                         as.character(mdy(Date)),
                         Date))
most_recent_date <- max(df_list$All_Events$Date)  

#want to put this in the function when ready
# Enc_release_data <- ENC_Release2_1 %>%
#     mutate(Date = ifelse(str_detect(Date, "/"),
#                          as.character(mdy(Date)),
#                          Date))
                    
rainbow_trout_pallette <- list(pink1 = "#E3BABBFF", olive_green1 = "#C4CFBFFF", dark_olive_green1 = "#81754EFF",
                               mustard_yellow1 = "#CBA660FF", brown_yellow = "#86551CFF" )
# Define UI for application that draws a histogram

ui <- fluidPage(
  
  navbarPage(title = "WGFP Data Exploration",
             theme = shinytheme("lumen"), #end of navbar page arguments; what follow is all inside it

               
             #   bs_theme(
             #   bg = rainbow_trout_pallette$dark_olive_green1,
             #   fg = rainbow_trout_pallette$pink1,
             #   primary = "#CBA660FF",
             #   secondary = "#86551CFF",
             #   base_font = "#E3BABBFF",
             #   code_font =  "#C4CFBFFF"
             #     
             # ),
             
             # tags$head(tags$style(HTML('.navbar-static-top {background-color: green;}',
             #                           '.navbar-default .navbar-nav>.active>a {background-color: green;}'))),
             # 
             tabPanel("About/How to Use",
                      includeHTML(paste0("www/", "WGFP_dataclean_vis_about.html"))
                      ), #end fo how to use TabPanel
             

# Individual Datsets UI ---------------------------------------------------

             
             tabPanel("Individual Datasets",
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput("drangeinput1", "Select a Date Range:",
                                         #was accidnetly omitting events from the allevents tab bc the earliest release date is 2020-09-01
                                         #earliest detection was 2020-09-03, which was what it was set at before
                                         start = "2020-08-01", 
                                         end = max(df_list$All_Events$Date)+1), #end of date range input
                          actionButton("button1", label = "Render Table")
                          ),
                       
                      mainPanel(tabsetPanel(
                        tabPanel("Stationary Clean",
                                 withSpinner(DT::dataTableOutput("stationary1"))),
                        tabPanel("Biomark",
                                 withSpinner(DT::dataTableOutput("biomark1"))),
                        tabPanel("Mobile",
                                 withSpinner(DT::dataTableOutput("mobile1")))
                        
                          ) #end of sidebarlayout: incldes sidebar panel and mainpanel
                        ) #end of individual datasets tabset panel
                      )#end of individual datasets Mainpanel)
               
                    ),#end of Individual data tab panel
             
             # new Tab "Encounter Histories"

# Encounter Histories UI --------------------------------------------------

             
             tabPanel("Encounter Histories",
                      sidebarLayout(
                        sidebarPanel(
                          textInput("textinput1", "Filter by Tag"),
                          dateRangeInput("drangeinput2", "Select a Date Range:",
                                         start = "2020-08-01", 
                                         end = max(df_list$All_Events$Date) + 1), #end of date range input
                          pickerInput(inputId = "picker1",
                                      label = "Select Event",
                                      choices = unique(df_list$All_Events$Event),
                                      selected = unique(df_list$All_Events$Event),
                                      multiple = TRUE,
                                      options = list(
                                        `actions-box` = TRUE #this makes the "select/deselect all" option
                                      ),
                                      
                          ), #end of picker input
                          
                          pickerInput(inputId = "picker2",
                                      label = "Select Fish Species:",
                                      choices = sort(unique(df_list$All_Events$Species)),
                                      selected = unique(df_list$All_Events$Species),
                                      multiple = TRUE,
                                      options = list(
                                        `actions-box` = TRUE #this makes the "select/deselect all" option
                                      ),
                                      
                          ), #end of picker 2 input
                          
                          pickerInput(inputId = "picker3",
                                      label = "Select Release Site:",
                                      choices = sort(unique(df_list$All_Events$ReleaseSite)),
                                      selected = unique(df_list$All_Events$ReleaseSite),
                                      multiple = TRUE,
                                      options = list(
                                        `actions-box` = TRUE #this makes the "select/deselect all" option
                                      )
                                      # choicesOpt = list(
                                      #   style = rep(("color: black; background: lightgrey; font-weight: bold;"),10)
                                      # ) #end of choices opt
                                      
                          ), #end of picker 3 input
                          
                          checkboxInput("checkbox1", "Remove Duplicate Days, TAGs and Sites"),
                          checkboxInput("checkbox2", "Remove Duplicate TAGs: doesn't work with TAG filter"), #deliberate decision not to add another if statement to have it actually work because it doesn't make sense you would use both at the same time
                          #submit button is limited in scope, doesn't even have a input ID , but works for controlling literally all inputs
                          #submitButton("Update inputs", icon("sync"))
                          actionButton("button2", label = "Render Table")
                        ), #end of sidebar 
                        mainPanel(tabsetPanel(
                          
                          tabPanel("Encounter Release History",
                                   downloadButton(outputId = "download1", label = "Save Enc Hist as CSV"),
                                   withSpinner(DT::dataTableOutput("enc_release1"))),
                          tabPanel("All Events",
                                   downloadButton(outputId = "download2", label = "Save All Events as CSV"),
                                   withSpinner(DT::dataTableOutput("allevents1"))
                                   ) #end of tabpanel
                          
                          
                          )#end of encounter histories tabset panel within mainPanel
                        )#end of mainPanel
                        
                      ) #end of Encounter histories sidebar layout
                      
             ) #end of Encounter Histories Tab
    ) #end of navbar page
) #end of fluidpage


# Define server logic
# Warning: Error in validate_session_object: object 'session' not found solved by adding session to the part up here
server <- function(input, output, session) {
    

# Ind D Reactives ---------------------------------------------------------

  
    indiv_datasets_list <- eventReactive(input$button1,{
      
      
      stationary_filtered <- df_list$WGFP_Clean %>%
        filter(DTY >= req(input$drangeinput1[1]) & DTY <= req(input$drangeinput1[2]),
               #TAG == input$textinput1 #not gonna do tag filtering for now
        )
      
      biomark_filtered <- Biomark %>%
        filter(Scan.Date >= input$drangeinput1[1] & Scan.Date <= input$drangeinput1[2])
      
      mobile_filtered <- Mobile %>%
        filter(MobileDate >= input$drangeinput1[1] & MobileDate <= input$drangeinput1[2])
      
      indiv_d_list <- list(
        "stationarycleandata" = stationary_filtered,
        "biomarkdata" = biomark_filtered,
        "mobiledata" = mobile_filtered
        
      )
      
      return(indiv_d_list)
      
      
    })
    

# Enc Hist Reactives ------------------------------------------------------

    
    #enc_releae_data wasn't registering bc i used reactive() instead of reactive ({}).
    #i guess reactive ({}) makes it so you can make multiple expressions within a reactive context whereas reactive() can only do 1
    enc_hist_data_list <- eventReactive(input$button2,{
        
        #input$textinput1
      
      # all_events_filtered <- df_list$All_Events  %>%
      #   filter(
      #     #{if (input$textinput1 != "") TAG == input$textinput1},
      #     
      #     TAG %in% c(input$textinput1),
      #     Datetime >= input$drangeinput2[1] & Datetime <= input$drangeinput2[2],
      #     Event %in% input$picker1,
      #     Species %in% input$picker2,
      #     ReleaseSite %in% input$picker3
      #   ) 
      # # %>%
      # #   {if (input$textinput1 != "") filter(df_list$All_Events$TAG == input$textinput1)}
      # 
      
      if(input$textinput1 !=''){
        #all events
        all_events_filtered <- df_list$All_Events  %>%
          filter(

            TAG %in% c(input$textinput1),
            Datetime >= input$drangeinput2[1] & Datetime <= input$drangeinput2[2],
            Event %in% input$picker1,
            Species %in% input$picker2,
            ReleaseSite %in% input$picker3
          )
        
        Enc_release_data_filtered <- Enc_release_data %>%
          filter(
            TAG %in% c(input$textinput1),
            Species %in% input$picker2,
            ReleaseSite %in% input$picker3)
        
      } else {
        all_events_filtered <- df_list$All_Events  %>%
          filter(
            
            Datetime >= input$drangeinput2[1] & Datetime <= input$drangeinput2[2],
            Event %in% input$picker1,
            Species %in% input$picker2,
            ReleaseSite %in% input$picker3
          )
        
        Enc_release_data_filtered <- Enc_release_data %>%
          filter(
            Species %in% input$picker2,
            ReleaseSite %in% input$picker3)
      }
        
        

        Enc_release_data_filtered <- Enc_release_data %>%
            filter(
              #TAG %in% c(input$textinput1),
              Species %in% input$picker2,
                   ReleaseSite %in% input$picker3)
        
        # error below solved because I wasn't using the correct variable names for each dataset
        # x `Site_Code` not found in `.data`.
        # x `Scan_Date` not found in `.data` 
      ### Filtering for TAG, SIte Code, and Day  
        
        #if there is a tag input along with the first box checked
    if (input$checkbox1 == TRUE & input$checkbox2 == FALSE & input$textinput1 !='') {
        
        all_events_filtered <- df_list$All_Events %>%
          
          filter(
            TAG == input$textinput1,
            Datetime >= input$drangeinput2[1] & Datetime <= input$drangeinput2[2],
                 Event %in% input$picker1,
                 Species %in% input$picker2,
                 ReleaseSite %in% input$picker3) %>%
          distinct(TAG, Event, Date, .keep_all = TRUE) 
        
        
    }
        #if there isn't a tag input along with first box checked
        if (input$checkbox1 == TRUE & input$checkbox2 == FALSE & input$textinput1 =='') {
          
          all_events_filtered <- df_list$All_Events %>%
            
            filter(
              Datetime >= input$drangeinput2[1] & Datetime <= input$drangeinput2[2],
              Event %in% input$picker1,
              Species %in% input$picker2,
              ReleaseSite %in% input$picker3) %>%
            distinct(TAG, Event, Date, .keep_all = TRUE) 
          
          
        }
        
        if (input$checkbox2 == TRUE) {
          
          
          
          all_events_filtered <- df_list$All_Events %>%
            filter(
              #TAG == input$textinput1,
                    Datetime >= input$drangeinput2[1] & Datetime <= input$drangeinput2[2],
                   Event %in% input$picker1,
                   Species %in% input$picker2,
                   ReleaseSite %in% input$picker3) %>%
            #need to have distinct() at the end of the expression
            distinct(TAG, .keep_all = TRUE) 
        }

        enc_hist_d_list <- list(
            "enc_release_data" = Enc_release_data_filtered,
            "allevents_data" = all_events_filtered
        )
        
        return(enc_hist_d_list)
    }
        
        
        
    )
    
    

# Datatable renders -------------------------------------------------------

    
    output$stationary1 <- DT::renderDataTable(
        
      
      indiv_datasets_list()$stationarycleandata,
        rownames = FALSE,
        #extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
        
    )
    
    
    output$biomark1 <- renderDataTable(
        
      indiv_datasets_list()$biomarkdata,
        rownames = FALSE,
        #extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
    )
    
    
    output$mobile1 <- renderDataTable(
        
      indiv_datasets_list()$mobiledata,
        rownames = FALSE,
        #extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Enter inputs and press Render Table")
          
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
    )
    
    # Dt 
    
    output$enc_release1 <- renderDataTable(
        
      enc_hist_data_list()$enc_release_data,
        rownames = FALSE,
        #extensions = c('Buttons'),
        #for slider filter instead of text input
        filter = 'top',
        options = list(
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again #errorin list: arg 5 is empty because I had a comma after the dom argument so it thought there was gonna be another argument input
          language = list(emptyTable = "Enter inputs and press Render Table")
          
          #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
        )
    )
    
    output$allevents1 <- renderDataTable(
      enc_hist_data_list()$allevents_data,
      rownames = FALSE,
      #extensions = c('Buttons'),
      #for slider filter instead of text input
      filter = 'top',
      options = list(
        pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
        dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again #errorin list: arg 5 is empty because I had a comma after the dom argument so it thought there was gonna be another argument input
        language = list(emptyTable = "Enter inputs and press Render Table")
      ) #end of options list
      
    )
    

# Download Handlers -------------------------------------------------------

    
    output$download1 <- downloadHandler(
      filename = 
        function() {
          paste0("ReleaseEncounters_",most_recent_date,".csv")
        }
      ,
      content = function(file) {
        write_csv(enc_hist_data_list()$enc_release_data, file)
        
        
      }
    ) #end of download2
    
    output$download2 <- downloadHandler(
      filename = 
        function() {
          paste0("allevents_",most_recent_date,".csv")
        }
      ,
      content = function(file) {
        write_csv(enc_hist_data_list()$allevents_data, file)
        
        
      }
    ) #end of download2
}

# Run the application 
shinyApp(ui = ui, server = server)
