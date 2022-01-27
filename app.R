library(shiny)
library(shinycssloaders)
library(tidyverse) #error has occ
library(lubridate)
library(leaflet)
library(PBSmapping)
library(DT)
library(shinyWidgets) # for pickerinput
library(shinythemes)
library(bslib)
#Biomark is temporarily labelled as B3 and B4 to make data filtering easier
# tieh the site_code %in% picker1 line, because B1 and B2 are technically "in" RB1 and Rb2, it would include them to be part of it 
# so for now this is easier. but actually idk if this is true, it could have been some other problem
# cntrl + shft + A to reformat chunks of code
# rsconnect::showLogs(appName="WGFP_dataclean_vis",streaming=TRUE) will show logs when trying to load app browser
# had "application failed to start" error and fixed both times with above command. both times because packages in local environment (tidyverse and lubridate) weren't called with library() command 

# Data Read Ins -----------------------------------------------------------

#stationary1 <- read.csv(paste0("WGFP_Raw_20211130.csv"))

# if column names change in any of these read-ins, might require some modification to code to get them to combine
Stationary <- read.csv(paste0("WGFP_Raw_20220110.csv")) #WGFP_Raw_20211130.csv WGFP_Raw_20220110_cf6.csv
Mobile <- read.csv("WGFP_Mobile_Detect_AllData.csv" , colClasses= c(rep("character",14), rep("numeric", 4), rep("character", 3)))
Biomark <- read.csv("Biomark_Raw_20211109_1.csv", dec = ",")
Release <- read.csv("WGFP_ReleaseData_Master.csv", na.strings = c(""," ","NA"), colClasses=c(rep("character",8), "numeric", "numeric",rep("character",8) ))
Recaptures <- read.csv("WGFP_RecaptureData_Master.csv", na.strings = c(""," ","NA"), colClasses = c(rep("character", 9), rep("numeric", 2), rep("character", 8)))
Stationdata1 <- read_csv("EncounterHistory_AllData_wStations_20220114.csv", 
                         col_types = cols(
                           #OBJECTID = col_skip(), Join_Count = col_skip(), TARGET_FID = col_skip(), 
                           TAG = col_character(), Release_Length = col_number(), 
                           UTM_X = col_character(), UTM_Y = col_character(),
                           #Date_ = col_date(format = "%m/%d/%Y"),
                           Release_Weight = col_number()))

#  
Mobile <- Mobile %>%
    mutate(Date = as.character(mdy(Date)))

Release_05 <- Release %>%
  mutate(Date = as.character(mdy(Date)))

Recaptures_05 <- Recaptures %>%
  mutate(Date = as.character(mdy(Date)))

source("WGFP_EncounterHistoriesFunction.R")
source("Combine_events_stations_function.R")
source("get_movements_function.R")
source("Get_states_function.R")

df_list <- WGFP_Encounter_FUN(Stationary = Stationary, Mobile = Mobile, Release= Release, Biomark = Biomark, Recaptures = Recaptures)
All_events <- df_list$All_Events

combined_events_stations <- combine_events_and_stations(All_events, Stationdata1)

Movements_df <- get_movements_function(combined_events_stations)

Mx <- Movements_df %>%
  filter(is.na(marker_color))



#statesdf_list <- Get_states_function(All_events, Stationdata1)

WGFP_Clean_1 <- df_list$WGFP_Clean
unknown_tags_1 <-df_list$Unknown_Tags

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
             theme = shinytheme("sandstone"), #end of navbar page arguments; what follow is all inside it

               
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
             

# Individual Datasets UI ---------------------------------------------------

             
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
                                 hr(),
                                 downloadButton(outputId = "download3", label = "Save this data as CSV"),
                                 hr(),
                                 withSpinner(DT::dataTableOutput("stationary1"))),
                        tabPanel("Biomark",
                                 withSpinner(DT::dataTableOutput("biomark1"))),
                        tabPanel("Mobile",
                                 withSpinner(DT::dataTableOutput("mobile1"))),
                        tabPanel("Recaptures",
                                 withSpinner(DT::dataTableOutput("recaps1"))),
                        tabPanel("Release",
                                 withSpinner(DT::dataTableOutput("release1")))
                        
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
                          
                          checkboxInput("checkbox1", "Remove Duplicate Days, TAGs Events and UTMs"),
                          checkboxInput("checkbox2", "Remove Duplicate TAGs: doesn't work with TAG filter"), #deliberate decision not to add another if statement to have it actually work because it doesn't make sense you would use both at the same time
                          actionButton("button2", "Reset Filters"),
                          tags$hr(),
                          #submit button is limited in scope, doesn't even have a input ID , but works for controlling literally all inputs
                          #submitButton("Update inputs", icon("sync"))
                          
                          actionButton("button3", label = "Render Table", width = "100%")
                          
                        ), #end of sidebar 
                        mainPanel(tabsetPanel(
                          
                          tabPanel("Encounter Release History",
                                   hr(),
                                   downloadButton(outputId = "download1", label = "Save this data as CSV"),
                                   hr(),
                                   withSpinner(DT::dataTableOutput("enc_release1"))),
                          tabPanel("All Events",
                                   hr(),
                                   downloadButton(outputId = "download2", label = "Save this data as CSV"),
                                   hr(),
                                   withSpinner(DT::dataTableOutput("allevents1"))
                                   ) #end of tabpanel
                          
                          
                          )#end of encounter histories tabset panel within mainPanel
                        )#end of mainPanel
                        
                      ) #end of Encounter histories sidebar layout
                      
             ), #end of Encounter Histories Tab


# States UI -----------------------------------------------------


            tabPanel("Daily States and Movements",
                     sidebarLayout(
                       sidebarPanel(
                         actionButton("button4", label = "Get States: Takes ~ 2 min", width = "100%",
                                      onclick = "var $btn=$(this); setTimeout(function(){$btn.remove();},0);"),
                         hr(),
                         conditionalPanel(condition = "input.button4 == true",
                                          textInput("textinput2", label = "Filter by TAG"),
                                          pickerInput(inputId = "picker4",
                                                      label = "Select number of daily unique events:",
                                                      choices = c(1:6), #will need to be updated later on for uniqueness
                                                      selected = c(1:5),
                                                      multiple = TRUE,
                                                      options = list(
                                                        `actions-box` = TRUE #this makes the "select/deselect all" option
                                                      )
                                                      
                                                      
                                          ), #end of picker 4 input 
                                          
                                          pickerInput(inputId = "picker5",
                                                      label = "States:",
                                                      choices = c(LETTERS[1:12]), #will need to be updated later on for uniqueness
                                                      selected = c(LETTERS[1:12]),
                                                      multiple = TRUE,
                                                      options = list(
                                                        `actions-box` = TRUE #this makes the "select/deselect all" option
                                                      )
                                                      
                                                      
                                          ), #end of picker 5 input 
                                          
                                          
                                          actionButton("button5", label = "Render Table/Data", width = "100%")
                                          ) #end of conditional panel
                         
                       ),#end of sidebar panel
                       mainPanel(tabsetPanel(
                         tabPanel("States Dataframe",
                                  withSpinner(DT::dataTableOutput("states1"))),
                         tabPanel("States and Days Wide",
                                  withSpinner(DT::dataTableOutput("states2"))),
                         
                         
                         
                        )#end of tabsetPanel
                      )#end of mainPanel
                     )#end of sidebarLayout including sidebarPanel and Mainpanel
                     
                     ),#end of States and movements ui Tab

# Movements and Map UI Tab --------------------------------------------------------------

### note on map: if detectoins are close together, they'll be grouped and you can't do more resolution. But they can still be upstream/downstream movements if they're >= 10 m difference in station
#the filtering also automatically takes out NA values on movement with picker6; but all the NA movement onlys should be from fish where we have no release info for,
#and also from fish that have detections before their offical "release" back in May
#if marker_color or icon_color is NA, it wont get mapped or displayed in data
#picker wasn't wokring becuase I had 2 differnt pick
            tabPanel("Daily Movements Map and Data",
                     sidebarLayout(
                       sidebarPanel(
                                      textInput("textinput3", label = "Filter by TAG"),
                                      pickerInput(inputId = "picker6",
                                                  label = "Select Movement Type:",
                                                  choices = sort(unique(Movements_df$movement_only)),
                                                  selected = unique(Movements_df$movement_only),
                                                  multiple = TRUE,
                                                  options = list(
                                                    `actions-box` = TRUE #this makes the "select/deselect all" option
                                                  )
                                      ), #end of picker 6 input
                                      
                                      pickerInput(inputId = "picker7",
                                                  label = "Select Detection Type",
                                                  choices = sort(unique(Movements_df$det_type)),
                                                  selected = unique(Movements_df$det_type),
                                                  multiple = TRUE,
                                                  options = list(
                                                    `actions-box` = TRUE #this makes the "select/deselect all" option
                                                  )
                                                  ), #end of picker 7 
                                      # radioButtons(inputId = "radiobuttons1",
                                      #              label = "Select Data Frequency",
                                      #              choices = c("days", "weeks"),
                                      #              selected = "days",
                                      #              inline = TRUE
                                      # ),
                                      sliderInput("slider1", "Date",
                                                  min = min(df_list$All_Events$Date -1),
                                                  max = max(df_list$All_Events$Date +1),  
                                                  value = c(min(df_list$All_Events$Date -1),max(df_list$All_Events$Date +1)),
                                                  step = 1,
                                                  timeFormat = "%d %b %y",
                                                  animate = animationOptions(interval = 500, loop = FALSE)
                                      ),
        
                            
                                      actionButton("button7", label = "Render Map and Data")
                                      

                       ),#end of sidebar panel
                       mainPanel(
                                  withSpinner(leafletOutput("map1",height=700)), #forgot to do LeafletOutput so the map wasn't showing up
                                  withSpinner(DT::dataTableOutput("movements1"))
            
            
                       )#end of mainPanel
                     )#end of sidebarLayout including sidebarPanel and Mainpanel
            
            ),#end of Map ui Tab


         

    ) #end of navbar page
) #end of fluidpage


# Define server logic
# Warning: Error in validate_session_object: object 'session' not found solved by adding session to the part up here
server <- function(input, output, session) {
  

# Reset Filters Logic -----------------------------------------------------

  
  observeEvent(input$button2, {
    
    updateTextInput(session, "textinput1",
                    value = "")
    
    updateDateRangeInput(session, "drangeinput2",
                         start = "2020-08-01", 
                         end = max(df_list$All_Events$Date) + 1)
    
    updatePickerInput(session, "picker1",
                      selected = unique(df_list$All_Events$Event)
    )
    
    updatePickerInput(session, "picker2",
                      selected = unique(df_list$All_Events$Species)
    )
    
    updatePickerInput(session, "picker3",
                      selected = unique(df_list$All_Events$ReleaseSite)
    )
    
    updateCheckboxInput(session, "checkbox1",
                        value = NULL)
    
    updateCheckboxInput(session, "checkbox2",
                        value = NULL)
    
  }) #end of reset 
  
  #when button to make States DF list is pressed, update these picker options
  observeEvent(input$button4,{
    updatePickerInput(session, "picker4",
                      choices = sort(unique(initial_states_data_list()$All_States$daily_unique_events)),
                      selected = unique(initial_states_data_list()$All_States$daily_unique_events)
    )
    
    updatePickerInput(session, "picker5",
                      choices = sort(unique(initial_states_data_list()$All_States$State)),
                      selected = unique(initial_states_data_list()$All_States$State)
    )
   
  })
  
  
  
  #create slider input depending on data frequency
  # observe({
  #   
  #   # allDates <- unique(covidData$Date_reported)
  #   # eligibleDates <- allDates[xts::endpoints(allDates, on = input$frequency)]
  #   # 
  #   if(input$slider1 == "weeks"){
  #     stepSize = 7
  #   }else{
  #     stepSize = 1
  #   }
  #   
  #   output$dateUI <- renderUI({
  #     sliderInput("slider1", "Date",
  #                 min = min(df_list$All_Events$Date),
  #                 max = max(df_list$All_Events$Date),
  #                 value = min(df_list$All_Events$Date),
  #                 step = stepSize,
  #                 timeFormat = "%d %b %y",
  #                 animate = animationOptions(interval = 500, loop = FALSE)
  #     )
  #   })
  # })
  #   

# Ind D Reactives ---------------------------------------------------------
    
  
    indiv_datasets_list <- eventReactive(input$button1,{
      
      
      stationary_filtered <- df_list$WGFP_Clean %>%
        filter(DTY >= req(input$drangeinput1[1]) & DTY <= req(input$drangeinput1[2]),
               #TAG == input$textinput1 #not gonna do tag filtering for now
        )
      
      biomark_filtered <- Biomark %>%
        filter(Scan.Date >= input$drangeinput1[1] & Scan.Date <= input$drangeinput1[2])
      
      mobile_filtered <- Mobile %>%
        filter(Date >= input$drangeinput1[1] & Date <= input$drangeinput1[2])
      
      recaps_filtered <- Recaptures_05 %>%
        filter(Date >= input$drangeinput1[1] & Date <= input$drangeinput1[2])
      
      release_filtered <- Release_05 %>%
        filter(Date >= input$drangeinput1[1] & Date <= input$drangeinput1[2])
      
      
      indiv_d_list <- list(
        "stationarycleandata" = stationary_filtered,
        "biomarkdata" = biomark_filtered,
        "mobiledata" = mobile_filtered,
        "recapdata" = recaps_filtered,
        "releasedata" = release_filtered
        
      )
      
      return(indiv_d_list)
      
      
    })
    

# Enc Hist Reactives ------------------------------------------------------

    
    #enc_releae_data wasn't registering bc i used reactive() instead of reactive ({}).
    #i guess reactive ({}) makes it so you can make multiple expressions within a reactive context whereas reactive() can only do 1
    enc_hist_data_list <- eventReactive(input$button3,{
        
      # if the Tag filter is used or not 
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
          #this part is for making sure the sequence of events will make sense sequentially: tells where a fish started and ended the day and keeps other unique entries in between
          group_by(Date) %>%
          mutate(first_last = case_when(Datetime == min(Datetime) ~ "First_of_day",
                                        Datetime == max(Datetime) ~ "Last_of_day",
                                        Datetime != min(Datetime) & Datetime != max(Datetime) ~ "0")
          ) %>%
          ungroup() %>%
          #need to include UTM_X and UTM_Y so that you can get multiple daily detections of mobile antennas in different locations
          distinct(TAG, Event, Date, first_last, UTM_X, UTM_Y, .keep_all = TRUE) %>%
          select(-first_last)
        
        
    }
        #if there isn't a tag input along with first box checked
        if (input$checkbox1 == TRUE & input$checkbox2 == FALSE & input$textinput1 =='') {
          
          all_events_filtered <- df_list$All_Events %>%
            
            filter(
              Datetime >= input$drangeinput2[1] & Datetime <= input$drangeinput2[2],
              Event %in% input$picker1,
              Species %in% input$picker2,
              ReleaseSite %in% input$picker3) %>%
            #this part is for making sure the sequence of events will make sense
            # if there's no tag input then have to group_by TAG as well
            group_by(Date, TAG) %>% 
            mutate(first_last = case_when(Datetime == min(Datetime) ~ "First_of_day",
                                          Datetime == max(Datetime) ~ "Last_of_day",
                                          Datetime != min(Datetime) & Datetime != max(Datetime) ~ "0")
            ) %>%
            ungroup() %>%
            distinct(TAG, Event, Date, first_last,  UTM_X, UTM_Y, .keep_all = TRUE) %>%
            select(-first_last) 
          
          
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
    }) #end of ENC data list eventReactive
    

# States data reactives ---------------------------------------------------

    #want it so that when the first button4 is pressed, the whole dataset is made
    #then after that i want to render the table with button5 along with filters
    initial_states_data_list <- eventReactive(input$button4,{
      states_data1_list <- Get_states_function(combined_events_stations)
      
      
      return(states_data1_list)
        
    })
    
    filtered_states_data <- eventReactive(input$button5,{
      
      if(input$textinput2 != ''){ 
        states_data1 <- initial_states_data_list()$All_States %>%
          filter(TAG %in% c(input$textinput2),
                 daily_unique_events %in% input$picker4,
                 State %in% input$picker5
                 )
      } else { 
        states_data1 <- initial_states_data_list()$All_States %>%
          filter(
            daily_unique_events %in% input$picker4,
            State %in% input$picker5
          )      
        }

      
      return(states_data1)
    }) 

# Movement Data Reactives -------------------------------------------------

    
    filtered_movements_data <- eventReactive(input$button7,{
      # movements_data1 <- Movements_df %>%
      #   filter(movement_only %in% input$picker6)
      
      if(input$textinput3 != ''){
        movements_data1 <- Movements_df %>%
          filter(TAG %in% c(input$textinput3),
                 Date >= input$slider1[1] & Date <= input$slider1[2],
                 #Date == input$slider1,
                 movement_only %in% c(input$picker6),
                 det_type %in% c(input$picker7)
                 
                 # daily_unique_events %in% input$picker4,
                 # State %in% input$picker5
          ) %>%
          arrange(Datetime)
        
        movements_data1$id <- seq.int(nrow(movements_data1))
        
      } else {
        movements_data1 <- Movements_df  %>% #initial_states_data_list()$Movements
          filter(
            Date >= input$slider1[1] & Date <= input$slider1[2],
            movement_only %in% c(input$picker6),
            det_type %in% c(input$picker7)
            
            # daily_unique_events %in% input$picker4,
            # State %in% input$picker5
          ) %>%
          arrange(Datetime)
        movements_data1$id <- seq.int(nrow(movements_data1))
        
      }
      
      
      return(movements_data1)
    }) 

      

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
    
    output$recaps1 <- renderDataTable(
      
      indiv_datasets_list()$recapdata,
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
    
    output$release1 <- renderDataTable(
      
      indiv_datasets_list()$releasedata,
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
    
    output$allevents1 <- renderDataTable({
      datatable(enc_hist_data_list()$allevents_data,
                  rownames = FALSE,
                  #extensions = c('Buttons'),
                  #for slider filter instead of text input
                  filter = 'top',
                  options = list(
                    pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again #errorin list: arg 5 is empty because I had a comma after the dom argument so it thought there was gonna be another argument input
                    language = list(emptyTable = "Enter inputs and press Render Table")
                  ) #end of options list
                ) %>%
        formatStyle(
          columns = 1:ncol(enc_hist_data_list()$allevents_data)
          #rownames = FALSE
        )

      
    })

# States Datatable Renders ------------------------------------------------


    output$states1 <- renderDT({
      
      input$button5
      
        datatable(filtered_states_data(), #initial_states_data_list()$All_States
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
        ) %>%
          formatStyle(
            columns = c(1:ncol(initial_states_data_list()$All_States))
            
          )
      
      
      
    })
    
    output$states2 <- renderDT({
      
      
      datatable(initial_states_data_list()$Days_and_states_wide,
                rownames = FALSE,
                
                filter = 'top',
                options = list(
                  pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                  dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                  language = list(emptyTable = "Enter inputs and press Render Table")
                  
                  #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
                )
      ) 
        
      
    })
    
    output$movements1 <- renderDT({
      
      
      datatable(filtered_movements_data(),
                rownames = FALSE,
                selection = "single",
                filter = 'top',
                options = list(
                  #statesave is restore table state on page reload
                  stateSave =TRUE,
                  pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                  dom = 'Blfrtip', #had to add 'lowercase L' letter to display the page length again
                  language = list(emptyTable = "Enter inputs and press Render Table")
                  
                  #buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
                )
      ) 
      
      
    })
    
    
    #input$_rows_selected
    
# Map proxy for Icons -----------------------------------------------------
    
    
    # to keep track of previously selected row
    #setting to nothing for now
    row_selected1 <- reactiveVal()
    #this is what the new icon for selected rows looks like
    my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'lightblue', iconColor = 'red')
    
    icons <- reactive({
        awesomeIcons(
          icon = 'ios-close',
          iconColor = filtered_movements_data()$icon_color,
          library = 'ion',
          markerColor = filtered_movements_data()$marker_color
        )
    })
    
    #group_name <- "my_additons"
    #230000228991
    
    observeEvent(input$movements1_rows_selected, {
      row_selected = filtered_movements_data()[input$movements1_rows_selected,]
      
      proxy <- leafletProxy('map1')
      #print(row_selected)
      #print(input$movements1_rows_selected)
      proxy %>%
        #clearing the group removes previous marker from previuous row before making a new one
        clearGroup(group = "markers") %>%
      
        addAwesomeMarkers(
          clusterOptions = markerClusterOptions(),
          group = "markers",
          popup = paste(
            "TAG:", row_selected$TAG, "<br>",
            "Release Site:", row_selected$ReleaseSite, "<br>",
            "Detection Event:", row_selected$det_type, "<br>",
            "Date:", row_selected$Datetime),
          
          layerId = as.character(row_selected$id),
          lng=row_selected$X, 
          lat=row_selected$Y,
          icon = my_icon)
      
      # if(is.null(row_selected1()))
      # {
      #   
      #   # prev_icon <- makeAwesomeIcon(icon = 'ios-close',
      #   #                              library = 'ion',
      #   #                              markerColor = prev_row()$marker_color,
      #   #                              iconColor =prev_row()$icon_color)
      #   #print(as.character(prev_row()$id))
      #   proxy %>%
      #     clearGroup(group = "markers")
      #   #removeMarker(layerId = as.character(prev_row()$id))
      #   
      #   # clearGroup(group_name) %>%
      #   #removeMarker(layerId = as.character(prev_row()$id))
      #   #when there has been a marker clicked before, add a marker that looks the same as the old one.
      #   #markers made with proxy doesn't integrate with original marker mapping
      #   #need to find a way to actually remove the previous markers made with proxy
      #   # addAwesomeMarkers(
      #   #   #group = group_name,
      #   #
      #   #   clusterOptions = markerClusterOptions(),
      #   #   icon = prev_icon,
      #   #   label = paste(prev_row()$movement_only, "\n",
      #   #                 prev_row()$Date),
      #   #   popup=paste(
      #   #     "TAG:", prev_row()$TAG, "<br>",
      #   #     "Release Site:", prev_row()$ReleaseSite, "<br>",
      #   #     "Detection Event:", prev_row()$det_type, "<br>",
      #   #     "Date:", prev_row()$Datetime),
      #   #   layerId = as.character(prev_row()$id),
      #   #   lng=prev_row()$X,
      #   #   lat=prev_row()$Y)
      # }
      # 
      # 
      # row_selected1(row_selected)
      
      # Reset previously selected marker
      #says if there is a row that was selected before, go back to using the normal icons
      
      #prev_icon <- makeAwesomeIcon(icon = 'ios-close', markerColor = prev_row()$marker_color, prev_row()$icon_color)

     
     })
    
    
  
    

# Movements Map Output ----------------------------------------------------

    output$map1 <- renderLeaflet({
      
      
      
      
      leaflet(filtered_movements_data()) %>% #Warning: Error in UseMethod: no applicable method for 'metaData' applied to an object of class "NULL"  solved becuase leaflet() needs an arg leaflet(x)
        addProviderTiles(providers$Esri.WorldImagery,options = providerTileOptions()) %>%
        
        addAwesomeMarkers(
          #group = "original",
          clusterOptions = markerClusterOptions(),
          lng=~X, 
          lat = ~Y,
          icon = icons(),
          label = paste(filtered_movements_data()$movement_only, "\n",
                        filtered_movements_data()$Date),
          layerId = as.character(filtered_movements_data()$id),
          popup = paste(
            "TAG:", filtered_movements_data()$TAG, "<br>",
            "Release Site:", filtered_movements_data()$ReleaseSite, "<br>",
            "Detection Event:", filtered_movements_data()$det_type, "<br>",
            "Date:", as.character(filtered_movements_data()$Datetime))
          )
      
    })
    
    #when map is clicked, go to that icon
    # pagination doesn't work right now
    observeEvent(input$map1_marker_click, {
      #need to assign layer ID's in leafletrender to have an id associated with the click
      #clicking the map gives info in the form of a list, including the layer id assigned in leaflet
      clickId <- input$map1_marker_click$id
      #print(clickId)
      #print(input$movements1_state$length)
      print(which(filtered_movements_data()$id == clickId))
      #saying get the rows in the data with the same id as clickId; clickId is the row number
      dataTableProxy("movements1") %>%
        selectRows(which(filtered_movements_data()$id == clickId)) %>%
        selectPage(which(input$movements1_rows_all == clickId) %/% input$movements1_state$length + 1)
    })

 
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
    ) #end of download1
    
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
    
    output$download3 <- downloadHandler(
      filename = 
        function() {
          paste0("StationaryClean_",most_recent_date,".csv")
        }
      ,
      content = function(file) {
        write_csv(indiv_datasets_list()$stationarycleandata, file)
        
        
      }
    ) #end of download2
}

# Run the application 
shinyApp(ui = ui, server = server)
