# this should be get_states_function
### current assumptions: 
# if a fish hits the upstream antenna and not the downstream one on a given day, it is assumed to  have swam back upstream and NOT just missed the downstream antenna
# vice versa for downstream antenna
# so also if a fish is assumed to be downstream of RB1, and last hit RB1 only, then hits RB1 again, that is assumed as an upstream movement

# if a fish is released upstream of an antenna 

# hitching post release site is below the antennas and if a fish released at hithcing post hits the hithcin post antennas, it's an upstream movement

# no mobile detections incorporated for now
All_events <- df_list$All_Events
# Stationdata1 <- read_csv("EncounterHistory_AllData_wStations_20220114.csv", 
#                          col_types = cols(
#                            #OBJECTID = col_skip(), Join_Count = col_skip(), TARGET_FID = col_skip(), 
#                                           TAG = col_character(), Release_Length = col_number(), 
#                                           UTM_X = col_character(), UTM_Y = col_character(),
#                                           #Date_ = col_date(format = "%m/%d/%Y"),
#                                           Release_Weight = col_number()))



station_data <- Stationdata1

Get_states_function <- function(All_events, station_data) {
  library(tidyverse) 
  library(lubridate)
  
  start_time <- Sys.time()
  

# Combining stations into all_events dataset ------------------------------
  #just getting distinct rows makes joining easier; all we need from this df is stations
  stations <- station_data %>%
    rename(
      Datetime = Datetime_,
      Time = Time_) %>%
    mutate(
      Date = mdy(Date_),
      ET_STATION = case_when(station_data$River %in% "Fraser River" ~ station_data$ET_STATION + 9566, #9566 is above Fraser River Confluence
                                  station_data$River %in% "Colorado River" ~ station_data$ET_STATION)) %>%
    distinct(Event, UTM_X, UTM_Y, TAG, .keep_all = TRUE) %>%
    select(-Date_)
    
  
  #massive datafrmae occurs when there are multiple rows in B for which the key columns (same-name columns by default) match the same, single row in A
  #usually this means you have to make sure you join by the fields which will not have any differenitation: iun this case, "TAG", UTM_X", "UTM_Y", and "Event". The other fields are just to help keep the dataframe more concise
  
  all_events_stations_2 <- left_join(All_events, stations, by = c("TAG", "UTM_X", "UTM_Y", "Event")) # "Species", "Release_Length", "Release_Weight", "Event", "Date", "Time", "ReleaseSite", "Release_Date", "RecaptureSite", "Recap_Length", "Recap_Weight"
  
  all_events_stations_21 <- all_events_stations_2 %>%
    filter(is.na(ET_STATION))

  All_events_stations_3 <- all_events_stations_2 %>%
    # select(-ET_STATION) %>%
    # rename(ET_STATION = ET_STATION1) %>%
    mutate(ET_STATION = case_when(
                                  (Event %in% c("RB1", "RB2")) ~ 4150, # 
                                  is.na(ET_STATION) & (Event %in% c("HP3", "HP4")) ~ 6340,
                                  is.na(ET_STATION) & (Event %in% c("CF5", "CF6")) ~ 9550,
                                  is.na(ET_STATION) & (Event %in% c("B3")) ~ 8290,
                                  !is.na(ET_STATION) & (!Event %in% c("RB1", "RB2")) ~ ET_STATION)) %>%
    # this line just makes the df smaller if htere are duplicates; usually doesn't change anything
    distinct(Datetime.x, Event, TAG, .keep_all =TRUE) %>%
    rename(
      
      Date = Date.x,
      Time = Time.x,
      Datetime = Datetime.x,
      
      Species = Species.x,
      Release_Length = Release_Length.x,
      Release_Weight = Release_Weight.x, 
      ReleaseSite = ReleaseSite.x,
      Release_Date = Release_Date.x,
      RecaptureSite = RecaptureSite.x,
      Recap_Length = Recap_Length.x,
      Recap_Weight = Recap_Weight.x
      
    ) %>%
    select(Date, Time, Datetime, TAG, Event, Species, Release_Length, Release_Weight, ReleaseSite, Release_Date, RecaptureSite, River, Recap_Length, Recap_Weight, UTM_X, UTM_Y, ET_STATION)
  

# Days_since and Prev_event -----------------------------------------------

  
  All_events_days <- All_events_stations_3 %>%
    mutate(days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days")))
    )
  
  
  All_events_days1 <- All_events_days %>%
    #filter(!Event %in% c("M1", "M2")) %>% #filtering out mobile detections for now
    
    group_by(Date, TAG) %>%
    mutate(first_last = case_when(Datetime == min(Datetime) & Event != "Release" ~ "First_of_day",
                                  Datetime == max(Datetime) ~ "Last_of_day",
                                  Datetime != min(Datetime) & Datetime != max(Datetime) ~ "0",
                                  #Event == "Release" ~ "Last_of_day"
                                  ),
           c_number_of_detections = n(),
           daily_unique_events = length(unique(Event))
    ) %>%
    ungroup() %>%
    distinct(TAG, Event, Date, first_last, UTM_X, UTM_Y, .keep_all = TRUE) %>%
    
    group_by(TAG) %>%
    mutate(
      previous_event = lag(Event, order_by = Datetime)
      # next_event = lead(Event, order_by = Datetime),
      # next_event_2 = lead(Event, n = 2, order_by = Datetime
                          # ),
      
      #same_day_next_events = (lead(Date, order_by = Datetime) == lead(Date, n = 2, order_by = Datetime))
  ) %>%
    
    select(Date, Datetime,TAG,Event,ReleaseSite,Species, Release_Length, Release_Weight, Release_Date, RecaptureSite, River, days_since, first_last, previous_event,  c_number_of_detections, daily_unique_events, ET_STATION) #next_event, next_event_2, same_day_next_events,

  r1 <- All_events_days1 %>%
    # no need to group_by date until states will be consolidated
    # need to group_by tag though so that the Lag(Date) will get the last date that that fish was detected
    # some movements weren't being recorded correctly because it was grouping by both date and Tag
    group_by(TAG) %>%
    
    mutate(
      
      current_event_vals = case_when(Event == "RB1" ~ 11.9,
                                     Event == "RB2" ~ 11.1,
                                     Event == "HP3" ~ 7.9,
                                     Event == "HP4" ~ 7.1,
                                     Event == "CF5" ~ 4.9,
                                     Event == "CF6" ~ 4.1,
                                     Event == "B3" ~ 6,
                                     Event == "B4" ~ .9,
                                     
                                     # #rekease site needs values for if a fish is only released and recaptured, #230000228364 it will have movement
                                     # Event == "Release" & ReleaseSite == "Lower River Run" ~ 4,
                                     # Event == "Release" & ReleaseSite == "Fraser River Ranch" ~ 2,
                                     # Event == "Release" & ReleaseSite == "Kaibab Park" ~ 1,
                                     # Event == "Release" & ReleaseSite == "Upper River Run" ~ 3,
                                     # Event == "Release" & ReleaseSite == "Below Confluence Antenna" ~ 5,
                                     # Event == "Release" & ReleaseSite == "Windy Gap Dam" ~ 6,
                                     # Event == "Release" & ReleaseSite == "Hitching Post" ~ 7,
                                     # Event == "Release" & ReleaseSite == "Chimney Rock Above Island" ~ 8,
                                     # Event == "Release" & ReleaseSite == "Chimney Rock Below Island" ~ 9,
                                     # Event == "Release" & ReleaseSite == "Upper Red Barn Fry Site" ~ 10,
                                     # Event == "Release" & ReleaseSite == "Pool Above Red Barn Antenna" ~ 11,
                                     # Event == "Release" & ReleaseSite == "Lower Red Barn Fry Site" ~ 12,
                                     # Event == "Release" & ReleaseSite == "Below Red Barn Diversion #1" ~ 13,
                                     # Event == "Release" & ReleaseSite == "Below Red Barn Diversion #2" ~ 14,
                                     # Event == "Release" & ReleaseSite == "Kinney Creek" ~ 15,
                                     # Event == "Release" & ReleaseSite == "Dark Timber Above Railroad" ~ 16,
                                     # Event == "Release" & ReleaseSite == "Sheriff Ranch Upper Field" ~ 17,
                                     # Event == "Release" & ReleaseSite == "Shefiff Ranch Middle Field" ~ 18,
                                     # Event == "Release" & ReleaseSite == "Sheriff Ranch Fry Site" ~ 19,
                                     
                                     Event == "Recapture" & RecaptureSite == "Lower River Run" ~ 4,
                                     Event == "Recapture" & RecaptureSite == "Fraser River Ranch" ~ 2,
                                     Event == "Recapture" & RecaptureSite == "Kaibab Park" ~ 1,
                                     Event == "Recapture" & RecaptureSite == "Upper River Run" ~ 3,
                                     Event == "Recapture" & RecaptureSite == "Below Confluence Antenna" ~ 5,
                                     Event == "Recapture" & RecaptureSite == "Windy Gap Dam" ~ 6,
                                     Event == "Recapture" & RecaptureSite == "Hitching Post" ~ 7,
                                     Event == "Recapture" & RecaptureSite == "Chimney Rock Above Island" ~ 8,
                                     Event == "Recapture" & RecaptureSite == "Chimney Rock Below Island" ~ 9,
                                     Event == "Recapture" & RecaptureSite == "Upper Red Barn Fry Site" ~ 10,
                                     Event == "Recapture" & RecaptureSite == "Pool Above Red Barn Antenna" ~ 11,
                                     Event == "Recapture" & RecaptureSite == "Lower Red Barn Fry Site" ~ 12,
                                     Event == "Recapture" & RecaptureSite == "Below Red Barn Diversion #1" ~ 13,
                                     Event == "Recapture" & RecaptureSite == "Below Red Barn Diversion #2" ~ 14,
                                     Event == "Recapture" & RecaptureSite == "Kinney Creek" ~ 15,
                                     Event == "Recapture" & RecaptureSite == "Dark Timber Above Railroad" ~ 16,
                                     Event == "Recapture" & RecaptureSite == "Sheriff Ranch Upper Field" ~ 17,
                                     Event == "Recapture" & RecaptureSite == "Shefiff Ranch Middle Field" ~ 18,
                                     Event == "Recapture" & RecaptureSite == "Sheriff Ranch Fry Site" ~ 19
                                     
      ),
      previous_event_vals = case_when(previous_event == "RB1" ~ 11.9,
                                      previous_event == "RB2" ~ 11.1,
                                      previous_event == "HP3" ~ 7.9,
                                      previous_event == "HP4" ~ 7.1,
                                      previous_event == "CF5" ~ 4.9,
                                      previous_event == "CF6" ~ 4.1,
                                      previous_event == "B3" ~ 6,
                                      previous_event == "B4" ~ .9,
                                      
                                      #using str_detect with "Release" gets both Release events and "recap and release" events
                                      str_detect(previous_event, "Release") & ReleaseSite == "Lower River Run" ~ 4,
                                      str_detect(previous_event, "Release") & ReleaseSite == "Fraser River Ranch" ~ 2,
                                      str_detect(previous_event, "Release") & ReleaseSite == "Kaibab Park" ~ 1,
                                      str_detect(previous_event, "Release") & ReleaseSite == "Upper River Run" ~ 3,
                                      str_detect(previous_event, "Release") & ReleaseSite == "Below Confluence Antenna" ~ 5,
                                      str_detect(previous_event, "Release") & ReleaseSite == "Windy Gap Dam" ~ 6,
                                      str_detect(previous_event, "Release") & ReleaseSite == "Hitching Post" ~ 7,
                                      str_detect(previous_event, "Release") & ReleaseSite == "Chimney Rock Above Island" ~ 8,
                                      str_detect(previous_event, "Release") & ReleaseSite == "Chimney Rock Below Island" ~ 9,
                                      str_detect(previous_event, "Release") & ReleaseSite == "Upper Red Barn Fry Site" ~ 10,
                                      str_detect(previous_event, "Release") & ReleaseSite == "Pool Above Red Barn Antenna" ~ 11,
                                      str_detect(previous_event, "Release") & ReleaseSite == "Lower Red Barn Fry Site" ~ 12,
                                      str_detect(previous_event, "Release") & ReleaseSite == "Below Red Barn Diversion #1" ~ 13,
                                      str_detect(previous_event, "Release") & ReleaseSite == "Below Red Barn Diversion #2" ~ 14,
                                      str_detect(previous_event, "Release") & ReleaseSite == "Kinney Creek" ~ 15,
                                      str_detect(previous_event, "Release") & ReleaseSite == "Dark Timber Above Railroad" ~ 16,
                                      str_detect(previous_event, "Release") & ReleaseSite == "Sheriff Ranch Upper Field" ~ 17,
                                      str_detect(previous_event, "Release") & ReleaseSite == "Shefiff Ranch Middle Field" ~ 18, #will need to be changed once this typo is corrected
                                      str_detect(previous_event, "Release") & ReleaseSite == "Sheriff Ranch Fry Site" ~ 19,
                                      
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Lower River Run" ~ 4,
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Fraser River Ranch" ~ 2,
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Kaibab Park" ~ 1,
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Upper River Run" ~ 3,
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Below Confluence Antenna" ~ 5,
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Windy Gap Dam" ~ 6,
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Hitching Post" ~ 7,
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Chimney Rock Above Island" ~ 8,
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Chimney Rock Below Island" ~ 9,
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Upper Red Barn Fry Site" ~ 10,
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Pool Above Red Barn Antenna" ~ 11,
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Lower Red Barn Fry Site" ~ 12,
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Below Red Barn Diversion #1" ~ 13,
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Below Red Barn Diversion #2" ~ 14,
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Kinney Creek" ~ 15,
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Dark Timber Above Railroad" ~ 16,
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Sheriff Ranch Upper Field" ~ 17,
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Shefiff Ranch Middle Field" ~ 18,
                                      previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Sheriff Ranch Fry Site" ~ 19
                                      
                                      
      ),
      
      #this section is going to disappear because we aren't acknowldgeing detections that 
      # next_event_vals = case_when(next_event == "RB1" ~ 11.9,
      #                                 next_event == "RB2" ~ 11.1,
      #                                 next_event == "HP3" ~ 7.9,
      #                                 next_event == "HP4" ~ 7.1,
      #                                 next_event == "CF5" ~ 4.9,
      #                                 next_event == "CF6" ~ 4.1,
      #                                 next_event == "B3" ~ 6,
      #                                 next_event == "B4" ~ 1,
      #                                 
      #                                 #using str_detect with "Release" gets both Release events and "recap and release" events
      #                                 ### may not need this section since you'd assume that release is never going to happen AFTER detections; 
      #                                 # but there is that time in may 2021 with the mark recapture study where there are (230000) 272140, 272063, 272153, 272273, and 142517 that have detections before official "release"
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Lower River Run" ~ 4,
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Fraser River Ranch" ~ 2,
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Kaibab Park" ~ 1,
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Upper River Run" ~ 3,
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Below Confluence Antenna" ~ 5,
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Windy Gap Dam" ~ 6,
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Hitching Post" ~ 7,
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Chimney Rock Above Island" ~ 8,
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Chimney Rock Below Island" ~ 9,
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Upper Red Barn Fry Site" ~ 10,
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Pool Above Red Barn Antenna" ~ 11,
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Lower Red Barn Fry Site" ~ 12,
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Below Red Barn Diversion #1" ~ 13,
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Below Red Barn Diversion #2" ~ 14,
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Kinney Creek" ~ 15,
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Dark Timber Above Railroad" ~ 16,
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Sheriff Ranch Upper Field" ~ 17,
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Shefiff Ranch Middle Field" ~ 18, #will need to be changed once this typo is corrected
      #                                 str_detect(next_event, "Release") & ReleaseSite == "Sheriff Ranch Fry Site" ~ 19,
      #                                 
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Lower River Run" ~ 4,
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Fraser River Ranch" ~ 2,
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Kaibab Park" ~ 1,
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Upper River Run" ~ 3,
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Below Confluence Antenna" ~ 5,
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Windy Gap Dam" ~ 6,
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Hitching Post" ~ 7,
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Chimney Rock Above Island" ~ 8,
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Chimney Rock Below Island" ~ 9,
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Upper Red Barn Fry Site" ~ 10,
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Pool Above Red Barn Antenna" ~ 11,
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Lower Red Barn Fry Site" ~ 12,
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Below Red Barn Diversion #1" ~ 13,
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Below Red Barn Diversion #2" ~ 14,
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Kinney Creek" ~ 15,
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Dark Timber Above Railroad" ~ 16,
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Sheriff Ranch Upper Field" ~ 17,
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Shefiff Ranch Middle Field" ~ 18,
      #                                 next_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Sheriff Ranch Fry Site" ~ 19
      #                                 
      #                                 
      # ),
    #) #this the break point irght now
      movement = case_when(
        
        Event %in% c("Release", "Recapture and Release") ~ "Initial Release",
        #if the values are more or less than previous values, it's moved upstream or downstream
        #ET_STATION - lag(ET_STATION) > 0 ~ 
        

        #transitions can only happen on one day
        #current station different than previous station, it's a movement
# Downstream Movements and Transitions ------------------------------------

        
(current_event_vals > previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION < lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & (Date == lead(Date, order_by = Datetime)) & (current_event_vals < lead(current_event_vals, order_by = Datetime)) ~ "Downstream Movement Before continuing downstream to transition at that site", 
(current_event_vals > previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION < lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & (Date == lead(Date, order_by = Datetime)) & (current_event_vals > lead(current_event_vals, order_by = Datetime)) & (!Event %in% c("M1", "M2","B3", "B4", "Recapture")) ~ "Downstream Movement and Transition before a Upstream Transition", #means fish missed a antenna 
(current_event_vals > previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION < lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & (current_event_vals < lead(current_event_vals, order_by = Datetime)) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture") ~ "Downstream Movement and Inferred Transition1", #missed a antenna going downstream #228314
(current_event_vals > previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION < lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & current_event_vals == lead(current_event_vals, order_by = Datetime) ~ "Downstream Movement with next detection on same antenna",

(current_event_vals > previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION < lag(ET_STATION, order_by = Datetime)) & (ET_STATION > lead(ET_STATION, order_by = Datetime)) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture") ~ "Downstream Movement and Transition", #means a fish missed a antenna heading downstream
(current_event_vals > previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION < lag(ET_STATION, order_by = Datetime)) & (ET_STATION < lead(ET_STATION, order_by = Datetime)) & !Event %in% c("M1", "M2") ~ "Downstream Movement without Transition", #this is a fish that hits an antenna and heads right back upstream
(current_event_vals > previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & ET_STATION < lag(ET_STATION, order_by = Datetime) & ET_STATION >= lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 0)  & !Event %in% c("B3", "B4", "M1", "M2")~ "Downstream Movement without Transition", #& (current_event_vals == lead(current_event_vals, order_by = Datetime)) #before hitting same antenna
(current_event_vals > previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & ET_STATION < lag(ET_STATION, order_by = Datetime) & ET_STATION >= lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 1)  & !Event %in% c("B3", "B4", "M1", "M2", "Recapture") ~ "Downstream Movement and downstream Transition", #& (current_event_vals == lead(current_event_vals, order_by = Datetime)) # before hitting same antenna

# current_event_vals > previous_event_vals & ET_STATION < lag(ET_STATION, order_by = Datetime) & ET_STATION >= lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 0)  & !Event %in% c("B3", "B4")~ "Downstream Movement without Transition", #& (current_event_vals == lead(current_event_vals, order_by = Datetime)) #before hitting same antenna
# current_event_vals > previous_event_vals & ET_STATION < lag(ET_STATION, order_by = Datetime) & ET_STATION >= lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 1)  & !Event %in% c("B3", "B4") ~ "Downstream Movement and downstream Transition", #& (current_event_vals == lead(current_event_vals, order_by = Datetime)) # before hitting same antenna


(current_event_vals > previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & (Date == lag(Date, order_by = Datetime)) & ET_STATION < lead(ET_STATION, order_by = Datetime) ~ "Downstream Transition then Upstream Transition", 
(current_event_vals >= previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION > lead(ET_STATION, order_by = Datetime) ~ "Downstream Transition Before continuing Downstream", #230000228136 #may need to include that previous detection was on the same day in order to do transition? 
(current_event_vals >= previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION >= lead(ET_STATION, order_by = Datetime) & (Date == lag(Date, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & current_event_vals < lead(current_event_vals, order_by = Datetime) ~ "Downstream Transition (possibly inferred) with next detection possibly at same site", 
(current_event_vals > previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION >= lead(ET_STATION, order_by = Datetime) & (Date == lag(Date, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & current_event_vals == lead(current_event_vals, order_by = Datetime)~ "Downstream Transition with next detection at same antenna", 
(current_event_vals >= previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION >= lead(ET_STATION, order_by = Datetime) & (Date == lag(Date, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & current_event_vals > lead(current_event_vals, order_by = Datetime) ~ "Downstream Transition (possibly inferred)  and inferred upstream transition with next detection possibly at same site", 
(current_event_vals > previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION >= lead(ET_STATION, order_by = Datetime)  & (Date == lag(Date, order_by = Datetime)) & (Date == lead(Date, order_by = Datetime)) ~ "Downstream Transition (possibly inferred) with next detection possibly at same site1", # if this line were to leave and these would be coded as NA, it might be beneficial


(current_event_vals < previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (Date != lag(Date, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & !Event %in% c("B3", "B4", "M1", "M2")  ~ "Inferred Downstream Transition with next detection possibly at same site", 


(Event %in% c("M1", "M2",  "B3", "B4","Recapture")) & (ET_STATION < lag(ET_STATION, order_by = Datetime))  ~ "Downstream Movement1",  #`movement = case_when(...)`. x object not interpretable as a factor# solved because I was typing uppercase C for a concatenation of strings, not c
(is.na(current_event_vals) | is.na(previous_event_vals)) & ET_STATION < lag(ET_STATION, order_by = Datetime) &  ET_STATION == lead(ET_STATION, order_by = Datetime) & current_event_vals == lead(current_event_vals, order_by = Datetime) & (str_detect(Event, "Release") == FALSE) ~ "Downstream Movement2 without Transition", #"Downstream Movement2",
(is.na(current_event_vals) | is.na(previous_event_vals)) & ET_STATION < lag(ET_STATION, order_by = Datetime) & ET_STATION == lead(ET_STATION, order_by = Datetime) & current_event_vals > lead(current_event_vals, order_by = Datetime) & (str_detect(Event, "Release") == FALSE) ~ "Downstream Movement2 and Transition", #skipped an antenna #230000228623
(is.na(current_event_vals) | is.na(previous_event_vals)) & ET_STATION < lag(ET_STATION, order_by = Datetime) & ET_STATION == lead(ET_STATION, order_by = Datetime) & current_event_vals < lead(current_event_vals, order_by = Datetime) & (str_detect(Event, "Release") == FALSE) ~ "Downstream Movement2 before potential Transition", #skipped an antenna #230000228623

(current_event_vals > previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION < lag(ET_STATION, order_by = Datetime)) & is.na(lead(Event, order_by = Datetime)) ~ "Downstream Movement and last detection of history",
(current_event_vals > previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & (Date == lag(Date, order_by = Datetime)) & is.na(lead(Event, order_by = Datetime)) & !Event %in% c("M1", "M2") ~ "Downstream Transition and last detection of history",

#fish must have also transitioned if the next time it's seen it's seen on the next-most ds antenna
#if a fish is headed downstream and only hits one antenna but then is detected downstream, it must have transitioned as well as moved
#if the station values are the same though, it's a transition, not a movement

# Upstream Movements and Transitions --------------------------------------
(current_event_vals < previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION > lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & (Date == lead(Date, order_by = Datetime)) & (current_event_vals > lead(current_event_vals, order_by = Datetime)) ~ "Upstream Movement Before continuing Upstream to transition at that site", 
(current_event_vals < previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION > lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & (Date == lead(Date, order_by = Datetime)) & (current_event_vals < lead(current_event_vals, order_by = Datetime)) & !Event %in% c("M1", "M2","B3", "B4", "Recapture") ~ "Upstream Movement and Transition111", #means fish missed a antenna #before a Upstream Transition
(current_event_vals < previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION > lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & (current_event_vals > lead(current_event_vals, order_by = Datetime)) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture") ~ "Upstream Movement and Inferred Transition1", #missed a antenna going Upstream #228314
(current_event_vals < previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION > lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & current_event_vals == lead(current_event_vals, order_by = Datetime) ~ "Upstream Movement with next detection on same antenna",

(current_event_vals < previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION > lag(ET_STATION, order_by = Datetime)) & (ET_STATION < lead(ET_STATION, order_by = Datetime)) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture") ~ "Upstream Movement and Transition", #means a fish missed a antenna heading Upstream
(current_event_vals < previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION > lag(ET_STATION, order_by = Datetime)) & (ET_STATION > lead(ET_STATION, order_by = Datetime)) & !Event %in% c("M1", "M2") ~ "Upstream Movement without Transition", #this is a fish that hits an antenna and heads right back upstream
(current_event_vals < previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & ET_STATION > lag(ET_STATION, order_by = Datetime) & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 0)  & !Event %in% c("B3", "B4", "M1", "M2") ~ "Upstream Movement without Transition", #& (current_event_vals == lead(current_event_vals, order_by = Datetime)) #before hitting same antenna
(current_event_vals < previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & ET_STATION > lag(ET_STATION, order_by = Datetime) & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 1)  & !Event %in% c("B3", "B4", "M1", "M2", "Recapture") ~ "Upstream Movement and Upstream Transition", #& (current_event_vals == lead(current_event_vals, order_by = Datetime)) # before hitting same antenna

(current_event_vals < previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture") & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & (Date == lag(Date, order_by = Datetime)) & ET_STATION > lead(ET_STATION, order_by = Datetime)  ~ "Upstream Transition then Downstream Transition", 
(current_event_vals <= previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION < lead(ET_STATION, order_by = Datetime)  ~ "Upstream Transition Before continuing Upstream", #230000228136
(current_event_vals <= previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (Date == lag(Date, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & current_event_vals > lead(current_event_vals, order_by = Datetime) ~ "Upstream Transition (possibly inferred) with next detection possibly at same site", 
(current_event_vals < previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (Date == lag(Date, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & current_event_vals == lead(current_event_vals, order_by = Datetime) ~ "Upstream Transition with next detection at same antenna", 
(current_event_vals <= previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (Date == lag(Date, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & current_event_vals < lead(current_event_vals, order_by = Datetime) ~ "Upstream Transition (possibly inferred)  and inferred downstream transition with next detection possibly at same site", 
(current_event_vals < previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & !Event %in% c("M1", "M2", "B3", "B4", "Recapture")  & (ET_STATION == lag(ET_STATION, order_by = Datetime))  & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (Date == lag(Date, order_by = Datetime)) & (Date == lead(Date, order_by = Datetime)) ~ "Upstream Transition (possibly inferred) with next detection possibly at same site1", 


#upstream inferred transition for fish 
(current_event_vals > previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION <= lead(ET_STATION, order_by = Datetime) & (Date != lag(Date, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & !Event %in% c("B3", "B4", "M1", "M2") ~ "Inferred Upstream Transition with next detection possibly at same site", #230000228862



(Event %in% c("M1", "M2", "B3", "B4", "Recapture")) & (ET_STATION > lag(ET_STATION, order_by = Datetime))  ~ "Upstream Movement1",  #`movement = case_when(...)`. x object not interpretable as a factor# solved because I was typing uppercase C for a concatenation of strings, not c
(is.na(current_event_vals) | is.na(previous_event_vals)) & ET_STATION > lag(ET_STATION, order_by = Datetime) &  ET_STATION == lead(ET_STATION, order_by = Datetime) & current_event_vals == lead(current_event_vals, order_by = Datetime) & (str_detect(Event, "Release") == FALSE) ~ "Upstream Movement2 without Transition", #"Upstream Movement2",
(is.na(current_event_vals) | is.na(previous_event_vals)) & ET_STATION > lag(ET_STATION, order_by = Datetime) & ET_STATION == lead(ET_STATION, order_by = Datetime) & current_event_vals < lead(current_event_vals, order_by = Datetime) & (str_detect(Event, "Release") == FALSE) ~ "Upstream Movement2 and Transition", #skipped an antenna #230000228623
(is.na(current_event_vals) | is.na(previous_event_vals)) & ET_STATION > lag(ET_STATION, order_by = Datetime) & ET_STATION == lead(ET_STATION, order_by = Datetime) & current_event_vals > lead(current_event_vals, order_by = Datetime) & (str_detect(Event, "Release") == FALSE) ~ "Upstream Movement2 before potential Transition", #skipped an antenna #230000228623

#(is.na(current_event_vals) | is.na(previous_event_vals)) & ET_STATION < lag(ET_STATION, order_by = Datetime) & ET_STATION == lead(ET_STATION, order_by = Datetime) & current_event_vals < lead(current_event_vals, order_by = Datetime) & (str_detect(Event, "Release") == FALSE) ~ "Upstream Movement2 before Transition", #skipped an antenna #230000228623

(current_event_vals < previous_event_vals| (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION > lag(ET_STATION, order_by = Datetime)) & is.na(lead(Event, order_by = Datetime)) ~ "Upstream Movement and last detection of history",
(current_event_vals < previous_event_vals| (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & (Date == lag(Date, order_by = Datetime)) & is.na(lead(Event, order_by = Datetime)) & !Event %in% c("M1", "M2") ~ "Upstream Transition and last detection of history",

# (current_event_vals < previous_event_vals) & (ET_STATION != lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & (Date == lead(Date, order_by = Datetime)) & (current_event_vals > lead(current_event_vals, order_by = Datetime)) ~ "Upstream Movement before continuing upstream", 
# (current_event_vals < previous_event_vals) & (ET_STATION != lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & (Date == lead(Date, order_by = Datetime)) & (current_event_vals < lead(current_event_vals, order_by = Datetime)) ~ "Upstream Movement and Transition before a Downstream Transition", #means fish missed a antenna 
# (current_event_vals < previous_event_vals) & (ET_STATION != lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & (current_event_vals > lead(current_event_vals, order_by = Datetime)) ~ "Upstream Movement and Transition1", 
# #if a fish is seen downstream, then heads upstream because the next time it's seen is a downstream transition, it must have had a upstream transition before that
# (current_event_vals < previous_event_vals) & (ET_STATION != lag(ET_STATION, order_by = Datetime)) & (ET_STATION > lead(ET_STATION, order_by = Datetime)) ~ "Upstream Movement without Transition",        
# (current_event_vals < previous_event_vals) & (ET_STATION != lag(ET_STATION, order_by = Datetime)) & (ET_STATION < lead(ET_STATION, order_by = Datetime)) ~ "Upstream Movement and Transition",                                                                                                                                                                                                                 
# (current_event_vals < previous_event_vals) & (ET_STATION != lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & current_event_vals == lead(current_event_vals, order_by = Datetime) ~ "Upstream Movement with next detection on same antenna",        
# #fish must have also transitioned if the next time it's seen it's seen on the next-most ds antenna
# (current_event_vals < previous_event_vals) & (ET_STATION != lag(ET_STATION, order_by = Datetime)) & is.na(lead(Event, order_by = Datetime)) ~ "Upstream Movement and Last detection of history", #  must be a logical vector, not a `formula` object. FIXED because there wasn't a comma at the end of the expression
# current_event_vals > lead(current_event_vals, order_by = Datetime) & daily_unique_events == 1 & ET_STATION < lead(ET_STATION) ~ "Upstream Transition missing 1 antenna", 
# (current_event_vals < previous_event_vals) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & (Date == lag(Date, order_by = Datetime)) & ET_STATION > lead(ET_STATION, order_by = Datetime) ~ "Upstream Transition then downstream transition",
# 
# (current_event_vals < previous_event_vals) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & ET_STATION < lead(ET_STATION, order_by = Datetime) ~ "Upstream Transition before continuing upstream",
# 
# (current_event_vals < previous_event_vals) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & (Date == lag(Date, order_by = Datetime)) & ET_STATION == lead(ET_STATION, order_by = Datetime) & current_event_vals == lead(current_event_vals, order_by = Datetime) ~ "Upstream Transition with next detection at same antenna", #error problem with vec_compare bc I didn't have a end parantheses with the lead() fucniton
# 
# (current_event_vals < previous_event_vals) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & (Date == lag(Date, order_by = Datetime)) & ET_STATION == lead(ET_STATION, order_by = Datetime) & current_event_vals < lead(current_event_vals, order_by = Datetime) & current_event_vals < lead(current_event_vals, n = 2, order_by = Datetime) ~ "Upstream then downstream transition11 with next detection at same site",
# 
# #last detections of the history
# (current_event_vals < previous_event_vals) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & (Date == lag(Date, order_by = Datetime)) & is.na(lead(Event, order_by = Datetime)) ~ "Upstream Transition and last detection of history",
# 
# 
# current_event_vals < previous_event_vals & ET_STATION > lag(ET_STATION, order_by = Datetime) & ET_STATION == lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 0) & (current_event_vals == lead(current_event_vals, order_by = Datetime)) & !Event %in% c("B3", "B4") ~ "Upstream Movement and Transition before hitting same antenna",
# current_event_vals < previous_event_vals & ET_STATION > lag(ET_STATION, order_by = Datetime) & ET_STATION == lead(ET_STATION, order_by = Datetime) & (as.numeric(str_extract(Event, "[:digit:]")) %% 2 == 1) & (current_event_vals == lead(current_event_vals, order_by = Datetime)) & !Event %in% c("B3", "B4") ~ "Upstream Movement without Transition before hitting same antenna",
# 
# Event %in% c("M1", "M2") & (ET_STATION > lag(ET_STATION, order_by = Datetime)) ~ "Upstream Movement1",
# (is.na(current_event_vals) | is.na(previous_event_vals)) & ET_STATION > lag(ET_STATION, order_by = Datetime) & (str_detect(Event, "Release") == FALSE) ~ "Upstream Movement2", 

#if a fish swims downstream, and hits only one antenna at a site before hitting the antenna again, it's either a movement with transition or without depending on if antenna has even or odd numbers in it

# Recap -------------------------------------------------------------------



# Not Enough Info or No Movement ---------------------------------------------------------

(current_event_vals != previous_event_vals) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & (Date != lag(Date, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime))  ~ "Not Enough Info to infer Movement", #might have to include that next station is the same as previous? 
(current_event_vals != previous_event_vals) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & (Date != lag(Date, order_by = Datetime)) & is.na(lead(Event, order_by = Datetime))  ~ "Not Enough Info to infer Movement and last detection of history", #might have to include that next station is the same as previous? 

(current_event_vals == previous_event_vals | (is.na(current_event_vals) | is.na(previous_event_vals))) & (ET_STATION == lag(ET_STATION, order_by = Datetime)) & is.na(lead(Event, order_by = Datetime)) ~ "No Movement and last detection of history",

#if a event is mobile , decide whether it's a upstream or downstream movement based on ET station

Event %in% c("M1", "M2") & (ET_STATION == lag(ET_STATION, order_by = Datetime)) ~ "No Movement",
#if current event vals is na or previous event vals are NA
(is.na(current_event_vals) | is.na(previous_event_vals)) & is.na(ET_STATION) ~ "Not Enough Info to infer movement1",
# #if the values are the same and the day is the same, it means there was multiple consecutive detections at the same antenna and same day 
#current_event_vals == previous_event_vals & (Date == lag(Date, order_by = Datetime)) & ET_STATION == lead(ET_STATION, order_by = Datetime) ~ "No Movement; Same Day", #current_event_vals == lead(current_event_vals, order_by = Datetime)
current_event_vals == previous_event_vals & (Date == lag(Date, order_by = Datetime)) & (Date == lead(Date, order_by = Datetime)) & ET_STATION == lead(ET_STATION, order_by = Datetime) & abs(current_event_vals - lead(current_event_vals, order_by = Datetime)) < 1 & Event %in% c("RB1", "RB2", "HP3", "HP4", "CF5", "CF6")  ~ "No Movement; Same Day for stationary antennas", #current_event_vals == lead(current_event_vals, order_by = Datetime)
current_event_vals == previous_event_vals & (Date == lag(Date, order_by = Datetime)) & (Date != lead(Date, order_by = Datetime)) & ET_STATION == lead(ET_STATION, order_by = Datetime) & (current_event_vals == lead(current_event_vals, order_by = Datetime) | is.na(lead(current_event_vals, order_by = Datetime)))  & Event %in% c("RB1", "RB2", "HP3", "HP4", "CF5", "CF6")  ~ "No Movement; Same Day for stationary antennas1", #current_event_vals == lead(current_event_vals, order_by = Datetime) #230000292262

current_event_vals == previous_event_vals & (Date == lag(Date, order_by = Datetime)) & !Event %in% c("RB1", "RB2", "HP3", "HP4", "CF5", "CF6")  ~ "No Movement; Same Day for non-stationary antennas", #current_event_vals == lead(current_event_vals, order_by = Datetime)

current_event_vals == previous_event_vals & (Date != lag(Date, order_by = Datetime)) ~ "No Movement1",         
#(current_event_vals > previous_event_vals) & (ET_STATION != lag(ET_STATION, order_by = Datetime)) & (ET_STATION == lead(ET_STATION, order_by = Datetime)) & current_event_vals lead(current_event_vals, order_by = Datetime) ~ "Downstream Movement and Downstream Transition ",

        
        
                                                                                                    
        #if a fish hits only 1 antenna but then turns around, it's a movement, not a transition 
        
        
        
      
         
        
        
        

        #last detections of the history
        
        
        
         ##
        
  
        # 
        # #if the vals are the same but the day is different, and the previous event was a stagnant state (B3, b4, release, etc) then there was no movement and the fish is in the same state it was
        # current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & previous_event %in% c("B3", "B4", "Release", "Recapture", "Recapture and Release") ~ "No Movement",
        # 
        # #if a fish ended the previous detection upstream of an antenna, then hits the same antenna again AND hits a different event that day (probably the other antenna detection from that site), it's a downstream movement
        # current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections > 1) & (daily_unique_events > 1) & previous_event %in% c("CF6", "HP4", "RB2") ~ "Downstream Movement1",
        # #if a fish ended the previous detection downstream of an antenna, then hits the same antenna again AND has a different event that day (probably the other antenna detection from that site), it's a upstream movement
        # current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections > 1) & (daily_unique_events > 1) & previous_event %in% c("CF5", "HP3", "RB1") ~ "Upstream Movement1",
        # 
        # 
        # 
        # #this says that if a fish ended the previous detection upstream of a antenna, then hits the same antenna again multiple times without hitting the next downstream antenna, it's assumed to have then ended back upstream
        # current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections > 1) & (daily_unique_events == 1) & previous_event %in% c("CF6", "HP4", "RB2") ~ paste("Not Enough Info to infer movement-", Event, "only detection"), #Down then back up move
        # #this says that if a fish ended the previous detection downstream of a antenna, then hits the same antenna again multiple times without hitting the next upstream antenna, it's assumed to have then ended back upstream
        # 
        # current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections > 1) & (daily_unique_events == 1) & previous_event %in% c("CF5", "HP3", "RB1") ~ paste("Not Enough Info to infer movement-", Event, "only detection"), # Up then back down move
        # #this says that if the fish came from downstream and hit only one antenna one time, it's assumed to have continued upstream and just missed hitting the upstream antenna
        # current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections == 1) & (daily_unique_events == 1) & previous_event %in% c("CF5", "HP3", "RB1") ~ paste("Not Enough Info to infer movement-", Event, "only detection"), #Upstream Movement without hitting both antennas
        # #this says that if the fish came from upstream and hit only one antenna one time, it's assumed to have continued downstream and just missed hitting the other downstream antenna
        # current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections == 1) & (daily_unique_events == 1) & previous_event %in% c("CF6", "HP4", "RB2") ~ paste("Not Enough Info to infer movement-", Event, "only detection")  #Downstream Movement without hitting both antennas
        # 
      ), #end of movement case_when
     ###
      
    
     #x11 = paste(lead(Event, order_by = Datetime), "is the thing"),

      teststate_11 = case_when(
                               str_detect(movement, "Not Enough Info to infer movement") ~ "NEI",
        
                               movement %in% c("Downstream Movement", "Downstream Movement1","Downstream Movement without hitting both antennas")  & (Event %in% c("CF5", "CF6")) ~ "L",
                               movement %in% c("Upstream Movement", "Upstream Movement1", "Upstream Movement without hitting both antennas")  & (Event %in% c("CF5", "CF6")) ~ "K",
                               movement %in% c("Downstream Movement", "Downstream Movement1","Downstream Movement without hitting both antennas") & (Event %in% c("HP3", "HP4")) ~ "J",
                               movement %in% c("Upstream Movement", "Upstream Movement1", "Upstream Movement without hitting both antennas") & (Event %in% c("HP3", "HP4")) ~ "I",
                               movement %in% c("Downstream Movement", "Downstream Movement1","Downstream Movement without hitting both antennas") & (Event %in% c("RB1", "RB2")) ~ "H",
                               movement %in% c("Upstream Movement", "Upstream Movement1", "Upstream Movement without hitting both antennas") & (Event %in% c("RB1", "RB2")) ~ "G",
                               movement == "Up then back down move" & (Event %in% c("HP3")) ~ "IJ",
                               movement == "Up then back down move" & (Event %in% c("RB1")) ~ "GH",
                               movement == "Up then back down move" & (Event %in% c("CF5")) ~ "KL",
                               movement == "Down then back up move" & (Event %in% c("HP4")) ~ "JI",
                               movement == "Down then back up move" & (Event %in% c("RB2")) ~ "HG",
                               movement == "Down then back up move" & (Event %in% c("CF6")) ~ "LK",
                               
                               Event == "B3" ~ "C",
                               Event == "B4" ~ "F",
                               str_detect(Event, "Release") & ReleaseSite == "Lower River Run" ~ "E",
                               str_detect(Event, "Release") & ReleaseSite == "Fraser River Ranch" ~ "F",
                               str_detect(Event, "Release") & ReleaseSite == "Kaibab Park" ~ "F",
                               str_detect(Event, "Release") & ReleaseSite == "Upper River Run" ~ "E",
                               str_detect(Event, "Release") & ReleaseSite == "Below Confluence Antenna" ~ "D",
                               str_detect(Event, "Release") & ReleaseSite == "Windy Gap Dam" ~ "C",
                               str_detect(Event, "Release") & ReleaseSite == "Hitching Post"~ "C",
                               str_detect(Event, "Release") & ReleaseSite == "Chimney Rock Above Island" ~ "B",
                               str_detect(Event, "Release") & ReleaseSite == "Chimney Rock Below Island" ~ "B",
                               str_detect(Event, "Release") & ReleaseSite == "Upper Red Barn Fry Site" ~ "B",
                               str_detect(Event, "Release") & ReleaseSite == "Pool Above Red Barn Antenna" ~ "B",
                               str_detect(Event, "Release") & ReleaseSite == "Lower Red Barn Fry Site" ~ "A",
                               str_detect(Event, "Release") & ReleaseSite == "Below Red Barn Diversion #1" ~ "A",
                               str_detect(Event, "Release") & ReleaseSite == "Below Red Barn Diversion #2" ~ "A",
                               str_detect(Event, "Release") & ReleaseSite == "Kinney Creek" ~ "A",
                               str_detect(Event, "Release") & ReleaseSite == "Dark Timber Above Railroad" ~ "A",
                               str_detect(Event, "Release") & ReleaseSite == "Sheriff Ranch Upper Field" ~ "A",
                               str_detect(Event, "Release") & ReleaseSite == "Shefiff Ranch Middle Field" ~ "A", #will need to be changed once this typo is corrected
                               str_detect(Event, "Release") & ReleaseSite == "Sheriff Ranch Fry Site" ~ "A",
                               
                               #recapture state
                               RecaptureSite == "Lower River Run" ~ "E",
                               RecaptureSite == "Fraser River Ranch" ~ "F",
                               RecaptureSite == "Kaibab Park" ~ "F",
                               RecaptureSite == "Upper River Run" ~ "E",
                               RecaptureSite == "Below Confluence Antenna" ~ "D",
                               RecaptureSite == "Windy Gap Dam" ~ "C",
                               RecaptureSite == "Hitching Post"~ "C",
                               RecaptureSite == "Chimney Rock Above Island" ~ "B",
                               RecaptureSite == "Chimney Rock Below Island" ~ "B",
                               RecaptureSite == "Upper Red Barn Fry Site" ~ "B",
                               RecaptureSite == "Pool Above Red Barn Antenna" ~ "B",
                               RecaptureSite == "Lower Red Barn Fry Site" ~ "A",
                               RecaptureSite == "Below Red Barn Diversion #1" ~ "A",
                               RecaptureSite == "Below Red Barn Diversion #2" ~ "A",
                               RecaptureSite == "Kinney Creek" ~ "A",
                               RecaptureSite == "Dark Timber Above Railroad" ~ "A",
                               RecaptureSite == "Sheriff Ranch Upper Field" ~ "A",
                               RecaptureSite == "Shefiff Ranch Middle Field" ~ "A", #will need to be changed once this typo is corrected
                               RecaptureSite == "Sheriff Ranch Fry Site" ~ "A",
      ), #end of case_when
      
      
    sum_dist_miles = (sum(abs(diff(ET_STATION)))) * 0.000621371,
    ) #%>% #end of mutate
    #select(Date, Datetime, first_last, Event, movement,  teststate_11, c_number_of_detections, daily_unique_events, ReleaseSite, RecaptureSite, TAG)
  # x <- r1 %>%
  #   filter(!is.na(previous_event) & !Event %in% c("Release", "Recapture and Release" ))
  unknown_movements <- r1 %>%
    filter(
      #!ReleaseSite %in% c("Pool Above Red Barn Antenna"),
      str_detect(TAG, c("^230")) | str_detect(TAG, c("^226")),
           #!is.na(previous_event), #don't want entries 
      is.na(movement)
      )
  
  r11 <- r1 %>%
    select(Date, Datetime, TAG, Event, movement,ET_STATION,  ReleaseSite, Release_Date, RecaptureSite, sum_dist_miles)
  
  r2 <- r1 %>%
    filter(!is.na(teststate_11)) %>%
    group_by(Date, TAG) %>%
    #arranging my datetime ensures that all states will be recorded in the correct order
    arrange(Datetime) %>%
    mutate(
      #tesst = str_detect(teststate_1, "[:alpha:]"))
      
      teststate_2 = case_when(min(Datetime) == max(Datetime) ~ teststate_11,
                              min(Datetime) != max(Datetime) & (daily_unique_events <= 2) ~ (paste0(teststate_11[which(Datetime == min(Datetime))],
                                                                                   #                                                                                                                               #unique(teststate_1), 
                                                                                   teststate_11[which(Datetime == max(Datetime))])),
                              #if there are many events, need to get all of them. Collapse aregument converts vector to string
                              min(Datetime) != max(Datetime) & (daily_unique_events > 2) ~ paste(teststate_11, collapse = "")
      ), #end of case_when
    )  %>%
    mutate(teststate_4 = gsub('([[:alpha:]])\\1+', '\\1', teststate_2)) %>% #removes consecutive letters
    distinct(Date, TAG, teststate_4, .keep_all = TRUE) %>%
    select(Date, Datetime, TAG, teststate_4, ReleaseSite, Species, Release_Length, Release_Weight, movement, c_number_of_detections, daily_unique_events, days_since) %>%
    rename(State = teststate_4)
  
  
          
  #testState4 is really the realState
  # r3 <- r2 %>%
  #   # mutate(New_movt = case_when(State %in% c("GH", "HG","LK","KL", "IJ","JI") ~ "No Net Movement",
  #   #                             State %in% c("K","I","G")~ "Upstream Movement",
  #   #                             State %in% c("L","J", "H") ~ "Downstream Movement",
  #   #                             State == lag(State, order_by = Datetime) ~ "No Net Movement",
  #   #                             State != lag(State, order_by = Datetime) & (State > lag(State, order_by = Datetime)) ~ "Upstream Movement",
  #   #                             State != lag(State, order_by = Datetime) & (State < lag(State, order_by = Datetime)) ~ "Downstream Movement")
  #   #                     )
  #   
  #   
  #   select(Date, Datetime,TAG,Event,State, movement, ReleaseSite,RecaptureSite, days_since, first_last, previous_event)
  

# Pivot_wider -------------------------------------------------------------

  days <- data.frame(days_since = 1:max(r2$days_since))
  
  days_and_states <- full_join(days, r2, by = "days_since")
  
  
  days_and_states_wide <- pivot_wider(days_and_states, id_cols = TAG, names_from = days_since, values_from = State)
  
  states_df_list <- list("Movements" = r1, "All_States" = r2, "Days_and_states_wide" = days_and_states_wide)
  #this just tells how long the fucntion takes
  end_time <- Sys.time()
  print(paste("States Function took", round(end_time-start_time,2)))
  
  return(states_df_list)
}


#statesdf_list <- Get_states_function(All_events, station_data)
# statesdf <- statesdf_list$All_States
#####################

# u4 <- u3 %>%
#   # filter(TAG == "230000272254") %>%
#   # mutate(x = (str_detect(previous_event, "Release")))
#   filter(is.na(movement),
#           !Event %in% c("Release", "Recapture and Release"),
#           !ReleaseSite %in% c("No Info"))#
# 
# Release1 <- Release %>%
#   rename(TAG = TagID)
# x <- anti_join(Release1, u4, by = "TAG")
# 
# statesdf1 <- statesdf %>%
#   select(Date, Datetime, TAG, teststate_4, Event)
# 
# states1 <- data.frame(unique(u3$teststate_4))
# #test_days <- pivot_wider(data = All_events_days1, id_cols = TAG, names_from = days_since, values_from = Event)
# t1 <- All_events %>%
#   filter(TAG == "230000228714")
# single_tag <- All_events_days1 %>%
#   select(Date, Datetime,TAG,Event,movement,ReleaseSite,RecaptureSite, days_since, first_last, previous_event) %>%
#   filter(TAG %in% "230000224371") 
# 
# 
# #which(single_tag$first_last == "First_of_day")
# #single_tag_with_release <- single_tag
# #incomplete list for now
# # values_list = list(
# #   "Fraser River Ranch" = 2, "Lower River Run" = 4, "CF6" = 4.1, "CF5" = 4.9, "Below Confluence Antenna" = 5, 
# #   "Pool Above Red Barn Antenna" = 11, "RB2" = 11.1, "RB1" = 11.9,
# #   "Hitching Post" = 7, "HP4" = 7.1, "HP3" = 7.9,
# #   "Windy Gap Dam" = 6, "B3" = 6,
# #   "B4" = .1,
# #   "Release" = 0
# # )
# 
# t1 <- Release %>%
#   distinct(RS_Num, .keep_all = TRUE)
# #if count(Date) has entries >1: 
# r1 <- single_tag %>%
#   #mutate(previous_event = lead(Event)) %>%
#   group_by(Date) %>% #will also want to group_by Tag for all fish
#   
#   ungroup() %>%
#     
#     mutate(
#       current_event_vals = case_when(Event == "RB1" ~ 11.9,
#                                      Event == "RB2" ~ 11.1,
#                                      Event == "HP3" ~ 7.9,
#                                      Event == "HP4" ~ 7.1,
#                                      Event == "CF5" ~ 4.9,
#                                      Event == "CF6" ~ 4.1,
#                                      Event == "B3" ~ 6
#       ),
#       previous_event_vals = case_when(previous_event == "RB1" ~ 11.9,
#                                       previous_event == "RB2" ~ 11.1,
#                                       previous_event == "HP3" ~ 7.9,
#                                       previous_event == "HP4" ~ 7.1,
#                                       previous_event == "CF5" ~ 4.9,
#                                       previous_event == "CF6" ~ 4.1,
#                                       previous_event == "B3" ~ 6,
#                                       previous_event == "Release" & ReleaseSite == "Lower River Run" ~ 4,
#                                       previous_event == "Release" & ReleaseSite == "Fraser River Ranch" ~ 2),
#       
#       # this is for quantiying general upstream/downstream movement
#       movement = case_when((current_event_vals > previous_event_vals) ~ "Downstream Movement",
#                            (current_event_vals < previous_event_vals) ~ "Upstream Movement",
#                            current_event_vals == previous_event_vals & (Date == lag(Date, order_by = Datetime)) ~ "No Movement",
#                            current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & previous_event %in% c("CF6", "HP4", "RB2") ~ "Downstream Movement",
#                            current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & previous_event %in% c("CF5", "HP3", "RB1") ~ "Upstream Movement"),
#       teststate_1 = case_when(movement == "Downstream Movement" & (Event %in% c("CF5", "CF6")) ~ "L",
#                               movement == "Upstream Movement" & (Event %in% c("CF5", "CF6")) ~ "K",
#                               movement == "Downstream Movement" & (Event %in% c("HP3", "HP4")) ~ "J",
#                               movement == "Upstream Movement" & (Event %in% c("HP3", "HP4")) ~ "I",
#                               movement == "Downstream Movement" & (Event %in% c("RB1", "RB2")) ~ "H",
#                               movement == "Upstream Movement" & (Event %in% c("RB1", "RB2")) ~ "G",
#                               Event == "B3" ~ "C")
#       
#       # test12 = values_list[[Event]] 
#       # US_DS = case_when(
#       #   #previous_event == "Release" & (as.numeric(values_list[ReleaseSite] < as.numeric(values_list[Event])))[1] ~ "DS",
#       #   (as.numeric(values_list[[as.character(Event)]]) > as.numeric(values_list[[as.character(previous_event)]]))[1] ~ "DS",
#       #   # as.numeric(values_list[Event] < as.numeric(values_list[previous_event])) ~ "US"
#       # )    
#     ) #
# 
# r2 <- r1 %>%
#   filter(!is.na(teststate_1)) %>%
#   group_by(Date) %>%
#   mutate(
#     #tesst = str_detect(teststate_1, "[:alpha:]"))
#     
#     teststate_2 = paste0(teststate_1[which(Datetime == min(Datetime))],
#                          #                                                                                                                               #unique(teststate_1), 
#                          teststate_1[which(Datetime == max(Datetime))]),
#     teststate_3 = str_c(teststate_1, collapse = NULL )
#     
#     # case_when(teststate_1[which(Datetime == min(Datetime))] != teststate_1[which(Datetime == max(Datetime))] ~ paste0(teststate_1[which(Datetime == min(Datetime))],
#     #                                                                                                                               #unique(teststate_1), 
#     #                                                                                                                               teststate_1[which(Datetime == max(Datetime))]),
#     #                       teststate_1[which(Datetime == min(Datetime))] == teststate_1[which(Datetime == max(Datetime))] ~ teststate_1)
#     
#   )  %>%
#   distinct(Date, TAG, teststate_2, .keep_all = TRUE) %>%
#   mutate(teststate_4 = gsub('([[:alpha:]])\\1+', '\\1', teststate_2)) #removes consecutive letters
# 
# # to do:
# # be able to capture all unique states in a day, not just the first and last ones of the day
# # apply to all tags
# # fill out full list of values and case_when for recaps and all release sites
# # see if I can corporate release event and state


#if the values are more or less than previous values, it's moved upstream or downstream
# (current_event_vals > previous_event_vals) ~ "Downstream Movement",
# (current_event_vals < previous_event_vals) ~ "Upstream Movement",
# 
# #if the values are the same and the day is the same, it means there was multiple consecutive detections at the same antenna and same day 
# current_event_vals == previous_event_vals & (Date == lag(Date, order_by = Datetime)) ~ "No Movement; Same Day",
# 
# #if the vals are the same but the day is different, and the previous event was a stagnant physical state (B3, b4, release, etc) then there was no movement and the fish is in the same state it was
# current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & previous_event %in% c("B3", "B4", "Release", "Recapture", "Recapture and Release") ~ "No Movement",
# 
# 
# 
# #if a fish ended the previous detection upstream of an antenna, then hits the same antenna again AND hits a different event that day (probably the other antenna detection from that site), it's a downstream movement
# current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (daily_unique_events > 1) & previous_event %in% c("CF6", "HP4", "RB2") ~ "Downstream Movement1", # & (c_number_of_detections > 1)
# 
# #if a fish ended the previous detection downstream of an antenna, then hits the same antenna again AND has a different event that day (probably the other antenna detection from that site), it's a upstream movement
# current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime)  & (daily_unique_events > 1) & previous_event %in% c("CF5", "HP3", "RB1") ~ "Upstream Movement1", #& (c_number_of_detections > 1)
# 
# ## if same thing but same_day next events == FALSE then there is not enough info 
# # current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (daily_unique_events == 1) & (same_day_next_events == FALSE) ~ "Not Enough Info",
# # 
# # # if a fish's previous values and curent event values are the same and occur on a different date, and only hits one antenna that day
# # # then next day it is detected multiple times (same_day_next_events == TRUE) the fish next makes a downstream movement (next_event_2_vals > next_event_vals),
# # # then the current event is actually an upstream movement
# # 
# # current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (daily_unique_events == 1) & (same_day_next_events == TRUE) & (next_event_2_vals > next_event_vals) ~ "Upstream Movement11",
# # 
# # 
# # # if a fish's previous values and curent event values are the same and occur on a different date, and only hits one antenna that day
# # # then next day it is detected multiple times (same_day_next_events == TRUE) the fish next makes a upstream movement (next_event_2_vals < next_event_vals),
# # # then the current event is actually an downstream movement  
# # 
# # current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (daily_unique_events == 1) & (same_day_next_events == TRUE) & (next_event_2_vals < next_event_vals) ~ "Downstream Movement11",
# # 
# # 
# # 
# 
# #this says that if a fish ended the previous detection upstream of a antenna, then hits the same antenna again multiple times without hitting the next downstream antenna, 
# #but the next 2 events are a upstream move at that antenna, there isn't enough info to assume movement 
# current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections > 1) & (daily_unique_events == 1) & previous_event %in% c("CF6", "HP4", "RB2") & ~ "Not Enough Info", #down then back up move
# 
# #this says that if a fish ended the previous detection downstream of a antenna, then hits the same antenna again multiple times without hitting the next upstream antenna, there isn't enough info to assume movement 
# current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections > 1) & (daily_unique_events == 1) & previous_event %in% c("CF5", "HP3", "RB1") ~ "Not Enough Info", #Up then back down move
# 
# #this says that if the fish ended the previous detection downstream of a antenna and hit the same antenna once, there isn't enough info to assume movement
# current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections == 1) & (daily_unique_events == 1) & previous_event %in% c("CF5", "HP3", "RB1") ~ "Not Enough Info", #Upstream Movement without hitting both antennas
# #this says that if the fish came from upstream and hit only one antenna one time, there isn't enough info to assume movement
# current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections == 1) & (daily_unique_events == 1) & previous_event %in% c("CF6", "HP4", "RB2") ~ "Not Enough Info"), #Downstream Movement without hitting both antennas
