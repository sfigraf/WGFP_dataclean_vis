# this should be get_states_function
### current assumptions: 
# if a fish hits the upstream antenna and not the downstream one on a given day, it is assumed to  have swam back upstream and NOT just missed the downstream antenna
# vice versa for downstream antenna
# so also if a fish is assumed to be downstream of RB1, and last hit RB1 only, then hits RB1 again, that is assumed as an upstream movement

# if a fish is released upstream of an antenna 

# hitching post release site is below the antennas and if a fish released at hithcing post hits the hithcin post antennas, it's an upstream movement

# no mobile detections incorporated for now
#All_events <- df_list$All_Events

get_states_function <- function(All_events) {
  library(tidyverse) 
  library(lubridate)
  
  start_time <- Sys.time()
  
  All_events_days <- All_events %>%
    mutate(days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days")))
    )
  
  
  All_events_days1 <- All_events_days %>%
    filter(!Event %in% c("M1", "M2")) %>% #filtering out mobile detections for now
    
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
    distinct(TAG, Event, Date, first_last, .keep_all = TRUE) %>%
    
    group_by(TAG) %>%
    mutate(
      previous_event = lag(Event, order_by = Datetime)) %>%
    
    select(Date, Datetime,TAG,Event,ReleaseSite,Species, Release_Length, Release_Weight, Release_Date, RecaptureSite, days_since, first_last, previous_event,c_number_of_detections, daily_unique_events) 

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
                                     Event == "B4" ~ 1,
                                     
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
                                      previous_event == "B4" ~ 1,
                                      
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
      
      movement = case_when(
        #if the values are more or less than previous values, it's moved upstream or downstream
        (current_event_vals > previous_event_vals) ~ "Downstream Movement",
        (current_event_vals < previous_event_vals) ~ "Upstream Movement",
        
        #if the values are the same and the day is the same, it means there was multiple consecutive detections at the same antenna and same day 
        current_event_vals == previous_event_vals & (Date == lag(Date, order_by = Datetime)) ~ "No Movement; Same Day",
        
        #if the vals are the same but the day is different, and the previous event was a stagnant state (B3, b4, release, etc) then there was no movement and the fish is in the same state it was
        current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & previous_event %in% c("B3", "B4", "Release", "Recapture", "Recapture and Release") ~ "No Movement",
        
        #if a fish ended the previous detection upstream of an antenna, then hits the same antenna again AND hits a different event that day (probably the other antenna detection from that site), it's a downstream movement
        current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections > 1) & (daily_unique_events > 1) & previous_event %in% c("CF6", "HP4", "RB2") ~ "Downstream Movement1",
        #if a fish ended the previous detection downstream of an antenna, then hits the same antenna again AND has a different event that day (probably the other antenna detection from that site), it's a upstream movement
        current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections > 1) & (daily_unique_events > 1) & previous_event %in% c("CF5", "HP3", "RB1") ~ "Upstream Movement1",
        
        
        
        #this says that if a fish ended the previous detection upstream of a antenna, then hits the same antenna again multiple times without hitting the next downstream antenna, it's assumed to have then ended back upstream
        current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections > 1) & (daily_unique_events == 1) & previous_event %in% c("CF6", "HP4", "RB2") ~ "Down then back up move",
        #this says that if a fish ended the previous detection downstream of a antenna, then hits the same antenna again multiple times without hitting the next upstream antenna, it's assumed to have then ended back upstream
        
        current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections > 1) & (daily_unique_events == 1) & previous_event %in% c("CF5", "HP3", "RB1") ~ "Up then back down move",
        #this says that if the fish came from downstream and hit only one antenna one time, it's assumed to have continued upstream and just missed hitting the upstream antenna
        current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections == 1) & (daily_unique_events == 1) & previous_event %in% c("CF5", "HP3", "RB1") ~ "Upstream Movement without hitting both antennas",
        #this says that if the fish came from upstream and hit only one antenna one time, it's assumed to have continued downstream and just missed hitting the other downstream antenna
        current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections == 1) & (daily_unique_events == 1) & previous_event %in% c("CF6", "HP4", "RB2") ~ "Downstream Movement without hitting both antennas"),
      
      teststate_11 = case_when(movement %in% c("Downstream Movement", "Downstream Movement1","Downstream Movement without hitting both antennas")  & (Event %in% c("CF5", "CF6")) ~ "L",
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
      
      
      
    ) #%>% #end of mutate
    #select(Date, Datetime, first_last, Event, movement,  teststate_11, c_number_of_detections, daily_unique_events, ReleaseSite, RecaptureSite, TAG)
  
  
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
    select(Date, Datetime, TAG, teststate_4, ReleaseSite, Species, Release_Length, Release_Weight, movement, c_number_of_detections, daily_unique_events, days_since)
          
  #testState4 is really the realState
  # r3 <- r2 %>%
  #   # mutate(New_movt = case_when(teststate_4 %in% c("GH", "HG","LK","KL", "IJ","JI") ~ "No Net Movement",
  #   #                             teststate_4 %in% c("K","I","G")~ "Upstream Movement",
  #   #                             teststate_4 %in% c("L","J", "H") ~ "Downstream Movement",
  #   #                             teststate_4 == lag(teststate_4, order_by = Datetime) ~ "No Net Movement",
  #   #                             teststate_4 != lag(teststate_4, order_by = Datetime) & (teststate_4 > lag(teststate_4, order_by = Datetime)) ~ "Upstream Movement",
  #   #                             teststate_4 != lag(teststate_4, order_by = Datetime) & (teststate_4 < lag(teststate_4, order_by = Datetime)) ~ "Downstream Movement")
  #   #                     )
  #   
  #   
  #   select(Date, Datetime,TAG,Event,teststate_4, movement, ReleaseSite,RecaptureSite, days_since, first_last, previous_event)
  

# Pivot_wider -------------------------------------------------------------

  days <- data.frame(days_since = 1:max(r2$days_since))
  
  days_and_states <- full_join(days, r2, by = "days_since")
  
  
  days_and_states_wide <- pivot_wider(days_and_states, id_cols = TAG, names_from = days_since, values_from = teststate_4)
  
  states_df_list <- list("All_States" = r2, "Days_and_states_wide" = days_and_states_wide)
  #this just tells how long the fucntion takes
  end_time <- Sys.time()
  print(paste("States Function took", round(end_time-start_time,2), "Seconds"))
  
  return(states_df_list)
}


#statesdf_list <- get_states_function(All_events)
#statesdf <- statesdf_list$All_States
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