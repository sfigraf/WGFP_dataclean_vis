# this should be get_states_function

All_events <- df_list$All_Events

All_events_days <- All_events %>%
  mutate(days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days")))
  )


All_events_days1 <- All_events_days %>%
  group_by(Date, TAG) %>%
  mutate(first_last = case_when(Datetime == min(Datetime) & Event != "Release" ~ "First_of_day",
                                Datetime == max(Datetime) ~ "Last_of_day",
                                Datetime != min(Datetime) & Datetime != max(Datetime) ~ "0",
                                #Event == "Release" ~ "Last_of_day"
  )
  ) %>%
  ungroup() %>%
  distinct(TAG, Event, days_since, first_last, .keep_all = TRUE) %>%
  
  group_by(TAG) %>%
  mutate(
    previous_event = lag(Event, order_by = Datetime)) %>%
  
  select(Date, Datetime,TAG,Event,ReleaseSite,RecaptureSite, days_since, first_last, previous_event) 
  
u1 <- All_events_days1 %>%
  # no need to group_by date until states will be consolidated
  # need to group_by tag though so that the Lag(Date) will get the last date that that fish was detected
  # some movements weren't being recorded correctly because it was grouping by both date and Tag
  group_by(TAG) %>% #will also want to group_by Tag for all fish
  # ungroup() %>%
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
                                    previous_event == "B3" ~ 6,
                                    
                                    previous_event == "Release" & ReleaseSite == "Lower River Run" ~ 4,
                                    previous_event == "Release" & ReleaseSite == "Fraser River Ranch" ~ 2,
                                    previous_event == "Release" & ReleaseSite == "Kaibab Park" ~ 1,
                                    previous_event == "Release" & ReleaseSite == "Upper River Run" ~ 3,
                                    previous_event == "Release" & ReleaseSite == "Below Confluence Antenna" ~ 5,
                                    previous_event == "Release" & ReleaseSite == "Windy Gap Dam" ~ 6,
                                    previous_event == "Release" & ReleaseSite == "Hitching Post" ~ 7,
                                    previous_event == "Release" & ReleaseSite == "Chimney Rock Above Island" ~ 8,
                                    previous_event == "Release" & ReleaseSite == "Chimney Rock Below Island" ~ 9,
                                    previous_event == "Release" & ReleaseSite == "Upper Red Barn Fry Site" ~ 10,
                                    previous_event == "Release" & ReleaseSite == "Pool Above Red Barn Antenna" ~ 11,
                                    previous_event == "Release" & ReleaseSite == "Lower Red Barn Fry Site" ~ 12,
                                    previous_event == "Release" & ReleaseSite == "Below Red Barn Diversion #1" ~ 13,
                                    previous_event == "Release" & ReleaseSite == "Below Red Barn Diversion #2" ~ 14,
                                    previous_event == "Release" & ReleaseSite == "Kinney Creek" ~ 15,
                                    previous_event == "Release" & ReleaseSite == "Dark Timber Above Railroad" ~ 16,
                                    previous_event == "Release" & ReleaseSite == "Sheriff Ranch Upper Field" ~ 17,
                                    previous_event == "Release" & ReleaseSite == "Shefiff Ranch Middle Field" ~ 18, #will need to be changed once this typo is corrected
                                    previous_event == "Release" & ReleaseSite == "Sheriff Ranch Fry Site" ~ 19),
    
    # this is for quantiying general upstream/downstream movement
    movement = case_when((current_event_vals > previous_event_vals) ~ "Downstream Movement",
                         (current_event_vals < previous_event_vals) ~ "Upstream Movement",
                         current_event_vals == previous_event_vals & (Date == lag(Date, order_by = Datetime)) ~ "No Movement",
                         current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & previous_event %in% c("CF6", "HP4", "RB2") ~ "Downstream Movement",
                         current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & previous_event %in% c("CF5", "HP3", "RB1") ~ "Upstream Movement"),
    teststate_1 = case_when(movement == "Downstream Movement" & (Event %in% c("CF5", "CF6")) ~ "L",
                            movement == "Upstream Movement" & (Event %in% c("CF5", "CF6")) ~ "K",
                            movement == "Downstream Movement" & (Event %in% c("HP3", "HP4")) ~ "J",
                            movement == "Upstream Movement" & (Event %in% c("HP3", "HP4")) ~ "I",
                            movement == "Downstream Movement" & (Event %in% c("RB1", "RB2")) ~ "H",
                            movement == "Upstream Movement" & (Event %in% c("RB1", "RB2")) ~ "G",
                            Event == "B3" ~ "C",
                            Event == "B4" ~ "F",
                            Event == "Release" & ReleaseSite == "Lower River Run" ~ "E",
                            Event == "Release" & ReleaseSite == "Fraser River Ranch" ~ "F",
                            Event == "Release" & ReleaseSite == "Kaibab Park" ~ "F",
                            Event == "Release" & ReleaseSite == "Upper River Run" ~ "E",
                            Event == "Release" & ReleaseSite == "Below Confluence Antenna" ~ "D",
                            Event == "Release" & ReleaseSite == "Windy Gap Dam" ~ "C",
                            Event == "Release" & ReleaseSite == "Hitching Post" ~ "C",
                            Event == "Release" & ReleaseSite == "Chimney Rock Above Island" ~ "B",
                            Event == "Release" & ReleaseSite == "Chimney Rock Below Island" ~ "B",
                            Event == "Release" & ReleaseSite == "Upper Red Barn Fry Site" ~ "B",
                            Event == "Release" & ReleaseSite == "Pool Above Red Barn Antenna" ~ "B",
                            Event == "Release" & ReleaseSite == "Lower Red Barn Fry Site" ~ "A",
                            Event == "Release" & ReleaseSite == "Below Red Barn Diversion #1" ~ "A",
                            Event == "Release" & ReleaseSite == "Below Red Barn Diversion #2" ~ "A",
                            Event == "Release" & ReleaseSite == "Kinney Creek" ~ "A",
                            Event == "Release" & ReleaseSite == "Dark Timber Above Railroad" ~ "A",
                            Event == "Release" & ReleaseSite == "Sheriff Ranch Upper Field" ~ "A",
                            Event == "Release" & ReleaseSite == "Shefiff Ranch Middle Field" ~ "A", #will need to be changed once this typo is corrected
                            Event == "Release" & ReleaseSite == "Sheriff Ranch Fry Site" ~ "A"),)
    
  ) #

u2 <- u1 %>%
  filter(!is.na(teststate_1)) %>%
  group_by(Date, TAG) %>% #Error: must be size x not size y solved by grouping_by TAG as well as Date
  mutate(
    #tesst = str_detect(teststate_1, "[:alpha:]"))
    
    teststate_2 = paste0(teststate_1[which(Datetime == min(Datetime))],
                         #                                                                                                                               #unique(teststate_1), 
                         teststate_1[which(Datetime == max(Datetime))]),
    teststate_3 = str_c(teststate_1, collapse = NULL )
    
    # case_when(teststate_1[which(Datetime == min(Datetime))] != teststate_1[which(Datetime == max(Datetime))] ~ paste0(teststate_1[which(Datetime == min(Datetime))],
    #                                                                                                                               #unique(teststate_1), 
    #                                                                                                                               teststate_1[which(Datetime == max(Datetime))]),
    #                       teststate_1[which(Datetime == min(Datetime))] == teststate_1[which(Datetime == max(Datetime))] ~ teststate_1)
    
  )  %>%
  distinct(Date, TAG, teststate_2, .keep_all = TRUE) %>%
  mutate(teststate_4 = gsub('([[:alpha:]])\\1+', '\\1', teststate_2)) #removes consecutive letters

#testState4 is really the realState
u3 <- u2 %>%
  # mutate(New_movt = case_when(teststate_4 %in% c("GH", "HG","LK","KL", "IJ","JI") ~ "No Net Movement",
  #                             teststate_4 %in% c("K","I","G")~ "Upstream Movement",
  #                             teststate_4 %in% c("L","J", "H") ~ "Downstream Movement",
  #                             teststate_4 == lag(teststate_4, order_by = Datetime) ~ "No Net Movement",
  #                             teststate_4 != lag(teststate_4, order_by = Datetime) & (teststate_4 > lag(teststate_4, order_by = Datetime)) ~ "Upstream Movement",
  #                             teststate_4 != lag(teststate_4, order_by = Datetime) & (teststate_4 < lag(teststate_4, order_by = Datetime)) ~ "Downstream Movement")
  #                     )
         

  select(Date, Datetime,TAG,Event,teststate_4, movement, ReleaseSite,RecaptureSite, days_since, first_last, previous_event)

#####################
#test_days <- pivot_wider(data = All_events_days1, id_cols = TAG, names_from = days_since, values_from = Event)

single_tag <- All_events_days1 %>%
  select(Date, Datetime,TAG,Event,movement,ReleaseSite,RecaptureSite, days_since, first_last, previous_event) %>%
  filter(TAG %in% "230000224371") 


#which(single_tag$first_last == "First_of_day")
#single_tag_with_release <- single_tag
#incomplete list for now
# values_list = list(
#   "Fraser River Ranch" = 2, "Lower River Run" = 4, "CF6" = 4.1, "CF5" = 4.9, "Below Confluence Antenna" = 5, 
#   "Pool Above Red Barn Antenna" = 11, "RB2" = 11.1, "RB1" = 11.9,
#   "Hitching Post" = 7, "HP4" = 7.1, "HP3" = 7.9,
#   "Windy Gap Dam" = 6, "B3" = 6,
#   "B4" = .1,
#   "Release" = 0
# )

t1 <- Release %>%
  distinct(RS_Num, .keep_all = TRUE)
#if count(Date) has entries >1: 
r1 <- single_tag %>%
  #mutate(previous_event = lead(Event)) %>%
  group_by(Date) %>% #will also want to group_by Tag for all fish
  
  ungroup() %>%
    
    mutate(
      current_event_vals = case_when(Event == "RB1" ~ 11.9,
                                     Event == "RB2" ~ 11.1,
                                     Event == "HP3" ~ 7.9,
                                     Event == "HP4" ~ 7.1,
                                     Event == "CF5" ~ 4.9,
                                     Event == "CF6" ~ 4.1,
                                     Event == "B3" ~ 6
      ),
      previous_event_vals = case_when(previous_event == "RB1" ~ 11.9,
                                      previous_event == "RB2" ~ 11.1,
                                      previous_event == "HP3" ~ 7.9,
                                      previous_event == "HP4" ~ 7.1,
                                      previous_event == "CF5" ~ 4.9,
                                      previous_event == "CF6" ~ 4.1,
                                      previous_event == "B3" ~ 6,
                                      previous_event == "Release" & ReleaseSite == "Lower River Run" ~ 4,
                                      previous_event == "Release" & ReleaseSite == "Fraser River Ranch" ~ 2),
      
      # this is for quantiying general upstream/downstream movement
      movement = case_when((current_event_vals > previous_event_vals) ~ "Downstream Movement",
                           (current_event_vals < previous_event_vals) ~ "Upstream Movement",
                           current_event_vals == previous_event_vals & (Date == lag(Date, order_by = Datetime)) ~ "No Movement",
                           current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & previous_event %in% c("CF6", "HP4", "RB2") ~ "Downstream Movement",
                           current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & previous_event %in% c("CF5", "HP3", "RB1") ~ "Upstream Movement"),
      teststate_1 = case_when(movement == "Downstream Movement" & (Event %in% c("CF5", "CF6")) ~ "L",
                              movement == "Upstream Movement" & (Event %in% c("CF5", "CF6")) ~ "K",
                              movement == "Downstream Movement" & (Event %in% c("HP3", "HP4")) ~ "J",
                              movement == "Upstream Movement" & (Event %in% c("HP3", "HP4")) ~ "I",
                              movement == "Downstream Movement" & (Event %in% c("RB1", "RB2")) ~ "H",
                              movement == "Upstream Movement" & (Event %in% c("RB1", "RB2")) ~ "G",
                              Event == "B3" ~ "C")
      
      # test12 = values_list[[Event]] 
      # US_DS = case_when(
      #   #previous_event == "Release" & (as.numeric(values_list[ReleaseSite] < as.numeric(values_list[Event])))[1] ~ "DS",
      #   (as.numeric(values_list[[as.character(Event)]]) > as.numeric(values_list[[as.character(previous_event)]]))[1] ~ "DS",
      #   # as.numeric(values_list[Event] < as.numeric(values_list[previous_event])) ~ "US"
      # )    
    ) #

r2 <- r1 %>%
  filter(!is.na(teststate_1)) %>%
  group_by(Date) %>%
  mutate(
    #tesst = str_detect(teststate_1, "[:alpha:]"))
    
    teststate_2 = paste0(teststate_1[which(Datetime == min(Datetime))],
                         #                                                                                                                               #unique(teststate_1), 
                         teststate_1[which(Datetime == max(Datetime))]),
    teststate_3 = str_c(teststate_1, collapse = NULL )
    
    # case_when(teststate_1[which(Datetime == min(Datetime))] != teststate_1[which(Datetime == max(Datetime))] ~ paste0(teststate_1[which(Datetime == min(Datetime))],
    #                                                                                                                               #unique(teststate_1), 
    #                                                                                                                               teststate_1[which(Datetime == max(Datetime))]),
    #                       teststate_1[which(Datetime == min(Datetime))] == teststate_1[which(Datetime == max(Datetime))] ~ teststate_1)
    
  )  %>%
  distinct(Date, TAG, teststate_2, .keep_all = TRUE) %>%
  mutate(teststate_4 = gsub('([[:alpha:]])\\1+', '\\1', teststate_2)) #removes consecutive letters

# to do:
# be able to capture all unique states in a day, not just the first and last ones of the day
# apply to all tags
# fill out full list of values and case_when for recaps and all release sites
# see if I can corporate release event and state