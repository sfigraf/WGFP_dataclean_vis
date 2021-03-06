#### ENC HIST summary table function
# recaps_and_all_detections <- df_list$Recaps_detections
# release_data <- Release
# all_events_condensed_with_stations <- combined_events_stations

#recaps and all detreitons comes from WGFP ENC_hist_function, release data is a read_in csv, all_events_condensed with stations comes from combine_stations_events function
enc_hist_wide_summary_function <- function(recaps_and_all_detections, release_data, all_events_condensed_with_stations){
  
  start_time <- Sys.time()
  
  all_enc12 <- recaps_and_all_detections %>%
    count(TAG, Event, name = "Encounters") 
  
  all_enc12 <- pivot_wider(data = all_enc12, id_cols = TAG, names_from = Event, values_from = Encounters)
  
  x <- all_enc12 %>%
    replace_na(list(Species = "No Info", ReleaseSite = "No Info"))
  
  #all_enc12[is.na(all_enc12)]=0
  
  ENC_ALL <- all_enc12 %>%
    rename(RB1_n = RB1,
           RB2_n = RB2,
           HP3_n = HP3,
           HP4_n = HP4,
           CF5_n = CF5,
           CF6_n = CF6,
           M1_n = M1,
           M2_n = M2,
           B3_n = B3,
           B4_n = B4,
           Recap_n = Recapture
    ) %>%
    select(TAG, RB1_n,RB2_n,HP3_n, HP4_n, CF5_n, CF6_n, M1_n, M2_n, B3_n, B4_n, Recap_n)
  
  
  #### Combine Release data ###
  Release1 <- release_data %>%
    rename(TAG = TagID) %>%
    mutate(TAG = str_trim(TAG)) %>%
    replace_na(list(Species = "No Info", ReleaseSite = "No Info")) #replaced species and releasesite to follow the same convention as AllEvents
  
  # was geting a massive dataframe because the Release df is called TAGid not TAG.
  #need to actually join on full join not merge
  ENC_Release <- full_join(Release1, ENC_ALL,  by = "TAG")
  
  #gets tag list that wasn't in release file
  unknown_tags <- ENC_Release %>%
    filter(is.na(ReleaseSite)) %>%
    select(TAG,where(is.numeric))
  
  
  #ENC_Release11$TAG[3433:nrow(ENC_Release11)]
  ENC_Release[is.na(ENC_Release)]=0 #gets rest of the number count columns to 0 from NA
  
  #### Make 1 or 0 for encounter history rather than counts ###
  #gets df with TF of whether a fish was detected at a antenna
  ENC_Release1 <- ENC_Release %>%
    mutate(RB1 = (RB1_n >0),
           RB2 = (RB2_n >0),
           HP3 = (HP3_n >0),
           HP4 = (HP4_n >0),
           CF5 = (CF5_n >0),
           CF6 = (CF6_n >0),
           M1 = (M1_n >0),
           M2 = (M2_n >0),
           B3 = (B3_n >0),
           B4 = (B4_n>0),
           Recapture = (Recap_n > 0))
  
  
  #summary stats of each antenna encounter
  #precariously built because Row numbers are used
  #but also has release data
  totalcols <- ncol(ENC_Release1)
  
  ENC_Release2 <- ENC_Release1 %>%
    #counts number of TRUE across specified rows. -SG
    # need to have parentheses (totalcols-1) that's why i was getting bad numbers on biomark T/F initially
    mutate(
      TotalEncounters = rowSums(ENC_Release1[(totalcols-10):totalcols] == TRUE),
      
      TotalAntennas1 = rowSums(ENC_Release1[(totalcols-10):(totalcols-1)] == TRUE),
      TotalStationary = rowSums(ENC_Release1[(totalcols-10):(totalcols-5)] == TRUE),
      TotalMobile = rowSums(ENC_Release1[(totalcols-4):(totalcols-3)] == TRUE),
      TotalBiomark = rowSums(ENC_Release1[(totalcols-2):(totalcols-1)] == TRUE),
      TotalRB = rowSums(ENC_Release1[(totalcols-10):(totalcols-9)] == TRUE),
      TotalHP = rowSums(ENC_Release1[(totalcols-8):(totalcols-7)] == TRUE),
      TotalCf = rowSums(ENC_Release1[(totalcols-6):(totalcols-5)] == TRUE)
    ) %>%
    # just says if the fish was ever detected at these sites
    mutate(RB = (RB1_n > 0 | RB2_n >0),
           HP = (HP3_n > 0 | HP4_n >0),
           CF = (CF5_n > 0 | CF6_n >0),
           Biomark = (B3_n > 0 | B4_n >0),
           Mobile = (M1_n > 0 | M2_n >0)) %>%
    filter(!UTM_X %in% c(0, NA)) # one way to filter out tags that don't have any sort of release data; usually if they're entered in release file then they have UTM's
  
  ###Bringing in Station data with info about ABOVE/BELOW dam for joining
  
  
  above_below_counts <- all_events_condensed_with_stations %>%
    count(TAG, det_type, above_below, name = "Encounters") %>%
    mutate(combined_event = paste(det_type, above_below),
           EncountersTF = ifelse(Encounters > 0, 
                                 TRUE,
                                 FALSE))
  
  above_below_counts1 <- pivot_wider(data = above_below_counts, id_cols = TAG, names_from = combined_event, values_from = EncountersTF)
  above_below_counts2 <- above_below_counts1 %>%
    select(TAG, `Release Above the Dam`,`Release Below the Dam`,`Recapture Above the Dam`,`Recapture Below the Dam`,`Recapture and Release Above the Dam`,`Recapture and Release Below the Dam`, `Mobile Run Above the Dam`, `Mobile Run Below the Dam`)
  #turns all the NA's made to FALSE
  above_below_counts2[is.na(above_below_counts2)] = FALSE

  ENC_Release3 <- left_join(ENC_Release2, above_below_counts2, by = "TAG")
  
  ENC_Release4 <- ENC_Release3 %>%
    mutate(through_dam = case_when(
      (RB1|RB2|HP3|HP4|B3|`Release Below the Dam`|`Recapture Below the Dam`|`Recapture and Release Below the Dam`|`Mobile Run Below the Dam`) == TRUE & (CF5|CF6|B4|`Release Above the Dam`|`Recapture Above the Dam`|`Recapture and Release Above the Dam`|`Mobile Run Above the Dam`) == TRUE ~ "Went through dam",
      (RB1|RB2|HP3|HP4|B3|`Release Below the Dam`|`Recapture Below the Dam`|`Recapture and Release Below the Dam`|`Mobile Run Below the Dam`) == TRUE & (CF5&CF6&B4&`Release Above the Dam`&`Recapture Above the Dam`&`Recapture and Release Above the Dam`&`Mobile Run Above the Dam`) == FALSE ~ "Stayed Below the Dam",
      (RB1&RB2&HP3&HP4&B3&`Release Below the Dam`&`Recapture Below the Dam`&`Recapture and Release Below the Dam`&`Mobile Run Below the Dam`) == FALSE & (CF5|CF6|B4|`Release Above the Dam`|`Recapture Above the Dam`|`Recapture and Release Above the Dam`|`Mobile Run Above the Dam`) == TRUE ~ "Stayed Above the Dam",
      
    ))
  #rearranging so that Tag is first column shown
  ENC_Release4<- ENC_Release4 %>%
    select(TAG, 1:ncol(ENC_Release4))
  ###joining on column with sum data
  #same code appears in movements function
  sum_dist1 <- all_events_condensed_with_stations %>%
    group_by(TAG) %>%
    arrange(Datetime) %>%
    mutate(dist_moved = ET_STATION - lag(ET_STATION, order_by = Datetime),
           sum_dist = (sum(abs(diff(ET_STATION, na.rm = TRUE))))
    ) %>% #end of mutate
    distinct(TAG, .keep_all = TRUE) %>%
    select(TAG, sum_dist)
  
  ENC_Release5 <- ENC_Release4 %>%
    left_join(sum_dist1, by = "TAG")
  
  
  
  
  
  enc_wide_list <- list(
    "ENC_Release_wide_summary" = ENC_Release5, "Unknown_Tags" = unknown_tags
  )
  
  end_time <- Sys.time()
  print(paste("Encounter Histories Summary Wide Function took", round(end_time-start_time,2), "Seconds"))
  
  return(enc_wide_list)
}
