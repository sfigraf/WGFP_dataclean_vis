
library(tidyverse)
# Create Function

WGFP_Encounter_FUN= function(Stationary, Mobile, Biomark, Release, Recaptures){
  
  library(tidyverse)
  library(lubridate)
  
  WGFP_NoMarkers <- Stationary %>%
    mutate(TAG = str_replace(str_trim(TAG), "\\_", "")) %>%
    filter(str_detect(TAG, "^900"), 
           !TAG %in% c("900230000102751","900226001581072","900230000004000"))
  
  ### Subset Detection Type "Codes" to only include Summary (S) and Individual (I) ###
  WGFP_Clean= data.frame(WGFP_NoMarkers[which(WGFP_NoMarkers$Code == "I" | WGFP_NoMarkers$Code == "S"),])
  
  #### Add Lat Longs to detections ###
   
  # takes out 900 from TAG in WGFP Clean
  # also takes out duplicate rows
  WGFP_Clean <- WGFP_Clean %>%
    mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG)) %>%
    
    # mutate(TAG = case_when(str_detect(TAG, "^900") ~ str_sub(TAG, 4,-1),
    #                        str_detect(TAG, "!^900") ~ TAG)) %>%
    mutate(UTM_X =case_when(SCD == "RB1" | SCD == "RB2" ~ "412607.7",
                            SCD == "HP3" | SCD == "HP4" ~ "414375.4",
                            SCD == "CF5" | SCD == "CF6" ~ "416965.0"),
           UTM_Y = case_when(SCD == "RB1" | SCD == "RB2" ~ "4439503.0",
                             SCD == "HP3" | SCD == "HP4" ~ "4440241.0",
                             SCD == "CF5" | SCD == "CF6" ~ "4439369.0")) %>%
    distinct()
  
  # biomark cleaning, getting dates into uniform format, 
  biomark2 <- Biomark %>%
    mutate(TAG = str_replace(DEC.Tag.ID, "\\.", ""),
           Reader.ID = case_when(Reader.ID == "A1" ~ "B3",
                                 Reader.ID == "A2" ~ "B4",
                                 str_detect(Reader.ID, "A1|A2") == FALSE ~ Reader.ID),
           #make a column for Scan>Date if parentheses are detected in the string, that means the format is in mdy 
           # and we want to convert it to YYYYMMDD format. elsewise, leave it as is
           Scan.Date = ifelse(str_detect(Scan.Date, "/"), 
                              as.character(mdy(Scan.Date)), 
                              Scan.Date)
             ) %>%
    filter(!TAG %in% c("900230000102751", "900226001581072", "999000000007586", "999000000007585" )) %>%
    
    # from gis: B1 416127.3, 4440146
    #B2: 420727.9, 4437221
    mutate(UTM_X =case_when(Reader.ID == "B3" ~ "416127.3",
                            Reader.ID == "B4" ~ "420727.9"),
           UTM_Y = case_when(Reader.ID == "B3" ~ "4440146",
                             Reader.ID == "B4" ~ "4437221")) %>%
    distinct()
  
  ###Create one big clean dataset
  WGFP_condensed <- WGFP_Clean %>%
    select(DTY, ARR, TAG, SCD, UTM_X, UTM_Y) %>%
    rename(Scan_Date = DTY, Scan_Time = ARR, Site_Code = SCD, UTM_X = UTM_X, UTM_Y = UTM_Y)
  
  
  Biomark_condensed <- biomark2 %>%
    mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG)) %>%
    select(Scan.Date, Scan.Time, TAG, Reader.ID, UTM_X, UTM_Y) %>%
    rename(Scan_Date = Scan.Date, Scan_Time = Scan.Time, Site_Code = Reader.ID, UTM_X = UTM_X, UTM_Y = UTM_Y)
  
  Mobile_condensed <- Mobile %>%
    mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG),
           MobileDate = ifelse(str_detect(MobileDate, "/"), 
                               as.character(mdy(MobileDate)), 
                               MobileDate)) %>% #end of mutate
    select(MobileDate, MobileTime, TAG, MobileAnt, MobileUTM_X, MobileUTM_Y) %>%
    rename(Scan_Date = MobileDate, Scan_Time = MobileTime, Site_Code = MobileAnt, UTM_X = MobileUTM_X, UTM_Y = MobileUTM_Y)
  
  WG_bio <- bind_rows(WGFP_condensed,Biomark_condensed)
  All_detections <- bind_rows(WG_bio, Mobile_condensed)
  
  #cleaning timestamps for mobile and old stationary detections mainly
  if (    length(unique( str_detect(All_detections$Scan_Time, "PM|AM"))) > 1) {
    All_detections <- All_detections %>%
      mutate(Scan_Time1 = case_when(str_detect(Scan_Time, "AM") ~ hms(Scan_Time) ,
                              str_detect(Scan_Time, "PM") ~ hms(Scan_Time) + hours(12),
                              #if it doesn't detect PM or AM just do hms(Scan_Time)
                              str_detect(Scan_Time, "PM|AM") == FALSE ~ hms(Scan_Time))
      ) %>%
      mutate(Scan_Time2 = as.character(as_datetime(Scan_Time1)), 
             Scan_Time3 = str_sub(Scan_Time2, start = 11, end = -1)) %>%
      select(Scan_Date, Scan_Time3, TAG, Site_Code, UTM_X, UTM_Y ) %>%
      rename(Scan_Time = Scan_Time3)
  }
  
  All_detections <- All_detections %>%
    filter(Scan_Date >= as.Date("2020-08-06")) %>% #right before the first date of marker tag detections on stationary antennas
    mutate(
      #datetime1 = as.POSIXct(paste(Scan_Date, Scan_Time),format="%Y-%m-%d %H:%M:%S"), #this line works too
           datetime2 = ymd_hms(paste(Scan_Date, Scan_Time))) %>%
    rename(Scan_DateTime = datetime2) %>%
    select(Scan_Date, Scan_DateTime, TAG, Site_Code, UTM_X, UTM_Y )
  
### all detections and recaps and release
  
  #getting timestamps in order and getting relevant columns
  Release1 <- Release %>%
    rename(TAG = TagID) %>%
    mutate(TAG = str_trim(TAG),
           Date = mdy(Date),
           Time1 = as_datetime(hm(Time)),
           Time2 = str_sub(Time1, start = 11, end = -1),
           DateTime = ymd_hms(paste(Date, Time2))) %>%
    select(RS_Num,River,ReleaseSite,Date,DateTime,UTM_X,UTM_Y,Species,Length,Weight,TAG,TagSize,Ant,Event) 
  
  #getting timestamps in order and getting relevant columns
  
  recaps1 <- Recaptures %>%
    rename(TAG = TagID) %>%
    mutate(TAG = str_trim(TAG),
           Date = mdy(Date),
           Time1 = as_datetime(hm(Time)),
           Time2 = str_sub(Time1, start = 11, end = -1),
           DateTime = ymd_hms(paste(Date, Time2))) %>%
    select(RS_Num,River,RecaptureSite,DateTime,Date,Time2,UTM_X,UTM_Y,Species,Length,Weight,TAG,TagSize,Ant,Event) %>%
    rename(Time = Time2,
           Recap_Length = Length,
           Recap_Weight = Weight
    )
  
  #getting all detections file ready to merge with encounters
  All_Detections_1_merge <- All_detections %>%
    mutate(Date = as.Date(Scan_Date)) %>%
    rename(
      DateTime = Scan_DateTime,
      Event = Site_Code) 
  
  #gets a df with a event "Release" without columns filled in yet
  all_detections_release <- bind_rows(All_Detections_1_merge, Release1)
  
  # bind rows vs left join; bind rows will make it so there is a "release" or "recapture" event and also make columns with relevant info
  
  detections_release_recaps <- bind_rows(all_detections_release, recaps1)
  
  #fills in release info so it is known at any row of detection
  filled_in_release_rows <- left_join(detections_release_recaps, Release1, by = c("TAG"))
  
  
  #this is the final df 
  
  #Change na to "No info" in select columns so that it will register with the Picker input in the app
  #pretty sure that's just a bug on the DT or shinyWidgets end that it can't select by NA
  # 87 rows were not even showing up on the all_events app because the Species was NA -12/14/21 SG
  
  filled_in_release_rows_condensed <- filled_in_release_rows %>%
    select(Date.x, DateTime.x, TAG, Event.x, Species.y, Length.y, Weight.y, ReleaseSite.y, Date.y, RecaptureSite, Recap_Length, Recap_Weight, UTM_X.x, UTM_Y.x) %>%
    rename(Release_Date = Date.y,
           Date = Date.x,
           Datetime = DateTime.x,
           Event = Event.x,
           Species = Species.y,
           Release_Length = Length.y,
           Release_Weight = Weight.y, 
           ReleaseSite = ReleaseSite.y,
           UTM_X = UTM_X.x,
           UTM_Y = UTM_Y.x) %>%
    replace_na(list(Species = "No Info", ReleaseSite = "No Info"))
  
  
  
### end of recaps and release joining section
### start of potentially now superfluous all_detecttions_release section  
  
  # Release1 <- Release %>%
  #   rename(TAG = TagID)
  # 
  # #full join retains tags with no release info, left join will get only the tags that have release info
  # 
  # #All_Detections_2 <- full_join(All_detections, Release1, by = "TAG")
  # All_Detections_2 <- left_join(All_detections, Release1, by = "TAG")
  # 
  # All_Detections_Release <- All_Detections_2 %>%
  #   select(Scan_Date, Scan_DateTime, TAG, Site_Code, UTM_X.x, UTM_Y.x, ReleaseSite, Date, Species, Length, Weight ) %>%
  #   rename(UTM_X = UTM_X.x,
  #          UTM_Y = UTM_Y.x,
  #          Release_Date = Date) %>%
  #   mutate(Release_Date = mdy(Release_Date))
  #   
  
  ### Create Encounter Histories ###
  
  all_enc12 <- All_detections %>%
    count(TAG, Site_Code, name = "Encounters") 
  
  all_enc12 <- pivot_wider(data = all_enc12, id_cols = TAG, names_from = Site_Code, values_from = Encounters)
  all_enc12[is.na(all_enc12)]=0
  
  ENC_ALL <- all_enc12 %>%
    rename(RB1_n = RB1,
           RB2_n = RB2,
           HP3_n = HP3,
           HP4_n = HP4,
           CF5_n = CF5,
           CF6_n = CF6,
           M1_n = M1,
           M2_n = M2,
           B1_n = B3,
           B2_n = B4
    ) %>%
    select(TAG, RB1_n,RB2_n,HP3_n, HP4_n, CF5_n, CF6_n, M1_n, M2_n, B1_n, B2_n)
  
  
  #### Merge Release data ###
  Release <- Release %>%
    rename(TAG = TagID) %>%
    mutate(TAG = str_trim(TAG))
    
  # was geting a massive dataframe because the Release df is called TAGid not TAG.
  #need to actually join on full join not merge
  ENC_Release <- full_join(Release, ENC_ALL,  by = "TAG")
  #gets tag list that wasn't in release file
  x <- ENC_Release %>%
    filter(is.na(ReleaseSite))
  
  unknown_tags <- x$TAG
  #ENC_Release11$TAG[3433:nrow(ENC_Release11)]
  #ENC_Release= merge(Release,ENC_ALL, all=TRUE)
  ENC_Release[is.na(ENC_Release)]=0
  
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
           B1 = (B1_n >0),
           B2 = (B2_n>0)) 
  
  #summary stats of each antenna encounter
  #precariously built because Row numbers are used
  #but also has release data
  totalcols <- ncol(ENC_Release1)
  
  ENC_Release2 <- ENC_Release1 %>%
    #counts number opf TRUE across specified rows. negates subsequent lines of code -SG
    mutate(TotalAntennas1 = rowSums(ENC_Release1[(totalcols-9):totalcols] == TRUE),
           TotalStationary = rowSums(ENC_Release1[(totalcols-9):(totalcols-4)] == TRUE),
           TotalMobile = rowSums(ENC_Release1[(totalcols-3):(totalcols-2)] == TRUE),
           TotalBiomark = rowSums(ENC_Release1[(totalcols-1):totalcols] == TRUE),
           TotalRB = rowSums(ENC_Release1[(totalcols-9):(totalcols-8)] == TRUE),
           TotalHP = rowSums(ENC_Release1[(totalcols-7):(totalcols-6)] == TRUE),
           TotalCf = rowSums(ENC_Release1[(totalcols-5):(totalcols-4)] == TRUE)
           ) %>%
    # just says if the fish was ever detected at these sites
    mutate(RB = (RB1_n > 0 | RB2_n >0),
           HP = (HP3_n > 0 | HP4_n >0),
           CF = (CF5_n > 0 | CF6_n >0),
           Biomark = (B1_n > 0 | B2_n >0),
           Mobile = (M1_n > 0 | M2_n >0)) %>%
    filter(!ReleaseSite %in% 0)
  
  df_list <- list("ENC_ALL" = ENC_ALL, "WGFP_Clean" = WGFP_Clean, "ENC_Release2" = ENC_Release2, "All_Detections" = All_detections, 
                  "All_Events" = filled_in_release_rows_condensed, "Unknown_Tags" = unknown_tags)
  return(df_list)
}
  
