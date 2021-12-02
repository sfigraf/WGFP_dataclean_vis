
library(tidyverse)
# Create Function

WGFP_Encounter_FUN= function(Stationary, Mobile, Biomark, Release){
  
  library(tidyverse)
  library(lubridate)
  
  WGFP_NoMarkers <- Stationary %>%
    mutate(TAG = str_replace(str_trim(TAG), "\\_", "")) %>%
    filter(str_detect(TAG, "^900"), 
           !TAG %in% c("900230000102751","900226001581072","900230000004000"))
  
  ### Subset Detection Type "Codes" to only include Summary (S) and Individual (I) ###
  WGFP_Clean= data.frame(WGFP_NoMarkers[which(WGFP_NoMarkers$Code == "I" | WGFP_NoMarkers$Code == "S"),])
  
  
  ### Getting times to same format if needed
  #
  if (    length(unique( str_detect(Stationary$ARR, "PM|AM"))) > 1) {
    WGFP_Clean <- WGFP_Clean %>%
      mutate(ARR1 = case_when(str_detect(ARR, "AM") ~ hms(ARR) ,
                              str_detect(ARR, "PM") ~ hms(ARR) + hours(12),
                              #if it doesn't detect PM or AM just do hms(ARR)
                              str_detect(ARR, "PM|AM") == FALSE ~ hms(ARR))
      ) %>%
      mutate(ARR2 = as.character(as_datetime(ARR1)), 
             ARR3 = str_sub(ARR2, start = 11, end = -1)) %>%
      select(Code, DTY, ARR3,  TRF,  DUR,  TTY,  TAG,  SCD,  ANT,  NCD,  EFA ) %>%
      rename(ARR = ARR3)
  }
  
  
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
           Reader.ID = case_when(Reader.ID == "A1" ~ "B1",
                                 Reader.ID == "A2" ~ "B2",
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
    mutate(UTM_X =case_when(Reader.ID == "B1" ~ "416127.3",
                            Reader.ID == "B2" ~ "420727.9"),
           UTM_Y = case_when(Reader.ID == "B1" ~ "4440146",
                             Reader.ID == "B2" ~ "4437221")) %>%
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
  
    # mutate(TAG = case_when(str_detect(TAG, "^900") ~ str_sub(TAG, 4,-1),
    #                        str_detect(TAG, "!^900") ~ TAG),
    #         %>%
    select(MobileDate, MobileTime, TAG, MobileAnt, MobileUTM_X, MobileUTM_Y) %>%
    rename(Scan_Date = MobileDate, Scan_Time = MobileTime, Site_Code = MobileAnt, UTM_X = MobileUTM_X, UTM_Y = MobileUTM_Y)
  
  WG_bio <- bind_rows(WGFP_condensed,Biomark_condensed)
  All_detections <- bind_rows(WG_bio, Mobile_condensed)
  
  #cleaning timestamps if need be
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
  
  ### Create Encounter Histories ###
  
  # Gives # of encounters of each tag per antenna
  
  # Stationary Antennas
  StationaryEnc= WGFP_Clean %>%
    count(TAG,SCD, name = "Encounters")
  
  # Mobile Antennas
  MobileEnc= Mobile %>%
    mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG)) %>%
    count(TAG,MobileAnt, name = "Encounters")
  
  # Biomark Antennas
  BiomarkEnc <- biomark2 %>%
    mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG)) %>%
    count(TAG, Reader.ID, name = "Encounters")
  
  ### Separate Encounter histories by Antenna ###
  Enc_RB1 = StationaryEnc%>%
    filter(SCD == "RB1")
  sum(Enc_RB1$Encounters)
  
  Enc_RB2 = StationaryEnc%>%
    filter(SCD == "RB2")
  sum(Enc_RB2$Encounters)
  
  Enc_HP3 = StationaryEnc%>%
    filter(SCD == "HP3")
  sum(Enc_HP3$Encounters)
  
  Enc_HP4 = StationaryEnc%>%
    filter(SCD == "HP4")
  sum(Enc_HP4$Encounters)
  
  Enc_CF5 = StationaryEnc%>%
    filter(SCD == "CF5")
  sum(Enc_CF5$Encounters)
  
  Enc_CF6 = StationaryEnc%>%
    filter(SCD == "CF6")
  sum(Enc_CF6$Encounters)
  
  Mob_M1 = MobileEnc%>%
    filter(MobileAnt == "M1")
  sum(Mob_M1$Encounters)
  
  Mob_M2 = MobileEnc%>%
    filter(MobileAnt == "M2")
  sum(Mob_M2$Encounters)
  
  Bio_B1 <- BiomarkEnc %>%
    filter(Reader.ID == "B1")
  
  Bio_B2 <- BiomarkEnc %>%
    filter(Reader.ID == "B2")
  # Make Individual Encounter tables
  
  RB1=Enc_RB1 %>%
    select(TAG,Encounters)%>%
    rename(RB1_n = Encounters)
  
  RB2=Enc_RB2 %>%
    select(TAG,Encounters)%>%
    rename(RB2_n = Encounters)
  
  HP3=Enc_HP3 %>%
    select(TAG,Encounters)%>%
    rename(HP3_n = Encounters)
  
  HP4=Enc_HP4 %>%
    select(TAG,Encounters)%>%
    rename(HP4_n = Encounters)
  
  CF5=Enc_CF5 %>%
    select(TAG,Encounters)%>%
    rename(CF5_n = Encounters)
  
  CF6=Enc_CF6 %>%
    select(TAG,Encounters)%>%
    rename(CF6_n = Encounters)
  
  M1=Mob_M1 %>%
    select(TAG,Encounters)%>%
    rename(M1_n = Encounters)
  
  M2=Mob_M2 %>%
    select(TAG,Encounters)%>%
    rename(M2_n = Encounters)
  
  B1=Bio_B1 %>%
    select(TAG,Encounters)%>%
    rename(B1_n = Encounters)
  
  B2=Bio_B2 %>%
    select(TAG,Encounters)%>%
    rename(B2_n = Encounters)
  
  ### Merge All Encounter Histories by antenna ###
  
  # Merge only takes 2 values
  #RB
  ENC_RB= merge(RB1,RB2, all=TRUE)
  ENC_RB[is.na(ENC_RB)]=0
  #HP
  ENC_HP= merge(HP3,HP4, all=TRUE)
  ENC_HP[is.na(ENC_HP)]=0
  #CF
  ENC_CF= merge(CF5,CF6, all=TRUE)
  ENC_CF[is.na(ENC_CF)]=0
  #Mobile
  ENC_M1M2 = merge(M1,M2, all=TRUE)
  ENC_M1M2[is.na(ENC_M1M2)]=0
  #Biomark
  ENC_B1B2 = merge(B1,B2, all=TRUE)
  ENC_B1B2[is.na(ENC_B1B2)]=0
  
  
  # Merge RB HP
  ENC_RBHP= merge(ENC_RB,ENC_HP, all=TRUE)
  ENC_RBHP[is.na(ENC_RBHP)]=0
  
  
  # Merge RBHP with CF
  ENC_ALLStationary= merge(ENC_RBHP,ENC_CF, all=TRUE)
  ENC_ALLStationary[is.na(ENC_ALLStationary)]=0
  
  
  # Merge ENC_AllStationary with ENC_M1M2
  # was getting dupicate tag numbers at the very end bc I wasn't stripping the 900 from the TAG at the very beginning of the fucntion for BIOmark and Mobile
  # so it wasn't merging correctly. Then the 900 was stripped later but by then it didn't make a dif
  ENC_Stationary_M1M2= merge(ENC_ALLStationary,ENC_M1M2, all=TRUE)
  ENC_Stationary_M1M2[is.na(ENC_Stationary_M1M2)]=0
  
  # Merge ENC_AllStationary with ENC_B1B2
   #gets dataset of all encounters on all antennas
  ENC_ALL= merge(ENC_Stationary_M1M2,ENC_B1B2, all=TRUE)
  ENC_ALL[is.na(ENC_ALL)]=0
  
  
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
           Mobile = (M1_n > 0 | M2_n >0))
  
  # ### Save the function to use in the future ###
  # #setwd("U:\\Projects\\Colorado_River\\Windy_Gap_FishMovementStudy\\Data\\RFID\\Detections\\CodingDetections")
  # 
  # save("WGFP_Encounter_FUN", file="WGFP_Encounters_Function.Rdata")
  df_list <- list("ENC_ALL" = ENC_ALL, "WGFP_Clean" = WGFP_Clean, "ENC_Release2" = ENC_Release2, "All_Detections" = All_detections, "Unknown_Tags" = unknown_tags)
  return(df_list)
}
  
