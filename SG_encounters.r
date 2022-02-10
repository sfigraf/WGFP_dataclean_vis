library(tidyverse)
library(readxl)
library(lubridate)
library(fishualize)
library(randomcoloR)
library(ColorPalette)

start_color <- randomColor()

comp_cols <- c(start_color, 
               complementColors(start_color, 5))
tetra_cols <- c(start_color, tetradicColors(start_color, 5))
#library(threadr) #needed for period to string function



#Stationary = read.csv(paste0("WGFP_Raw_20211109.csv"), colClasses = )
Stationary = read.csv(paste0("WGFP_Raw_20211130.csv"))
#Stationary11 = read.csv(paste0("WGFP_Raw_20211122_1.csv"), colClasses = c(rep("character",11)))

# Read mobile antenna detections
Mobile = read.csv("WGFP_MobileDetections.csv", colClasses=c(rep("character",10)))

Mobile_2 <- read.csv("WGFP_Mobile_Detect_AllData.csv" , colClasses= c(rep("character",14), rep("numeric", 4), rep("character", 3)))

Mobile_condensed <- Mobile_2 %>%
  rename(TAG = TagID) %>%
  mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG),
         Date = ifelse(str_detect(Date, "/"), 
                             as.character(mdy(Date)), 
                             Date)) %>% #end of mutate
  select(Date, Time, TAG, Ant, UTM_X, UTM_Y) %>%
  rename(Scan_Date = Date, Scan_Time = Time, Site_Code = Ant)




WGFP_Mobile_Detect_AllData <- read_csv("most_current_to_integrate/WGFP_Mobile_Detect_AllData.csv", 
                                       +     col_types = cols(TagID = col_character()))
#Read Biomark

Biomark_Raw_20211109_1 <- read_csv("Biomark_Raw_20211109_1.csv", 
                                   col_types = cols(`DEC Tag ID` = col_character()))

# need to be put in with decimal registering as "," because otherwise it won't bring in the full DEC.Id tag
biomark1 <- read_csv("Biomark_Raw_20211109_1.csv", col_types = "cccccccccccccccc")
# biomark_col_names <- c("Scan.Date","Scan.Time","Download.Date", "Download.Time" ,"Reader.ID","Antenna.ID","HEX.Tag.ID","DEC.Tag.ID","Temperature.C",
#     "Signal.mV","Is.Duplicate","Latitude","Longitude","File.Name")
# b <- read_csv("Biomark_Raw_20211109.csv", col_types = "ctcccccccccccccc", col_names = biomark_col_names)
# 
# b <- b[-1,]

Biomark <- read.csv("Biomark_Raw_20211109_1.csv", dec = ","
                    #, Scan.Time= strptime(Scan.Time)
                    #colClasses = c("character", "character", rep("character", 12))
                    )

# Biomark1 <- read.csv("Biomark_Raw_1.csv", dec = ","
#                     #, Scan.Time= strptime(Scan.Time)
#                     #colClasses = c("character", "character", rep("character", 12))
# )
#biomark11 <- read.csv("Biomark_Raw_2.csv", dec = ",")

# Read release data
#Release = read.csv("WGFP_ReleaseData.csv",colClasses=c(rep("character",19)))
Release = read.csv("WGFP_ReleaseData_Master.csv",colClasses=c(rep("character",18)))


#allll <- merge(Stationary, Mobile, all = TRUE)

# xRelease$Date <- as_date(mdy(xRelease$Date))
# 
# yRelease$Date <- as_date(mdy(yRelease$Date))
#list of dataframes that the function returns


#in DF list: ENC_ALL is df with just tags and all the raw numbers of the detections at each site.
# WGFP Clean is the clean data file for windy gap (i guess I should also include clean biomark?)
# ENC_release2 has all tags, including release site info, and which antennas they were seen at, as well as other summary info
## good to have to filter and view one spefic tag's history
# All Detecitons is just one df of mobile, biomark and stationary antennas including UTMs, and date and time of detection. 
## good to have to be able to make a encounter history by week, or by day like matt did. Can make new column with "state" and work from there
## will need to look at lubridate sheet a lot probably
# unknown tags is a list of tags that were detected on antennas that aren't in the release file. 
df_list <- WGFP_Encounter_FUN(Stationary = Stationary, Mobile = Mobile, Release= Release, Biomark = Biomark)

ENC_Release2_1 <-  df_list$ENC_Release2
All_Detections_1 <- df_list$All_Detections
WGFP_Clean_1 <- df_list$WGFP_Clean

unknown_tags_1 <-df_list$Unknown_Tags
# ENC_Release2_2 <-  df_list$ENC_Release2
# All_Detections_2 <- df_list$All_Detections
# WGFP_Clean_2 <- df_list$WGFP_Clean
x <- ENC_Release2_1 %>%
  filter(TotalStationary >= 5)
# x <- All_Detections_1 %>%
#   select(-Scan_Time)
# 
# y <- All_Detections_2 %>%
#   select(-Scan_Time)
# 
# z <- anti_join(y, x)

unknown_tags_2 <-df_list$Unknown_Tags

#### data exploratoin/filtering

# x <- df_list$All_Detections %>%
#   filter(TAG == 230000292262) %>%
#   distinct(Scan_Date, .keep_all=TRUE)
# 
# x$Scan_Date
# 
# y <- df_list$All_Detections %>%
#   filter(Scan_Date >= as.Date("2020-10-16") & Scan_Date <= as.Date("2021-04-04"),
#          Site_Code == "RB1")
# max(as.numeric(WGFP_Clean$NCD), na.rm = TRUE)
# NCDlist <- unique(as.numeric(WGFP_Clean$NCD))
# 
x <- WGFP_Clean %>%
  # filter(TAG == 900230000228822)
  filter(DTY >= as.Date("2020-12-03") & DTY <= as.Date("2021-08-15"),
         TAG == "230000228929")

max(df_list$All_Detections$Scan_Date)
# x <- Stationary %>%
#   mutate(duplicated1 = duplicated(Stationary))
# 
# duplicate_rows_stationary <- subset(x, duplicated1 == TRUE)
# u_dup_tags <- unique(duplicate_rows_stationary$TAG)

# Combine Biomark ---------------------------------------------------------
# don't need to do this piece again
#bring in csvs
below_kaibab_1 <- read_excel("Biomark\\Kaibab_Park\\CR_KB_B2_20210916.xlsx", sheet = "Downloaded Tag IDs")
below_kaibab_2 <- read_excel("Biomark\\Kaibab_Park\\CR_KB_B2_20211006.xlsx", sheet = "Downloaded Tag IDs")
below_kaibab_3 <- read_excel("Biomark\\Kaibab_Park\\CR_KB_B2_20211019.xlsx", sheet = "Downloaded Tag IDs")


below_WG_1 <- read_excel("Biomark\\Below_Windy_Gap\\CR_WG_B1_20210916.xlsx", sheet = "Downloaded Tag IDs")
below_WG_2 <- read_excel("Biomark\\Below_Windy_Gap\\CR_WG_B1_20211006.xlsx", sheet = "Downloaded Tag IDs")
below_WG_3 <- read_excel("Biomark\\Below_Windy_Gap\\CR_WG_B1_20211021.xlsx", sheet = "Downloaded Tag IDs")

biomark <- bind_rows(below_kaibab_1,below_kaibab_2,below_kaibab_3,below_WG_1,below_WG_2,below_WG_3)
biomark$`Scan Date` <- as_date(mdy(biomark$`Scan Date`))

biomark1 <- biomark %>%
  distinct()

#str_replace(biomark$`DEC Tag ID`, "\\.", "")

# gets rid of period in tag number/makes new column TAG
# and gets rid of duplicate rows that were
# x <- duplicated(biomark)
# x <- biomark %>%
#   mutate(duplicated1 = duplicated(biomark))
# 
# duplicate_rows <- subset(x, duplicated1 == TRUE)

#write_csv(biomark1, "Biomark_Raw_1.csv")


biomark1 <- read_csv("Biomark_Raw_20211109.csv", col_types = "Dccccccccccccccc")

#this line is in the wgfp_encounters function
# biomark1 <- biomark22 %>%
#   mutate(TAG = str_replace(biomark$`DEC Tag ID`, "\\.", "")) %>%
#   distinct()

# x <- Stationary %>%
#   distinct()

Stationary2 <- Stationary %>%
  select(DTY, ARR, TAG, SCD, ANT) %>%
  rename()

Mobile2 <- Mobile %>%
  select()


#999.000000007585


# Intervals ---------------------------------------------------------------

library(fishualize)
All_events <- df_list$All_Events
Enc_release_data <- df_list$ENC_Release2
# x <- All_detections$Scan_Date
# earliest_date <- min(x)
# recent_date <- max(x)
# strptime(min(x), format = "%y/%m/%d")
# ymd(min(x))
# interval(start = min(x), end = max(x))
# difftime(as.POSIXct(recent_date), as.POSIXct(earliest_date))

# Error in as.POSIXct.default(time1) : 
#   do not know how to convert 'time1' to class "POSIXct"
# solved because I was trying to substract the dates(-) not give 2 arguments with comma
#ceiling rounds up to nearest integer larger than x. 
#weeks_since_launch<- as.numeric(ceiling(difftime(max(x), min(x), units = "weeks")))
##Weeks
all_events_05 <- All_events %>%
  mutate(weeks_since = as.numeric(ceiling(difftime(Date, min(Date), units = "weeks")))
         )

#unique tags by site and Day

# don't need this part anymore since all_detections now contains release data
# spc_enc_only <- Enc_release_data %>%
#   select(Species, Length, Weight, TAG)
# 
# detections_and_species <- left_join(All_detections_05, spc_enc_only, by = "TAG")

All_events_05 %>%
  count(Event, Species, weeks_since) %>%
  ggplot(aes(x = weeks_since, y = n, fill = Event)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Weekly detections by Site")

All_events_05 %>%
  count(Event, Species) %>%
  ggplot(aes(x = Species, y = n, fill = Event)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(title = "Raw Number of detections by Species and Site")

All_events_05 %>%
  distinct(TAG, Event, .keep_all = TRUE) %>%
  count(Event,Species) %>%
  ggplot(aes(x = Event, y = n, fill = Species)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Species Detections by Site, Controlled by Day", subtitle = "A fish with a million detections on one day only registers once") +
  ylab("Number of Unique Days") +
  xlab("Site") +
  scale_fill_fish_d(option = "Oncorhynchus_mykiss", begin = .1, end = 1) + #Oncorhynchus_mykiss #beginning and end adjust the color pallete 
  add_fishape(family = "Salmonidae",
              option = "Oncorhynchus_nerka",
               xmin = 1, xmax = 3, ymin = 200, ymax = 400,
              fill = fish(option = "Oncorhynchus_nerka", n = 4)[2],
               alpha = 0.8
              )

spp <- fishualize::fish_palettes()
library(rfishbase)
# 2. Get data on the included species from FishBase using the rfishbase package
dt <- rfishbase::species(gsub("_"," ", spp))

###Weeks since datawrangling
All_events_05 <- All_events %>%
  mutate(weeks_since = as.numeric(ceiling(difftime(Date, min(Date), units = "weeks")))
  )

All_events_1 <- All_events_05 %>%
  distinct(TAG, Event, weeks_since, .keep_all = TRUE)

All_events_weeks <- pivot_wider(data = All_events_1, id_cols = TAG, names_from = weeks_since, values_from = Event)

## Days
All_events <- df_list$All_Events

All_events_days <- All_events %>%
  mutate(days_since = as.numeric(ceiling(difftime(Date, min(Date), units = "days")))
  )

#test <- All_events_days %>%
 # filter(TAG %in% c("230000224371"))
# this might be where to put in the c_number of detections and u1
All_events_days1 <- All_events_days %>%
  #filter(!Event %in% c("M1", "M2")) %>% #filtering out mobile detections for now
  
  group_by(Date, TAG) %>%
  mutate(first_last = case_when(Datetime == min(Datetime) & Event != "Release" ~ "First_of_day",
                                Datetime == max(Datetime) ~ "Last_of_day",
                                Datetime != min(Datetime) & Datetime != max(Datetime) ~ "0",
                                #Event == "Release" ~ "Last_of_day"
                            ),
  c_number_of_detections = n(),
  u1 = length(unique(Event))
  ) %>%
  ungroup() %>%
  #distinct 
  distinct(TAG, Event, days_since, first_last, .keep_all = TRUE) %>%
  
  group_by(TAG) %>%
  mutate(
    previous_event = lag(Event, order_by = Datetime)) %>%
  
  select(Date, Datetime,TAG,Event,ReleaseSite,RecaptureSite, days_since, first_last, previous_event, c_number_of_detections, u1) 

#test_days <- pivot_wider(data = All_events_days1, id_cols = TAG, names_from = days_since, values_from = Event)

single_tag <- All_events_days1 %>%
  select(Date, Datetime,TAG,Event,ReleaseSite,RecaptureSite, days_since, first_last, previous_event,c_number_of_detections, u1) %>%
  filter(TAG %in% "230000142699") #230000224371 #230000228314 this tag has both "movement up without hitting antenna" and "up/down without hitting multiple antenna  #230000228714 this one hit 5 in a day


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

# t1 <- Release %>%
#   distinct(RS_Num, .keep_all = TRUE)
#if count(Date) has entries >1: 
# r1 <- single_tag %>%
#   group_by(TAG) %>% 
#   # ungroup() %>%
#   mutate(
#     
#     current_event_vals = case_when(Event == "RB1" ~ 11.9,
#                                    Event == "RB2" ~ 11.1,
#                                    Event == "HP3" ~ 7.9,
#                                    Event == "HP4" ~ 7.1,
#                                    Event == "CF5" ~ 4.9,
#                                    Event == "CF6" ~ 4.1,
#                                    Event == "B3" ~ 6,
#                                    Event == "B4" ~ 1,
#                                    
#                                    Event == "Recapture" & RecaptureSite == "Lower River Run" ~ 4,
#                                    Event == "Recapture" & RecaptureSite == "Fraser River Ranch" ~ 2,
#                                    Event == "Recapture" & RecaptureSite == "Kaibab Park" ~ 1,
#                                    Event == "Recapture" & RecaptureSite == "Upper River Run" ~ 3,
#                                    Event == "Recapture" & RecaptureSite == "Below Confluence Antenna" ~ 5,
#                                    Event == "Recapture" & RecaptureSite == "Windy Gap Dam" ~ 6,
#                                    Event == "Recapture" & RecaptureSite == "Hitching Post" ~ 7,
#                                    Event == "Recapture" & RecaptureSite == "Chimney Rock Above Island" ~ 8,
#                                    Event == "Recapture" & RecaptureSite == "Chimney Rock Below Island" ~ 9,
#                                    Event == "Recapture" & RecaptureSite == "Upper Red Barn Fry Site" ~ 10,
#                                    Event == "Recapture" & RecaptureSite == "Pool Above Red Barn Antenna" ~ 11,
#                                    Event == "Recapture" & RecaptureSite == "Lower Red Barn Fry Site" ~ 12,
#                                    Event == "Recapture" & RecaptureSite == "Below Red Barn Diversion #1" ~ 13,
#                                    Event == "Recapture" & RecaptureSite == "Below Red Barn Diversion #2" ~ 14,
#                                    Event == "Recapture" & RecaptureSite == "Kinney Creek" ~ 15,
#                                    Event == "Recapture" & RecaptureSite == "Dark Timber Above Railroad" ~ 16,
#                                    Event == "Recapture" & RecaptureSite == "Sheriff Ranch Upper Field" ~ 17,
#                                    Event == "Recapture" & RecaptureSite == "Shefiff Ranch Middle Field" ~ 18,
#                                    Event == "Recapture" & RecaptureSite == "Sheriff Ranch Fry Site" ~ 19
#                                    
#     ),
#     previous_event_vals = case_when(previous_event == "RB1" ~ 11.9,
#                                     previous_event == "RB2" ~ 11.1,
#                                     previous_event == "HP3" ~ 7.9,
#                                     previous_event == "HP4" ~ 7.1,
#                                     previous_event == "CF5" ~ 4.9,
#                                     previous_event == "CF6" ~ 4.1,
#                                     previous_event == "B3" ~ 6,
#                                     previous_event == "B4" ~ 1,
#                                     
#                                     #using str_detect with "Release" gets both Release events and "recap and release" events
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Lower River Run" ~ 4,
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Fraser River Ranch" ~ 2,
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Kaibab Park" ~ 1,
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Upper River Run" ~ 3,
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Below Confluence Antenna" ~ 5,
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Windy Gap Dam" ~ 6,
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Hitching Post" ~ 7,
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Chimney Rock Above Island" ~ 8,
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Chimney Rock Below Island" ~ 9,
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Upper Red Barn Fry Site" ~ 10,
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Pool Above Red Barn Antenna" ~ 11,
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Lower Red Barn Fry Site" ~ 12,
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Below Red Barn Diversion #1" ~ 13,
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Below Red Barn Diversion #2" ~ 14,
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Kinney Creek" ~ 15,
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Dark Timber Above Railroad" ~ 16,
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Sheriff Ranch Upper Field" ~ 17,
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Shefiff Ranch Middle Field" ~ 18, #will need to be changed once this typo is corrected
#                                     str_detect(previous_event, "Release") & ReleaseSite == "Sheriff Ranch Fry Site" ~ 19,
#                                     
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Lower River Run" ~ 4,
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Fraser River Ranch" ~ 2,
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Kaibab Park" ~ 1,
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Upper River Run" ~ 3,
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Below Confluence Antenna" ~ 5,
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Windy Gap Dam" ~ 6,
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Hitching Post" ~ 7,
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Chimney Rock Above Island" ~ 8,
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Chimney Rock Below Island" ~ 9,
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Upper Red Barn Fry Site" ~ 10,
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Pool Above Red Barn Antenna" ~ 11,
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Lower Red Barn Fry Site" ~ 12,
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Below Red Barn Diversion #1" ~ 13,
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Below Red Barn Diversion #2" ~ 14,
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Kinney Creek" ~ 15,
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Dark Timber Above Railroad" ~ 16,
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Sheriff Ranch Upper Field" ~ 17,
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Shefiff Ranch Middle Field" ~ 18,
#                                     previous_event == "Recapture" & lag(RecaptureSite, order_by = Datetime) == "Sheriff Ranch Fry Site" ~ 19
#                                     
#                                     
#     ),
#     
#     # this is for quantiying general upstream/downstream movement
#     movement = case_when(
#       # if previous movement is > or < current one, it was either a upstream or downstream movement
#       (current_event_vals > previous_event_vals) ~ "Downstream Movement",
#       (current_event_vals < previous_event_vals) ~ "Upstream Movement",
#       
#       #if the values are the same and the day is the same, it means there was multiple consecutive detections at the same antenna and same day 
#       current_event_vals == previous_event_vals & (Date == lag(Date, order_by = Datetime)) ~ "No Movement; Same Day",
#       
#       #if the vals are the same but the day is different, and the previous event was a stagnant state (B3, b4, release, etc) then there was no movement and the fish is in the same state it was
#       current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & previous_event %in% c("B3", "B4", "Release", "Recapture", "Recapture and Release") ~ "No Movement",
#       
#       #if the vals are the same but the day is different and the previous state was  "movement" state, it is either a upstream movement or downstream one. 
#       # this is the part i might want to change because i kinda want something to show if a fish ended the day on the same antenna
#       current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & previous_event %in% c("CF6", "HP4", "RB2") ~ "Downstream Movement",
#       current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & previous_event %in% c("CF5", "HP3", "RB1") ~ "Upstream Movement"),
#     
#     #   case_when(
#     #   #current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & previous_event %in% c("CF5", "HP3", "RB1") ~ "Upstream Movement1",
#     #       
#     # ),
#     teststate_1 = case_when(movement == "Downstream Movement" & (Event %in% c("CF5", "CF6")) ~ "L",
#                             movement == "Upstream Movement" & (Event %in% c("CF5", "CF6")) ~ "K",
#                             movement == "Downstream Movement" & (Event %in% c("HP3", "HP4")) ~ "J",
#                             movement == "Upstream Movement" & (Event %in% c("HP3", "HP4")) ~ "I",
#                             movement == "Downstream Movement" & (Event %in% c("RB1", "RB2")) ~ "H",
#                             movement == "Upstream Movement" & (Event %in% c("RB1", "RB2")) ~ "G",
#                             Event == "B3" ~ "C",
#                             Event == "B4" ~ "F",
#                             str_detect(Event, "Release") & ReleaseSite == "Lower River Run" ~ "E",
#                             str_detect(Event, "Release") & ReleaseSite == "Fraser River Ranch" ~ "F",
#                             str_detect(Event, "Release") & ReleaseSite == "Kaibab Park" ~ "F",
#                             str_detect(Event, "Release") & ReleaseSite == "Upper River Run" ~ "E",
#                             str_detect(Event, "Release") & ReleaseSite == "Below Confluence Antenna" ~ "D",
#                             str_detect(Event, "Release") & ReleaseSite == "Windy Gap Dam" ~ "C",
#                             str_detect(Event, "Release") & ReleaseSite == "Hitching Post" ~ "C",
#                             str_detect(Event, "Release") & ReleaseSite == "Chimney Rock Above Island" ~ "B",
#                             str_detect(Event, "Release") & ReleaseSite == "Chimney Rock Below Island" ~ "B",
#                             str_detect(Event, "Release") & ReleaseSite == "Upper Red Barn Fry Site" ~ "B",
#                             str_detect(Event, "Release") & ReleaseSite == "Pool Above Red Barn Antenna" ~ "B",
#                             str_detect(Event, "Release") & ReleaseSite == "Lower Red Barn Fry Site" ~ "A",
#                             str_detect(Event, "Release") & ReleaseSite == "Below Red Barn Diversion #1" ~ "A",
#                             str_detect(Event, "Release") & ReleaseSite == "Below Red Barn Diversion #2" ~ "A",
#                             str_detect(Event, "Release") & ReleaseSite == "Kinney Creek" ~ "A",
#                             str_detect(Event, "Release") & ReleaseSite == "Dark Timber Above Railroad" ~ "A",
#                             str_detect(Event, "Release") & ReleaseSite == "Sheriff Ranch Upper Field" ~ "A",
#                             str_detect(Event, "Release") & ReleaseSite == "Shefiff Ranch Middle Field" ~ "A", #will need to be changed once this typo is corrected
#                             str_detect(Event, "Release") & ReleaseSite == "Sheriff Ranch Fry Site" ~ "A"),
#     
#   ) #end of mutate
# test_05 = lag(Date, order_by = Datetime),
#     test = (Date == lag(Date, order_by = Datetime)),
 # r2 <- r1 %>%
 #  filter(!is.na(teststate_1)) %>%
 #  group_by(Date) %>%
 #  mutate(
 #    #tesst = str_detect(teststate_1, "[:alpha:]"))
 #    
 #    teststate_2 = paste0(teststate_1[which(Datetime == min(Datetime))],
 #                         #                                                                                                                               #unique(teststate_1), 
 #                         teststate_1[which(Datetime == max(Datetime))]),
 #    teststate_3 = str_c(teststate_1, collapse = NULL )
 #      
 #      # case_when(teststate_1[which(Datetime == min(Datetime))] != teststate_1[which(Datetime == max(Datetime))] ~ paste0(teststate_1[which(Datetime == min(Datetime))],
 #      #                                                                                                                               #unique(teststate_1), 
 #      #                                                                                                                               teststate_1[which(Datetime == max(Datetime))]),
 #      #                       teststate_1[which(Datetime == min(Datetime))] == teststate_1[which(Datetime == max(Datetime))] ~ teststate_1)
 #          
 #   )  %>%
 #  distinct(Date, TAG, teststate_2, .keep_all = TRUE) %>%
 #  mutate(teststate_4 = gsub('([[:alpha:]])\\1+', '\\1', teststate_2)) #removes consecutive letters

# to do:
# be able to capture all unique states in a day, not just the first and last ones of the day
# apply to all tags
#


values_list = list(
  "Fraser River Ranch" = 2, "Lower River Run" = 4, "CF6" = 4.1, "CF5" = 4.9, "Below Confluence Antenna" = 5, 
  "Pool Above Red Barn Antenna" = 11, "RB2" = 11.1, "RB1" = 11.9,
  "Hitching Post" = 7, "HP4" = 7.1, "HP3" = 7.9,
  "Windy Gap Dam" = 6, "B3" = 6,
  "B4" = .1,
  "Release" = 0
)

r2_5 <- single_tag %>%
  #don't need this part now if these are calculated at the beginning of the thing
  # group_by(Date, TAG) %>%
  # mutate(c_number_of_detections = n(),
  #        u1 = length(unique(Event)),
  #        
  #        ) %>%
  # ungroup() %>%
  #need to group_by TAG for lag to work; don't group_by date or else lag will leave out some stuff
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
    current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections > 1) & (u1 > 1) & previous_event %in% c("CF6", "HP4", "RB2") ~ "Downstream Movement1",
    #if a fish ended the previous detection downstream of an antenna, then hits the same antenna again AND has a different event that day (probably the other antenna detection from that site), it's a upstream movement
    current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections > 1) & (u1 > 1) & previous_event %in% c("CF5", "HP3", "RB1") ~ "Upstream Movement1",
    
    
    
    #this says that if a fish ended the previous detection upstream of a antenna, then hits the same antenna again multiple times without hitting the next downstream antenna, it's assumed to have then ended back upstream
    current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections > 1) & (u1 == 1) & previous_event %in% c("CF6", "HP4", "RB2") ~ "Down then back up move",
    #this says that if a fish ended the previous detection downstream of a antenna, then hits the same antenna again multiple times without hitting the next upstream antenna, it's assumed to have then ended back upstream
    
    current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections > 1) & (u1 == 1) & previous_event %in% c("CF5", "HP3", "RB1") ~ "Up then back down move",
    #this says that if the fish came from downstream and hit only one antenna one time, it's assumed to have continued upstream and just missed hitting the upstream antenna
    current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections == 1) & (u1 == 1) & previous_event %in% c("CF5", "HP3", "RB1") ~ "Upstream Movement without hitting both antennas",
    #this says that if the fish came from upstream and hit only one antenna one time, it's assumed to have continued downstream and just missed hitting the other downstream antenna
    current_event_vals == previous_event_vals & Date != lag(Date, order_by = Datetime) & (c_number_of_detections == 1) & (u1 == 1) & previous_event %in% c("CF6", "HP4", "RB2") ~ "Downstream Movement without hitting both antennas"),

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
                          
                          #this section could be shortened without the str_detect
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
  
  

) %>% #end of mutate
  select(Date, Datetime, first_last, Event, movement,  teststate_11, c_number_of_detections, u1, ReleaseSite, RecaptureSite, TAG)
  #count(Date, TAG) 

# str_which(r1$RecaptureSite, "Hitching Post")
# r1$RecaptureSite[257] <- "Kinney Creek"
# x <- data.frame(str_detect(r1$Event, "Release|Recapture"))

r3_5 <- r2_5 %>%
  #at this point the only NA states are when there is no movement on the same day, so might as well get rid of them
  filter(!is.na(teststate_11)) %>%
  group_by(Date, TAG) %>%
  #arranging my datetime ensures that all states will be recorded in the correct order
  arrange(Datetime) %>%
  mutate(
    #tesst = str_detect(teststate_1, "[:alpha:]"))
    
    teststate_2 = case_when(min(Datetime) == max(Datetime) ~ teststate_11,
                            min(Datetime) != max(Datetime) & (u1 <= 2) ~ (paste0(teststate_11[which(Datetime == min(Datetime))],
                                                                    #                                                                                                                               #unique(teststate_1), 
                                                                    teststate_11[which(Datetime == max(Datetime))])),
                            #if there are many events, need to get all of them. Collapse aregument converts vector to string
                            min(Datetime) != max(Datetime) & (u1 > 2) ~ paste(teststate_11, collapse = "")
                            ), #end of case_when
      # paste0(teststate_11[which(Datetime == min(Datetime))],
      #                    #                                                                                                                               #unique(teststate_1), 
      #                    teststate_11[which(Datetime == max(Datetime))]),
    #teststate_3 = str_c(teststate_11, collapse = NULL )
    
    # case_when(teststate_1[which(Datetime == min(Datetime))] != teststate_1[which(Datetime == max(Datetime))] ~ paste0(teststate_1[which(Datetime == min(Datetime))],
    #                                                                                                                               #unique(teststate_1), 
    #                                                                                                                               teststate_1[which(Datetime == max(Datetime))]),
    #                       teststate_1[which(Datetime == min(Datetime))] == teststate_1[which(Datetime == max(Datetime))] ~ teststate_1)
    
  )  %>%
  mutate(teststate_4 = gsub('([[:alpha:]])\\1+', '\\1', teststate_2)) %>% #removes consecutive letters
  distinct(Date, TAG, teststate_4, .keep_all = TRUE) %>%
  select(Date, Datetime, first_last, movement, teststate_4,c_number_of_detections, u1, TAG)
  


single_tag %>%
  group_by(Date) %>%
  mutate(x1 = count(Date))

non_na_rows <- which(!is.na(x3$State))
######
# x <- statesdf %>%
#   mutate(days_since = as.numeric(ceiling(difftime(as.Date(Date), min(as.Date(Date)), units = "days"))))

days <- data.frame(days_since = 1:max(statesdf$days_since))

days_and_states <- full_join(days, statesdf, by = "days_since")


days_and_states_wide <- pivot_wider(days_and_states, id_cols = TAG, names_from = days_since, values_from = teststate_4)
#test_wider <- pivot_wider(data = x1, id_cols = TAG, names_from = days_since, values_from = Event)
####states new movemnt

statesdf_list <- get_states_function(All_events)
statesdf <- statesdf_list$All_States

x <- statesdf %>%
  group_by(TAG) %>%
    mutate(New_movt = case_when(State %in% c("GH", "HG","LK","KL", "IJ","JI") ~ "No Net Movement",
                                State %in% c("K","I","G")~ "Upstream Movement",
                                State %in% c("L","J", "H") ~ "Downstream Movement",
                                # State == lag(State, order_by = Datetime) ~ "No Net Movement",
                                # State != lag(State, order_by = Datetime) & (State > lag(State, order_by = Datetime)) ~ "Upstream Movement",
                                # State != lag(State, order_by = Datetime) & (State < lag(State, order_by = Datetime)) ~ "Downstream Movement")
                        )# end of case_when
    )

x1 <- x %>%
  #filter(!is.na(New_movt))
  filter(str_detect(New_movt,"Downstream Movement"))
  

x1 <- x %>%
  filter(!is.na(New_movt)) %>%
  arrange(Datetime) %>%
  ggplot(aes(x = Date, fill = New_movt)) +
  geom_bar() +
  scale_x_date(date_labels = "%m-%Y" #date_breaks = "1 month"
               ) +
  theme_classic()

ggplotly(x1)



data <- data.frame(
  day = as.Date("2017-06-14") - 0:364,
  value = runif(365) + seq(-140, 224)^2 / 10000
)

# Most basic bubble plot
p <- ggplot(data, aes(x=day, y=value)) +
  geom_line() + 
  xlab("")
p

str(x1$Date)

select(Date, Datetime,TAG,Event,State, movement, ReleaseSite,RecaptureSite, days_since, first_last, previous_event)

if (x3$State[non_na_rows[i]] == "F" & x3$State[non_na_rows[i+1]] == "L") {
  

all_tags <- All_events_days1 %>%
  select(Date, Datetime,TAG,Event,ReleaseSite,RecaptureSite, days_since) 

days <- data.frame(days_since = 1:455)

x <- full_join(days, single_tag, by = "days_since")

all_tags_combined <- full_join(days, all_tags, by = "days_since")

x1 <- x %>%
  group_by(days_since) %>%
  filter(is.na(Datetime)|Datetime == max(Datetime)) %>% ## need to keep NA entries
  ungroup() #need to ungroup in order to have "lag" work with making new column
    # mutate(
    #   #Date = Date,
    #           Datetime = max(Datetime),
    #           #TAG = TAG,
    #           #Event = Event
    #           )
  #distinct(days_since, .keep_all = TRUE)
    
#these designations work for after a grouping by day and getting the event the fish was last at; 
    # but if a fish used to be in state A, and missed a antenna and only was seen on HP3, that should be a US movement but as of now is classified as Upstream

    
x2 <- x1 %>%
  mutate(State = case_when(Event == "Release" & ReleaseSite == "Fraser River Ranch" ~ "F",
                           Event == "RB1" ~ "H", #downstream movement
                           Event == "RB2" ~ "G", #upstream movet
                           Event == "HP3" ~ "J", #DS
                           Event == "HP4" ~ "I", #US
                           Event == "CF5" ~ "L", #DS
                           Event == "CF6" ~ "K", #US
                           Event == "B3" ~ "C",
                           ))
         #                   lag(State) == "RF" ~ "F",
         #                   Event == "CF5" & lag(State) == "F" ~ "L")
         #   #lag(days_since, n = 1L)
         # ) #end of mutate

# can try and pivot wider and 
test_wider <- pivot_wider(data = x1, id_cols = TAG, names_from = days_since, values_from = Event)
test_wider <- pivot_wider(data = data1, id_cols = TAG, names_from = days_since, values_from = Event)



non_na_rows <- which(!is.na(x3$State))

x3$Event[non_na_rows[1]]

#create list assigning states values 
#then can create simple if statement checking if value is > than other
# but will still need to see which exact states those are 
# because you have to know which state it resides
x3 <- x2 %>%
  filter(days_since %in% (364:418))

length(non_na_rows)
#x3$State[3:30] <- 0
x3$State[(non_na_rows[1]+1):(non_na_rows[1+1]-1)] <- 0

#x3 is a df where there is only 1 event per days_since
#now that I know you only need 0's in between, makes it easier
States_function <- function(x3) {
  
  #getting a days df to bind on
  days1 <- data.frame(days_since = 1:455, TAG = rep(NA, 455), State = rep(NA, 455))
  days2<- pivot_wider(data = days1, id_cols = TAG, names_from = days_since, values_from = State)
  # gets a list of which rows aren't NA in the State column
  non_na_rows <- which(!is.na(x3$State))
  
  #replaces the NA entries in x3$TAG with the tag number. needed for row binding with TAG column later
  #works for now because there's only 1 tag in the df used (x3)
  #tag1 <- unique(x3$TAG)[2]
  x3 <- x3 %>%
    replace_na(list(TAG = unique(x3$TAG)[2]))
  
  
  for (i in 1:length(non_na_rows)) {
    if (x3$State[non_na_rows[i]] == "F" & x3$State[non_na_rows[i+1]] == "L") {
      x3$State[(non_na_rows[i]+1):(non_na_rows[i+1]-1)] <- 0
      #x3$State[non_na_rows[i]+1:non_na_rows[i+1]-1]
    } 
    
    # if (x3$State[non_na_rows[i]] == "RF" & x3$State[non_na_rows[i+1]] == "L") {
    #   x3$State[non_na_rows[i]+1:non_na_rows[i+1]-1]
    # } 
    # 
    else {
      #print("False")
    }
  }
  
  new_df <- pivot_wider(data = x3, id_cols = TAG, names_from = days_since, values_from = State) 
  
  new_df1 <- bind_rows(days2, new_df)
  return(new_df1)
  
}

y <- States_function(x3)

y1 <- y %>%
  select(TAG, 360:417)

# rowShift <- function(x, shiftLen = 1L) {
#   r <- (1L + shiftLen):(length(x) + shiftLen)
#   r[r<1] <- NA
#   return(x[r])
# }
    
all_tags_combined1 <- all_tags_combined %>%
  group_by(days_since, TAG) %>%
  filter(is.na(Datetime)|Datetime == max(Datetime)) ## need to keep NA entries

all_tags_combined2 <- all_tags_combined1 %>%
  mutate(State = case_when(Event == "Release" & ReleaseSite == "Fraser River Ranch" ~ "F",
                           Event == "RB1" ~ "H", #downstream movement
                           Event == "RB2" ~ "G", #upstream movet
                           Event == "HP3" ~ "J", #DS
                           Event == "HP4" ~ "I", #US
                           Event == "CF5" ~ "L", #DS
                           Event == "CF6" ~ "K", #US
                           Event == "B3" ~ "C",
  ))

test11 <- pivot_wider(data = all_tags_combined2, id_cols = TAG, names_from = days_since, values_from = State,)
# mutate(
#   #Date = Date,
#   Datetime = max(Datetime),
#   TAG = TAG
#   #Event = Event
# )

df <- tibble(
  x = sample(10, 100, rep = TRUE),
  y = sample(10, 100, rep = TRUE)
)  

y1 <- distinct(df, diff = abs(x - y))

y1 <- distinct(starwars, across(contains("color")))
starwars
# function that will tell if something is upstream of something else with 2 inputs; returns True or False (or maybe upstream or downstream)
#     or maybe could even by done with a list or df
# if release site is "x" and event is "y", state is "z"
# if no string detected in Event, continue putting previous state in
# if string detected: 
#   if return of upstream function is "upstream", put a upstream moving/"dummy" state
#   if return is downstream, put the downstream moving state  for that specific spot 
# if the string detected is a specific upstream moving state, then the next state is going to be another specific state because that's the only one it can be. 
#     same with downstream

# if there are 2 events on the same day, see which one occurred later in the day and take that one. 

# once df is filled out, convert to wide format and bind rows with other df



#just filtering days stuff
x <- All_detections %>%
  distinct(TAG, Site_Code, Scan_Date, .keep_all = TRUE)

### Getting times correct

x <- Biomark$Scan.Time[71755]
y <- mdy(x)
str_length(x)
str_detect(x, "/")
# if the string sonctains //
# Stationary1 <- Stationary %>%
#   filter(TAG == "900_230000228791")
#   #mutate(ARR1 = hms(ARR))


# Correcting bad timestamps in stationary file ----------------------------


# this is typically the problematic date range
Stationary12 <- Stationary %>%
  filter(
    DTY >= as.Date("2021-03-02") & DTY <= as.Date("2021-04-06"),
         SCD == "RB1",
         TAG != "900_230000228791")
         #str_length(ARR) <8)

# #this is the same as the filter right now in WGFP function
# WGFP_NoMarkers_1 <- Stationary %>%
#   mutate(TAG = str_replace(TAG, "\\_", "")) %>%
#   filter(str_detect(TAG, "^900"), 
#          !TAG %in% c("900230000102751","900226001581072","900230000004000"))
#            # Stationary$TAG !=  &
#            # Stationary$TAG !=  &
#            # Stationary$TAG !=  &
#            #this one is the ghost tag removed 4/6 from RB1
#            # (Stationary$TAG != "900230000228791" | DTY <= as.Date("2021-12-01"))
#   
# 
# WGFP_NoMarkers_11 <- Stationary11 %>%
#   mutate(TAG = str_replace(TAG, "\\_", "")) %>%
#   filter(str_detect(TAG, "^900"), 
#          !TAG %in% c("900230000102751","900226001581072","900230000004000"),
#          # Stationary$TAG !=  &
#          # Stationary$TAG !=  &
#          # Stationary$TAG !=  &
#          #this one is the ghost tag removed 4/6 from RB1
#          (Stationary$TAG != "900230000228791" | DTY <= as.Date("2021-03-02"))
#   )
# 
# 
# #see which rows in x are different from those in Y
# diferences <- anti_join(WGFP_NoMarkers, WGFP_NoMarkers_1)

#gets the rows that are have probematic ARR
problem_times <- Stationary %>%
  filter(str_length(ARR) < 8) %>%
  mutate(month111 = month(DTY)) 

#gets which ecat days are problematic
problem_times %>%
  distinct(DTY) 
  #distinct(month111, SCD,  .keep_all = TRUE)




write_csv(Stationary1, "WGFP_Raw_20211122.csv")
# there was a problem with the Timestamps used in the files where 791 (the ghost tag) was present and also in the APril detections. So this part and above
# is a 1 time solution to figure that all out
new_times <- read.csv("new_times.csv", colClasses = c(rep("character",11)))
#should be about 30030 entries
new_times1 <- new_times %>%
  filter(!TAG %in% c("900_230000228791"))

no_problem_times <- Stationary %>%
  filter(str_length(ARR) >= 8)

new_Stationary <- bind_rows(new_times1, no_problem_times)

#should be 30030 entries that are different
diferences <- anti_join(new_Stationary, Stationary)

write_csv(new_Stationary, "New_Stationary.csv")

# 
#   mutate(month111 = month(DTY)) %>%
#   distinct(month111, SCD, DTY, .keep_all = TRUE)

new_Stationary <- read.csv("New_Stationary.csv", colClasses = c(rep("character",11)))
x <- new_Stationary %>%
  filter(str_length(ARR) < 8)

new_Stationary1 <- new_Stationary %>%
  mutate(ARR1 = case_when(str_detect(ARR, "AM") ~ hms(ARR) ,
                          str_detect(ARR, "PM") ~ hms(ARR) + hours(12),
                          #if it doesn't detect PM or AM just do hms(ARR)
                          str_detect(ARR, "PM|AM") == FALSE ~ hms(ARR))
         ) %>%
  mutate(ARR2 = as.character(as_datetime(ARR1)), 
         ARR3 = str_sub(ARR2, start = 11, end = -1)) %>%
  select(Code, DTY, ARR3,  TRF,  DUR,  TTY,  TAG,  SCD,  ANT,  NCD,  EFA ) %>%
  rename(ARR = ARR3)

x <-unique( str_detect(Stationary$ARR, "PM|AM") )
length(x) >1
y <- unique(str_detect(new_Stationary1$ARR, "PM|AM"))
length(y) >1

#as_datetime(as.character(new_Stationary1$ARR1))

# library(threadr)
# #this funtion doesn't deal with decimal seconds very
# period_to_string(new_Stationary1$ARR1)
# 
# period_to_seconds(new_Stationary1$ARR1)
# str_detect(new_Stationary1$ARR[147013], "PM|AM") == FALSE
# x <- format(new_Stationary1$ARR1, "%H:%M:%S")
# 
# new_Stationary1 <- new_Stationary1 %>%
#   mutate(ARR2 = as.POSIXct(ARR1, origin = "1960-01-01")) %>%
#   mutate(ARR3 = str_sub(ARR1, start = 11, end = -3))
# as.POSIXct(new_Stationary1$ARR1, origin = "1960-01-01")
# as.character(new_Stationary1$ARR1)

Release1 <- Release %>%
  rename(TAG = TagID)
x <- anti_join(ENC_Release2_1, Release1, by = "TAG")

All_Detections_1 %>%
  filter(DTY >= "2020-12-03" & DTY <= "2021-04-15")
         
list1 <- list(All_Detections_1$TAG, All_Detections_1$Scan_Date, All_Detections_1$Site_Code)
sapply(list1, `[`, 1)


x<-x %>%
  #distinct(TAG, Site_Code, Scan_Date,.keep_all=TRUE) %>%
  distinct(TAG, .keep_all = TRUE) %>%
  filter(Species == "MTS")
  
  #unique(All_Detections_1[,c("TAG", "Scan_Date", "Site_Code")])
x <- unique(All_Detections_1[,c("TAG", "Scan_Date", "Site_Code")])

unique(df_list$All_Detections$ReleaseSite)

All_Detections_21 <- anti_join(All_detections, Release1, by = "TAG")
unique(All_Detections_21$TAG)

#gets x colors of the theme for that fish species
x <- fish(n = 5,
  
  option = "Oncorhynchus_mykiss"
)


# encounter histories condensing ------------------------------------------

all_enc1 <- All_detections %>%
  count(TAG, Site_Code, name = "Encounters")

# x <- all_enc1 %>%
#   filter(Site_Code == "B3")

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


#x <- anti_join(all_enc123, ENC_ALL)

  # mutate(RB1_n = ifelse(Site_Code == "RB1", Encounters, 0),
  #        RB2_n = ifelse(Site_Code == "RB2", Encounters, 0),
  #        HP3_n = ifelse(Site_Code == "HP3", Encounters, 0),
  #        HP4_n = ifelse(Site_Code == "HP4", Encounters, 0),
  #        CF5_n = ifelse(Site_Code == "CF5", Encounters, 0),
  #        CF6_n = ifelse(Site_Code == "CF6", Encounters, 0),
  #        M1_n = ifelse(Site_Code == "M1", Encounters, 0),
  #        M2_n = ifelse(Site_Code == "M2", Encounters, 0),
  #        B1_n = ifelse(Site_Code == "B3", Encounters, 0),
  #        B2_n = ifelse(Site_Code == "B4", Encounters, 0),
  #        
  #        
  #        )# end of mutate

# library(reshape2)
# x <- all_enc1 %>%
#   dcast(TAG~RB1_n + RB2_n, fun.aggregate = sum)
  #cast(TAG~c("RB1_n"))
  #extract(Encounters, into = "A")

# # Stationary Antennas
# StationaryEnc= WGFP_Clean %>%
#   count(TAG,SCD, name = "Encounters")
# 
# # Mobile Antennas
# MobileEnc= Mobile %>%
#   mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG)) %>%
#   count(TAG,MobileAnt, name = "Encounters")
# 
# # Biomark Antennas
# BiomarkEnc <- biomark2 %>%
#   mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG)) %>%
#   count(TAG, Reader.ID, name = "Encounters")

### Separate Encounter histories by Antenna ###
# Enc_RB1 = StationaryEnc%>%
#   filter(SCD == "RB1")
# sum(Enc_RB1$Encounters)
# 
# Enc_RB2 = StationaryEnc%>%
#   filter(SCD == "RB2")
# sum(Enc_RB2$Encounters)
# 
# Enc_HP3 = StationaryEnc%>%
#   filter(SCD == "HP3")
# sum(Enc_HP3$Encounters)
# 
# Enc_HP4 = StationaryEnc%>%
#   filter(SCD == "HP4")
# sum(Enc_HP4$Encounters)
# 
# Enc_CF5 = StationaryEnc%>%
#   filter(SCD == "CF5")
# sum(Enc_CF5$Encounters)
# 
# Enc_CF6 = StationaryEnc%>%
#   filter(SCD == "CF6")
# sum(Enc_CF6$Encounters)
# 
# Mob_M1 = MobileEnc%>%
#   filter(MobileAnt == "M1")
# sum(Mob_M1$Encounters)
# 
# Mob_M2 = MobileEnc%>%
#   filter(MobileAnt == "M2")
# sum(Mob_M2$Encounters)
# 
# Bio_B1 <- BiomarkEnc %>%
#   filter(Reader.ID == "B3")
# 
# Bio_B2 <- BiomarkEnc %>%
#   filter(Reader.ID == "B4")
# # Make Individual Encounter tables
# 
# RB1=Enc_RB1 %>%
#   select(TAG,Encounters)%>%
#   rename(RB1_n = Encounters)
# 
# RB2=Enc_RB2 %>%
#   select(TAG,Encounters)%>%
#   rename(RB2_n = Encounters)
# 
# HP3=Enc_HP3 %>%
#   select(TAG,Encounters)%>%
#   rename(HP3_n = Encounters)
# 
# HP4=Enc_HP4 %>%
#   select(TAG,Encounters)%>%
#   rename(HP4_n = Encounters)
# 
# CF5=Enc_CF5 %>%
#   select(TAG,Encounters)%>%
#   rename(CF5_n = Encounters)
# 
# CF6=Enc_CF6 %>%
#   select(TAG,Encounters)%>%
#   rename(CF6_n = Encounters)
# 
# M1=Mob_M1 %>%
#   select(TAG,Encounters)%>%
#   rename(M1_n = Encounters)
# 
# M2=Mob_M2 %>%
#   select(TAG,Encounters)%>%
#   rename(M2_n = Encounters)
# 
# B1=Bio_B1 %>%
#   select(TAG,Encounters)%>%
#   rename(B1_n = Encounters)
# 
# B2=Bio_B2 %>%
#   select(TAG,Encounters)%>%
#   rename(B2_n = Encounters)
# 
# ### Merge All Encounter Histories by antenna ###
# 
# # Merge only takes 2 values
# #RB
# ENC_RB= merge(RB1,RB2, all=TRUE)
# ENC_RB[is.na(ENC_RB)]=0
# #HP
# ENC_HP= merge(HP3,HP4, all=TRUE)
# ENC_HP[is.na(ENC_HP)]=0
# #CF
# ENC_CF= merge(CF5,CF6, all=TRUE)
# ENC_CF[is.na(ENC_CF)]=0
# #Mobile
# ENC_M1M2 = merge(M1,M2, all=TRUE)
# ENC_M1M2[is.na(ENC_M1M2)]=0
# #Biomark
# ENC_B1B2 = merge(B1,B2, all=TRUE)
# ENC_B1B2[is.na(ENC_B1B2)]=0
# 
# 
# # Merge RB HP
# ENC_RBHP= merge(ENC_RB,ENC_HP, all=TRUE)
# ENC_RBHP[is.na(ENC_RBHP)]=0
# 
# 
# # Merge RBHP with CF
# ENC_ALLStationary= merge(ENC_RBHP,ENC_CF, all=TRUE)
# ENC_ALLStationary[is.na(ENC_ALLStationary)]=0
# 
# 
# # Merge ENC_AllStationary with ENC_M1M2
# # was getting dupicate tag numbers at the very end bc I wasn't stripping the 900 from the TAG at the very beginning of the function for BIOmark and Mobile
# # so it wasn't merging correctly. Then the 900 was stripped later but by then it didn't make a dif
# ENC_Stationary_M1M2= merge(ENC_ALLStationary,ENC_M1M2, all=TRUE)
# ENC_Stationary_M1M2[is.na(ENC_Stationary_M1M2)]=0
# 
# # Merge ENC_AllStationary with ENC_B1B2
# #gets dataset of all encounters on all antennas
# ENC_ALL= merge(ENC_Stationary_M1M2,ENC_B1B2, all=TRUE)
# ENC_ALL[is.na(ENC_ALL)]=0
# 

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



# RECAPTURES --------------------------------------------------------------

#This section reads in recaptures, takes a df of all detection info sans release info, and the relase info datasheet
#it gets timstamps in order and corrects some column names in prep to combine
# it first binds_rows() between all_detecitons and release to get an Event called "Release" that begins the journey for the ish
# then binds_rows() on recaptures() to do the same for event "recaputre" 
# then it left joins release info so that the whole dataframe will have release info
# so now it's basically like the ALldetections df except now it includes recaptures.
# next: can replace All_detections df

# when saving csv from excel file in excel, make sure to specify TAG as a number otherwise it will try and save just the first part of TAG
# which is bullshit
recaps <- read.csv("WGFP_RecaptureData_Master.csv", colClasses = c(rep("character", 9), rep("numeric", 2), rep("character", 8)))
#recaps <- read_csv("WGFP_RecaptureData_Master.csv", col_select = c(-1),col_types = "cccccccccnncccccccc" )
#takes first column off bc for some reason there's a weird one going on
#recaps <- recaps[,-c(1, 14)]

recaps <- recaps  %>%
  select(-Num, -QAQC)

df_list <- WGFP_Encounter_FUN(Stationary = Stationary, Mobile = Mobile, Release= Release, Biomark = Biomark)

All_Detections_1 <- df_list$All_Detections

#wrangling release data times
#getting"12:00" to read 12:00:00
Release1 <- Release %>%
  rename(TAG = TagID) %>%
  mutate(TAG = str_trim(TAG),
         Event = str_trim(Event),
         Date = mdy(Date),
         Time1 = as_datetime(hm(Time)),
         Time2 = str_sub(Time1, start = 11, end = -1),
         DateTime = ymd_hms(paste(Date, Time2))) %>%
  select(RS_Num,River,ReleaseSite,Date,DateTime,UTM_X,UTM_Y,Species,Length,Weight,TAG,TagSize,Ant,Event) 

recaps1 <- Recaptures %>%
  rename(TAG = TagID) %>%
  mutate(TAG = str_trim(TAG),
         Event = str_trim(Event),
         Date = mdy(Date),
         Time1 = as_datetime(hm(Time)),
         Time2 = str_sub(Time1, start = 11, end = -1),
         DateTime = ymd_hms(paste(Date, Time2))) %>%
  select(RS_Num,River,RecaptureSite,DateTime,Date,Time2,UTM_X,UTM_Y,Species,Length,Weight,TAG,TagSize,Ant,Event) %>%
  rename(Time = Time2,
         Recap_Length = Length,
         Recap_Weight = Weight
         )

#df without release info
All_Detections_1 <- df_list$All_Detections

All_Detections_1_merge <- All_Detections_1 %>%
  mutate(Date = as.Date(Scan_Date)) %>%
  rename(
         DateTime = Scan_DateTime,
         Event = Site_Code) 

#gets a df with a event "Release"
#successfully puts 3436 rows onto the end 
all_detections_release <- bind_rows(All_Detections_1_merge, Release1)

# merge vs join; will want to do both 

detections_release_recaps <- bind_rows(all_detections_release, recaps1)

# x <- detections_release_recaps %>%
#   filter(Event %in% c("Release","Recapture and Release"))
# 
# y <- anti_join( Release1,x, by = "TAG")


#fills in release info so it is known at any row of detection
filled_in_release_rows <- left_join(detections_release_recaps, Release1, by = c("TAG"))


#this is the final df 

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
         UTM_Y = UTM_Y.x)


x <- filled_in_release_rows_condensed %>%
  filter(Event %in% c("Recapture and Release"))

#539086-538465 = 621: number of rows currently getting lost between condensed rows and df displaying in app
3436 - 2915
# detections_release_recaps1 <- detections_release_recaps %>%
#   select(Date, DateTime, TAG, Event, Species, Length, Weight, ReleaseSite, RecaptureSite, Recap_Length, Recap_Weight)

# detections_release_recaps11 <- detections_release_recaps1 %>%
#   filter(Event == "Release" | Event == "Recapture")
# 
# 
# x <- left_join(Release1, recaps1, by = "TAG")
# #DF wih just release and recaputres made to try and compare growth rates
# xx <- x %>%
#   select(TAG, DateTime.x, DateTime.y, Length, Weight, ReleaseSite, RecaptureSite, Recap_Length, Recap_Weight)

# x <- all_events %>%
#   filter(Event %in% c("Recapture")) %>%
#   distinct(TAG, .keep_all = TRUE)
# 
# #trying to figure out why what is being displayed in allevents tab in rshiny app is 500-600 rows diferent than what is made in the function
###SOLVED: my date range started at 2020-09-03 instead of 2020-09-01 when fish were first released

app_file <- read.csv("allevents.csv")

app_file1 <- app_file %>%
  mutate(TAG = as.character(TAG))



# differences <- anti_join(all_events, app_file1, by = "TAG")
# unique_tags <- data.frame(unique(differences$TAG))
# 
# unique_tags <- unique_tags %>%
#   rename(TAG = unique.differences.TAG.)
# #colnames(unique_tags)
# 
# #these are unique tags that don't show up in all_events in the shiny app
# x <- left_join(unique_tags, Release1, by = "TAG")
# 
# y <- left_join(x, all_events, by = "TAG")


### trying to see the dif rows from release to no relase

x <- Release1 %>%
  distinct(TAG, .keep_all = TRUE)

all_events1 <- all_events %>%
  filter(Event %in% c("Release", "Recapture and Release")) %>%
  distinct(TAG, .keep_all =TRUE)

#gets freq table of how much each one occurs
allevents1_table <- data.frame(table(all_events1$TAG))

Release1_table <- data.frame(table(Release1$TAG))

differences <- anti_join(all_events1, Release1, by = c("TAG","Event"))

#trying to see where differences are in main all events dataframe
testevents <- read_csv("allevents2.csv", col_types = cols(.default = "?", TAG = "c", UTM_X = "c", UTM_Y = "c"))
#c("TcccnncDc")
x <- anti_join(all_events, testevents)
                         
u_tags <- data.frame(unique(x$TAG))
events_list <- unique(all_events$Event)
species_list <- unique(all_events$Species)
release_site_list <- unique(all_events$ReleaseSite)

y <- x %>%
  filter(Datetime >= "2020-08-01" & Datetime <= "2021-12-14",
        Event %in% events_list,
        Species %in% species_list,
        ReleaseSite %in% release_site_list) %>%
  select(-Date)

all_events1 <- all_events %>%
  replace_na(list(Species = "No Info", ReleaseSite = "No Info"))

  #replace(is.na("Species"), "No Info")


all_detections12 <- df_list$All_Detections_Release

all_detections12 %>%
  filter(is.na(Species))

sort(unique(df_list$All_Events$ReleaseSite))

x <- df_list$ENC_Release2 %>%
  filter(TAG == "226001581749")

### incorporating recaps into enc_releases file
all_enc12 <- recaps_detections %>%
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


#### Merge Release data ###
Release1 <- Release %>%
  rename(TAG = TagID) %>%
  mutate(TAG = str_trim(TAG)) %>%
  replace_na(list(Species = "No Info", ReleaseSite = "No Info")) #replaced species and releasesite to follow the same convention as AllEvents

# was geting a massive dataframe because the Release df is called TAGid not TAG.
#need to actually join on full join not merge
ENC_Release <- full_join(Release1, ENC_ALL,  by = "TAG")
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
         B3 = (B3_n >0),
         B4 = (B4_n>0),
         Recapture = (Recap_n > 0))
  

#summary stats of each antenna encounter
#precariously built because Row numbers are used
#but also has release data
totalcols <- ncol(ENC_Release1)

ENC_Release2 <- ENC_Release1 %>%
  #counts number of TRUE across specified rows. negates subsequent lines of code -SG
  mutate(
    TotalEncounters = rowSums(ENC_Release1[(totalcols-10):totalcols] == TRUE),

    TotalAntennas1 = rowSums(ENC_Release1[(totalcols-10):totalcols-1] == TRUE),
         TotalStationary = rowSums(ENC_Release1[(totalcols-10):(totalcols-5)] == TRUE),
         TotalMobile = rowSums(ENC_Release1[(totalcols-4):(totalcols-3)] == TRUE),
         TotalBiomark = rowSums(ENC_Release1[(totalcols-2):totalcols-1] == TRUE),
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
  filter(!UTM_X %in% 0) #


# Condensed ENC_hist correct ----------------------------------------------
#224371

tag_only <- df_list$All_Events %>%
  filter(TAG %in% c("230000224371"))

#makes it so when you filter on distinct, you get the first and last events for each day and every distinct event in between

a1 <- tag_only %>%
  filter(!Event %in% "Release") %>%
  group_by(Date) %>%
  mutate(first_last = case_when(Datetime == min(Datetime) ~ "First_of_day",
                                Datetime == max(Datetime) ~ "Last_of_day",
                                Datetime != min(Datetime) & Datetime != max(Datetime) ~ "0")
         ) %>%
  ungroup()

a2 <- a1 %>%
  #group_by(min_datetTime, max_dateTime, Event) %>%
  distinct(Date, Event, TAG, first_last, .keep_all = TRUE)
# %>%
#   filter(min_datetTime = min(min))
  
# if (there are more than one antenna on one day ) {
#   display the first and last antennas that were hit that day
#   and any unique antennas in the middle of those
# }
  

x <- data.frame("Date" = c("2020-12-31", "2020-12-31", "2021-01-01", "2021-01-02", "2021-01-03", "2021-01-04"), "Event" = c("HP4", "HP3", "HP3", "RB2", "HP3", "HP3"))

# Timestamp QAQC ----------------------------------------------------------



library(plotly)

Markers_only <- Stationary %>%
  mutate(TAG = str_replace(str_trim(TAG), "\\_", "")) %>%
  filter(
        str_detect(TAG, "^0000000"), 
    #TTY %in% c("W"),
         #!TAG %in% c("900230000102751","900226001581072","900230000004000")
    )

### Subset Detection Type "Codes" to only include Summary (S) and Individual (I) ###
#WGFP_Clean= data.frame(WGFP_NoMarkers[which(WGFP_NoMarkers$Code == "I" | WGFP_NoMarkers$Code == "S"),])

#### Add Lat Longs to detections ###

# takes out 900 from TAG in WGFP Clean
# also takes out duplicate rows
Markers_only1 <- Markers_only %>%
  mutate(
    #TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG),
         DTY = ifelse(str_detect(DTY, "/"), 
                      as.character(mdy(DTY)), 
                      DTY))

Markers_only_test <- Markers_only1 %>%
  # filter(ARR <= hms::as_hms('12:00:00'),
  #        #ARR <= hms::as_hms('13:00:00'),
  #        #str_detect(ARR, c("AM|PM"))
  #        ) %>%
  mutate(Scan_Time1 = case_when(str_detect(ARR, "AM") & str_detect(ARR, "^12:") ~ hms(ARR) - hours(12),
                                str_detect(ARR, "PM") & str_detect(ARR, "^12:") ~ hms(ARR),
                                
                                str_detect(ARR, "AM") & str_detect(ARR, "^12:", negate = TRUE) ~ hms(ARR),
                                str_detect(ARR, "PM") & str_detect(ARR, "^12:", negate = TRUE) ~ hms(ARR) + hours(12),
                                #if it doesn't detect PM or AM just do hms(ARR)
                                str_detect(ARR, "PM|AM") == FALSE ~ hms(ARR)),
         Scan_Time2 = as.character(as_datetime(Scan_Time1)), 
         CleanARR = str_sub(Scan_Time2, start = 11, end = -1)
         )


y <- Markers_only_test %>%
  filter(TAG == "00000000000000004948") %>% #00000000000000004948 shows that it might have fallen off a bit
  #distinct(DTY, SCD, TAG, .keep_all = TRUE) %>%
  #filter(!Event %in% c("Release", "Recapture", "B3", "B4", "M1", "M2")) %>%
  ggplot(aes(x = DTY, y = Scan_Time3, color = SCD)) +
  geom_point() +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank())

y

ggplotly(y)

    # ARR2 = case_when(str_detect(ARR, "AM") ~ hms(ARR)))

# if (    length(unique( str_detect(Markers_only1$ARR, "PM|AM"))) > 1) {
#   Markers_only2 <- Markers_only1 %>%
#     mutate(Scan_Time1 = case_when(str_detect(ARR, "AM") ~ hms(ARR) ,
#                                   str_detect(ARR, "PM") ~ hms(ARR) + hours(12),
#                                   #if it doesn't detect PM or AM just do hms(Scan_Time)
#                                   str_detect(ARR, "PM|AM") == FALSE ~ hms(ARR))
#     ) %>%
#     mutate(Scan_Time2 = as.character(as_datetime(Scan_Time1)), 
#            Scan_Time3 = str_sub(Scan_Time2, start = 11, end = -1)) %>%
#     #select(Scan_Date, Scan_Time3, TAG, Site_Code, UTM_X, UTM_Y ) %>%
#     rename(Scan_Time = (Scan_Time3))
# }


s <- read.csv("WGFP_Raw_20211130 - Copy (2).csv")
s1 <- read.csv("WGFP_Raw_20210603.csv")

### all events
x <- All_events %>%
  
  distinct(Date, Event, TAG, .keep_all = TRUE) %>%
  filter(
    !Event %in% c("Release", "Recapture", "B3", "B4", "M1", "M2")
    #Event %in% c("RB2")
    
         ) %>%
  ggplot(aes(x = Date, y = Time, color = Event, text = TAG)) +
  geom_point() +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "All Events Scaterplot")
x
ggplotly(x)

### HOURS ONLY BAR PLOT

#pretty sure when I use distinct(), it is keeping only the first detection of that day for that fish.
#so if a fish sits on an antenna for a lot of the day, it's only going to show up during the first bit of the day? I think?
x <- All_events %>%
  #distinct(Date, Event, TAG, UTM_X, UTM_Y, .keep_all = TRUE) %>%
  filter(
    !Event %in% c("Release", "Recapture", "B3", "B4", "M1", "M2")
    #Event %in% c("RB2")

  ) %>%
  mutate(hour1 = hour(Datetime)) %>%
  ggplot(aes(x = hour1, fill = Event)) +
  geom_bar(stat = "Count") +
  theme_classic() +
  labs(title = "Hourly Events by Site")

x
ggplotly(x)

####### STATIONARY 

#before ARR is cleaned: 12 am comes up the same as 12 pm so there is a lot of detections in the 12 o clock range
x <- Stationary %>%
  distinct(DTY, SCD, TAG, .keep_all = TRUE) %>%
  filter(
    #!Event %in% c("Release", "Recapture", "B3", "B4", "M1", "M2")
    #Event %in% c("RB2")
    
  ) %>%
  ggplot(aes(x = DTY, y = ARR, color = SCD,  text = TAG)) +
  geom_point() +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) +
  labs(title = "Statoinary Only Scatterplot")
x
ggplotly(x)

##after Arr is cleaned
###after ARR is clean, if marker tags are still in stationary file and you use the distinct() function,
# you'll get a lot of detections in the 0-15 min range because that's the only time that day a marker tag will show up in that dataframe
cleanArr_Stationary <- Stationary %>%
  mutate(Scan_Time1 = case_when(str_detect(ARR, "AM") & str_detect(ARR, "^12:") ~ hms(ARR) - hours(12),
                                str_detect(ARR, "PM") & str_detect(ARR, "^12:") ~ hms(ARR),
                                
                                str_detect(ARR, "AM") & str_detect(ARR, "^12:", negate = TRUE) ~ hms(ARR),
                                str_detect(ARR, "PM") & str_detect(ARR, "^12:", negate = TRUE) ~ hms(ARR) + hours(12),
                                #if it doesn't detect PM or AM just do hms(ARR)
                                str_detect(ARR, "PM|AM") == FALSE ~ hms(ARR)),
         Scan_Time2 = as.character(as_datetime(Scan_Time1)), 
         CleanARR = str_trim(str_sub(Scan_Time2, start = 11, end = -1)),
         #hour = hour(CleanARR)
  )

###
x <- cleanArr_Stationary %>%
  #distinct(DTY, SCD, TAG, .keep_all = TRUE) %>%
  filter(
    #!Event %in% c("Release", "Recapture", "B3", "B4", "M1", "M2")
    SCD %in% c("RB2")
    
  ) %>%
  ggplot(aes(x = DTY, y = CleanARR, color = SCD,  text = TAG)) +
  geom_point() +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) +
  labs(title = "Statoinary Only Cleaned ARR scatterplot")
x
ggplotly(x)

####STATIONAY NO MARKER TAGS
## this should really be the key if ARR is behaving badly

cleanArr_NOMarkers_Stationary <- Stationary %>%
  mutate(Scan_Time1 = case_when(str_detect(ARR, "AM") & str_detect(ARR, "^12:") ~ hms(ARR) - hours(12),
                                str_detect(ARR, "PM") & str_detect(ARR, "^12:") ~ hms(ARR),
                                
                                str_detect(ARR, "AM") & str_detect(ARR, "^12:", negate = TRUE) ~ hms(ARR),
                                str_detect(ARR, "PM") & str_detect(ARR, "^12:", negate = TRUE) ~ hms(ARR) + hours(12),
                                #if it doesn't detect PM or AM just do hms(ARR)
                                str_detect(ARR, "PM|AM") == FALSE ~ hms(ARR)),
         Scan_Time2 = as.character(as_datetime(Scan_Time1)), 
         CleanARR = str_trim(str_sub(Scan_Time2, start = 11, end = -1)),
         
  ) %>%
  filter(str_detect(TAG, "^900"))

x <- cleanArr_NOMarkers_Stationary %>%
  distinct(DTY, SCD, TAG, .keep_all = TRUE) %>%
  filter(
    #!Event %in% c("Release", "Recapture", "B3", "B4", "M1", "M2")
    SCD %in% c("CF6")
    
  ) %>%
  ggplot(aes(x = DTY, y = CleanARR, color = SCD,  text = TAG)) +
  geom_point() +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    
    axis.ticks = element_blank()) +
  labs(title = "Stationary Only Cleaned ARR scatterplot")
x
ggplotly(x)


# mutate(TAG = str_replace(str_trim(TAG), "\\_", "")) %>%
#   filter(str_detect(TAG, "^900"), 

y <- s %>%
  #distinct(DTY, SCD, TAG, .keep_all = TRUE) %>%
  filter(
    #!Event %in% c("Release", "Recapture", "B3", "B4", "M1", "M2"),
         Event %in% c("RB1")
         ) %>%
  ggplot(aes(x = DTY, y = ARR, color = SCD)) +
  geom_point() +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())

y <- Stationary %>%
  filter(TAG == "0000_0000000000005394") %>%
  #distinct(DTY, SCD, TAG, .keep_all = TRUE) %>%
  #filter(!Event %in% c("Release", "Recapture", "B3", "B4", "M1", "M2")) %>%
  ggplot(aes(x = DTY, y = ARR, color = SCD)) +
  geom_point() +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank())

y



ggplotly(x) 

mutate(TAG = ifelse(str_detect(TAG, "^900"), str_sub(TAG, 4,-1), TAG),
       DTY = ifelse(str_detect(DTY, "/"), 
                    as.character(mdy(DTY)), 
                    DTY))
# Bad Timestamps between 12:00 and 1:00 -----------------------------------


### fixing bad timestamps between 12/10 and 1/30
#this is 
timestamp_problems <- Stationary %>%
  filter(DTY >= "2020-12-10" & DTY <= "2021-02-02", 
         #str_detect(ARR, c("AM","PM"))
         ) %>%
  mutate(ARR1 = case_when(str_detect(ARR, "AM") ~ hms(ARR) ,
                                str_detect(ARR, "PM") ~ hms(ARR) + hours(12),
                                #if it doesn't detect PM or AM just do hms(ARR)
                                str_detect(ARR, "PM|AM") == FALSE ~ hms(ARR))
  ) %>%
  mutate(ARR2 = as.character(as_datetime(ARR1)), 
         ARR3 = hms::as_hms(str_sub(ARR2, start = 11, end = -1))) %>%
  filter(
         ARR3 >= hms::as_hms('12:00:00'),
         ARR3 <= hms::as_hms('13:00:00'),
         str_detect(ARR, c("AM|PM"))
         ) %>%
  select(-c(ARR1, ARR2, ARR3))

##this gets a dataset without the problem times
#this is what I need to join to clean data

no_problems <- anti_join(Stationary, timestamp_problems)

#after combining .txt files in the app, bring that csv file in to combine with other dataset
# note: there was no RB1 data for 20210202; inly 1 entry in the .txt file
no_problem_times <- read.csv("no_problem_times.csv", colClasses= c(rep("character",10), "numeric"))

#remove ghost tag that was on RB
no_problem_times1 <- no_problem_times %>%
  filter(!TAG %in% c("900_230000228791"))

New_Stationary <- bind_rows(no_problems, no_problem_times1)


#shows that differences are actually no real detections
difs <- anti_join(stationary1, no_problem_times1, by = c("Code", "DTY"))

write_csv(New_Stationary, "New_Stationary.csv")


  
  #mutate(x11 = str_detect(ARR, c("AM|PM")))
  
  
  
 
  # select(-ARR) %>%
  # rename(ARR = ARR3)

#### Joining all_events with stations
All_events <- df_list$All_Events

EncounterHistory_AllData_wStations_20220107 <- read_csv("EncounterHistory_AllData_wStations_20220107.csv", 
                                                        col_types = cols(OBJECTID = col_skip(), 
                                                                         Join_Count = col_skip(), TARGET_FID = col_skip(), 
                                                                         TAG = col_character(), Release_Length = col_number(), 
                                                                         UTM_X = col_character(), UTM_Y = col_character(),
                                                                         Date_ = col_date(format = "%m/%d/%Y"),
                                                                         Release_Weight = col_number()))



stations <- EncounterHistory_AllData_wStations_20220107 %>%
  #select(Event, UTM_X, UTM_Y) %>%
  #mutate(Release_Date = mdy(str_sub(Release_Date, start = 1, end = -5))) %>% #
  rename(
    Date = Date_,
         Time = Time_) %>%
  distinct(Event, UTM_X, UTM_Y, TAG, .keep_all = TRUE)

#massive datafrmae occurs when there are multiple rows in B for which the key columns (same-name columns by default) match the same, single row in A
#usually this means you have to make sure you join by the fields which will not have any differenitation: iun this case, "TAG", UTM_X", "UTM_Y", and "Event". The other fields are just to help keep the dataframe more concise

all_events_stations_2 <- left_join(All_events, stations, by = c("TAG", "UTM_X", "UTM_Y", "Event")) # "Species", "Release_Length", "Release_Weight", "Event", "Date", "Time", "ReleaseSite", "Release_Date", "RecaptureSite", "Recap_Length", "Recap_Weight"


x <- all_events_stations_2 %>%
  mutate(ET_STATION = case_when(is.na(ET_STATION) & (Event %in% c("RB1", "RB2")) ~ 4300,
                                is.na(ET_STATION) & (Event %in% c("HP3", "HP4")) ~ 6340,
                                is.na(ET_STATION) & (Event %in% c("CF5", "CF6")) ~ 9550,
                                is.na(ET_STATION) & (Event %in% c("B3")) ~ 8290,
                                !is.na(ET_STATION) ~ ET_STATION)) %>%
  
  distinct(Datetime, Event, TAG, .keep_all =TRUE) %>%
  rename(
    
    Date = Date.x,
    Time = Time.x,
    
    Species = Species.x,
    Release_Length = Release_Length.x,
    Release_Weight = Release_Weight.x, 
    ReleaseSite = ReleaseSite.x,
    Release_Date = Release_Date.x,
    RecaptureSite = RecaptureSite.x,
    Recap_Length = Recap_Length.x,
    Recap_Weight = Recap_Weight.x
    
  ) %>%
  select(Date, Time, Datetime, TAG, Event, Species, Release_Length, Release_Weight, ReleaseSite, Release_Date, RecaptureSite, Recap_Length, Recap_Weight, UTM_X, UTM_Y, ET_STATION)
  
#all_events_stations <- left_join(All_events, EncounterHistory_AllData_wStations_20220107, by = c("UTM_X", "UTM_Y", "Species", "Release_Length", "Release_Weight", "Event"))



x <- EncounterHistory_AllData_wStations_20220107 %>%
  filter(Event %in% c("RB1","RB2", "HP3", "HP4", "CF5", "CF6", "B3", "B4")) %>%
  distinct(Event, UTM_X, UTM_Y, .keep_all = TRUE) %>%
  select(Event, UTM_X, UTM_Y)
  
# y <- all_events_stations_2 %>%
#   filter(TAG %in% c("230000224079"),
#          Event.x %in% c("M1", "M2"))
x <- split(All_events, f=All_events$UTM_X)  

unique_utmx_allevents = data.frame(UTM_X = unique(All_events$UTM_X)  )
  
unique_utmy_allevents = data.frame(UTM_Y = unique(All_events$UTM_Y)  )

stations_x <- data.frame(UTM_X = unique(EncounterHistory_AllData_wStations_20220107$UTM_X))
stations_y <- data.frame(UTM_Y = unique(EncounterHistory_AllData_wStations_20220107$UTM_Y))

difs_x <- anti_join(stations_x, unique_utmx_allevents, by = "UTM_X")   

test <- left_join(stations_x, unique_utmx_allevents)

r11 <- r1 %>%
  filter(sum_dist > 0) %>%
  distinct(TAG, .keep_all = TRUE)

r11 %>%
  ggplot(aes(x = sum_dist)) +
  geom_bar(stat = "count") +
  theme_classic()

write_csv(r14, "older_movement NA values to look at.csv")

#####

r1 <- statesdf_list$Movements

cf5 <- read.csv("WGFP_Raw_20220110_cf5.csv")
cf6 <- read.csv("WGFP_Raw_20220110_cf6.csv")
x <- anti_join(cf5, cf6)

x1 <- cf6 %>%
  filter(is.na(DTY))

movements_df1 <- Movements_df %>%
  filter(
    !is.na(movement_only),
    !movement_only %in% c("No Movement", "Initial Release"),
         #Date >= input$slider1[1] & Date <= input$slider1[2]
         ) %>%
  mutate(weeks_since = as.numeric(ceiling(difftime(Date, min(Date), units = "weeks"))),
         Date1 = as.character(Date)
         )


   
plot <- movements_df1 %>%
  ggplot(aes(x = Date, fill = movement_only,
             text = paste('Date: ', as.character(Date), '\n'))
         ) +
  geom_bar(stat = "count", position = "dodge") +
  theme_classic() +
  labs(title="Fish Movement by Day",
       x ="Date", y = "Count") +
  scale_fill_manual(values = c("Downstream Movement" = "red",
                               "Upstream Movement" = "chartreuse3",
                               "No Movement" = "black",
                               "Initial Release" = "darkorange"))
plot + 
  geom_bar
plotly1 <- ggplotly(p = plot)
#?ggplotly
plotly1

####release events counted
releases_moves <- Movements_df %>%
  ungroup() %>%
  count(Date, movement_only, name = "Num") %>%
  mutate(Initial1 = case_when(movement_only == "Initial Release" ~ Num,
         TRUE ~ as.integer(0)))
  #separate(movement_only, c("Initial Release", "Movement1"))
  # filter(
  #   !is.na(movement_only),
  #   movement_only %in% c("Initial Release"))

plot <- releases_moves %>%
  # filter(
  #   !movement_only %in% c("Initial Release")
  # )
  ggplot(aes(x = Date, y = Num, fill = movement_only,
             text = paste('Date: ', as.character(Date), '\n'))
  ) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic() +
  labs(title="Fish Movement by Day",
       x ="Date", y = "Count") +
  scale_fill_manual(values = c("Downstream Movement" = "red",
                               "Upstream Movement" = "chartreuse3",
                               "No Movement" = "black",
                               "Initial Release" = "darkorange"))
plot$data <- plot$data %>%
  filter(
    movement_only %in% c("Initial Release")
  )

plot +
  geom_vline(aes(xintercept = Date))
#   geom_bar(aes(y = Initial1), stat = )
# fig <- releases %>%
#   ggplot(aes(x = Date)) +
#   geom_bar(stat = "count", position = "dodge") +
#   theme_classic() +
#   scale_y_reverse()

fig <- plot_ly(releases, x = ~Date, y = ~Num, type = "bar")
fig
### geom_vline
p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
p + geom_vline(xintercept = 5)
p + geom_vline(xintercept = 1:5)
p + geom_vline(xintercept = 1:5, colour="green", linetype = "longdash")
p
p + geom_vline(aes(xintercept = wt))

# With coordinate transforms
p + geom_vline(aes(xintercept = wt)) + coord_equal()
p + geom_vline(aes(xintercept = wt)) + coord_flip()
p + geom_vline(aes(xintercept = wt)) + coord_polar()

p2 <- p + aes(colour = factor(cyl))
p2 + geom_vline(xintercept = 15)

# To display different lines in different facets, you need to
# create a data frame.
p <- qplot(mpg, wt, data=mtcars, facets = vs ~ am)
vline.data <- data.frame(z = c(15, 20, 25, 30), vs = c(0, 0, 1, 1), am = c(0, 1, 0, 1))
p + geom_vline(aes(xintercept = z), vline.data)

plotly2 <- plotly1 %>%
  #hovertemplate = 
  # #hover
  layout(
    #hovermode = "x unified"
  #   hoverinfo = 'text',
     hovertext = "y" #paste("Date"),
    # xaxis = list(
    # type = "date",
    # tickformat = "%Y %m %d"
  )
  

plotly2
#library(plotly)


oregon_rfid <- new_pit(data = "oregon_rfid", test_tags = NULL, print_to_file = FALSE, time_zone = "America/Vancouver")


x <- r2 %>%
  group_by(State) %>%
  tally()

qDat <- quakes
qDat$id <- seq.int(nrow(qDat))

# Release L W plot --------------------------------------------------------

plot3 <- Release %>%
  ggplot(aes(x = Length, y = Weight, color = Species)) +
  geom_point() + 
  theme_classic() +
  labs(title = "Length/Weight Plot for Release Data")

ggplotly(plot3)


hms::hms((min(All_events$Time)))
x <- All_events %>%
  # mutate(Time1 =
  #          #strptime(str_trim(Time), format = "%H:%M:%S"),
  #          hour(Datetime),
  #          #hms::hms(as.numeric(str_trim(Time)))
  #        #Time2 = as.POSIXct(Time1)
  #        ) %>%
  group_by(Date, TAG) %>% 
  mutate(first_last = case_when(Datetime == min(Datetime) ~ "First_of_day",
                                Datetime == max(Datetime) ~ "Last_of_day",
                                Datetime != min(Datetime) & Datetime != max(Datetime) ~ "0")
  ) %>%
  ungroup() %>%
  distinct(TAG, Event, Date, first_last,  UTM_X, UTM_Y, .keep_all = TRUE) %>%
  select(-first_last) 
  #filter(hour(Datetime) >= " 12:01:00" & Time <= " 23:59:59")



allevents_2022_01_10 <- read_csv("allevents_2022-01-10.csv", 
                                 col_types = cols(TAG = col_character(), 
                                                  Time = col_character(),
                                                  UTM_X = col_character(), UTM_Y = col_character()))
#rows in A that don't have a match in b
z2 <- anti_join(x,allevents_2022_01_10, by = "Datetime") # by = c("TAG", "Event", "Date", "UTM_X", "UTM_Y"))
test <- left_join(z2, z1, by = c("Date", "TAG", "Event", "UTM_X", "UTM_Y"))
test1 <- test %>%
  select(Datetime.x, Datetime.y,Time.x, Time.y,  1:ncol(test))
max(x$Time1)
#library(data.table)

x1 <- All_events %>%
  ggplot(aes(x= Date, fill = Event, 
             #text = TAG
             )) +
  geom_bar(stat = "count", position = "dodge") +
  theme_classic() +
  labs(title = "Raw Detections Frequency")
  

ggplotly(x1)


x <- Movements_df %>%
  #group_by(TAG) %>%
  filter(
    TAG 
    
    # det_type %in% c("Red Barn Stationary Antenna","Confluence Stationary Antenna") 
    #      & det_type %in% c("Confluence Stationary Antenna")
         ) 

###Filter for above+below the dam
ENC_Release2 <- df_list$ENC_Release2

# ENC_Release3 <- ENC_Release2 %>%
#   mutate(Abov_below = case_when(
#     (RB1|RB2|HP3|HP4|B3) == TRUE & (CF5|CF6|B4) == TRUE ~ "Above and Below the Dam",
#     (RB1|RB2|HP3|HP4|B3) == TRUE & (CF5&CF6&B4) == FALSE ~ "Below Dam Only",
#                                 (CF5|CF6|B4) == TRUE ~ "Above Dam Only"))


x <- All_events_days1 %>%
  count(TAG, det_type, above_below, name = "Encounters") %>%
  mutate(combined_event = paste(det_type, above_below),
         EncountersTF = ifelse(Encounters > 0, 
                               TRUE,
                               FALSE))

x1 <- pivot_wider(data = x, id_cols = TAG, names_from = combined_event, values_from = EncountersTF)
x2 <- x1 %>%
  select(TAG, `Release Above the Dam`,`Release Below the Dam`,`Recapture Above the Dam`,`Recapture Below the Dam`,`Recapture and Release Above the Dam`,`Recapture and Release Below the Dam`, `Mobile Run Above the Dam`, `Mobile Run Below the Dam`)
  
x2[is.na(x2)] = FALSE
#x2[x2 > 0] = TRUE

x3 <- left_join(ENC_Release2, x2, by = "TAG")

x4 <- x3 %>%
  mutate(through_dam = case_when(
    (RB1|RB2|HP3|HP4|B3|`Release Below the Dam`|`Recapture Below the Dam`|`Recapture and Release Below the Dam`|`Mobile Run Below the Dam`) == TRUE & (CF5|CF6|B4|`Release Above the Dam`|`Recapture Above the Dam`|`Recapture and Release Above the Dam`|`Mobile Run Above the Dam`) == TRUE ~ "Went through dam",
    (RB1|RB2|HP3|HP4|B3|`Release Below the Dam`|`Recapture Below the Dam`|`Recapture and Release Below the Dam`|`Mobile Run Below the Dam`) == TRUE & (CF5&CF6&B4&`Release Above the Dam`&`Recapture Above the Dam`&`Recapture and Release Above the Dam`&`Mobile Run Above the Dam`) == FALSE ~ "Stayed Below the Dam",
    (RB1&RB2&HP3&HP4&B3&`Release Below the Dam`&`Recapture Below the Dam`&`Recapture and Release Below the Dam`&`Mobile Run Below the Dam`) == FALSE & (CF5|CF6|B4|`Release Above the Dam`|`Recapture Above the Dam`|`Recapture and Release Above the Dam`|`Mobile Run Above the Dam`) == TRUE ~ "Stayed Above the Dam",
    
  )) %>%
  select(TAG, through_dam)

###make column for release and recapture below the dam
#make values either TRUE or FALSE
#do logic for making row of "did it get through the dam"

min(All_events$Release_Length, na.rm = TRUE)


x <- movement_table_notrans %>%
  distinct(TAG, .keep_all = TRUE)

x1 <- x %>%
  ggplot(aes(x = sum_dist, text = unique(TAG))) +
  geom_histogram(
    #binwidth = 30
  ) +
  theme_classic()
x1
ggplotly(x1)
