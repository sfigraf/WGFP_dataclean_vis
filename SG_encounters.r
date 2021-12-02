library(tidyverse)
library(readxl)
library(lubridate)
#library(threadr) #needed for period to string function



#Stationary = read.csv(paste0("WGFP_Raw_20211109.csv"), colClasses = )
Stationary = read.csv(paste0("WGFP_Raw_20211130.csv"))
#Stationary11 = read.csv(paste0("WGFP_Raw_20211122_1.csv"), colClasses = c(rep("character",11)))

# Read mobile antenna detections
Mobile = read.csv("WGFP_MobileDetections.csv", colClasses=c(rep("character",10)))

#Read Biomark
# need to be put in with decimal registering as "," because otherwise it won't bring in the full DEC.Id tag
#biomark1 <- read_csv("Biomark_Raw_20211109.csv", col_types = "cccccccccccccccc")
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
# x <- WGFP_Clean %>%
#   filter(TAG == 900230000228822)
#   #filter(between(Scan_Date, "2020-10-16", "2021-04-04"))
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

All_detections <- df_list$All_Detections
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
All_detections <- All_detections %>%
  mutate(weeks_since = as.numeric(ceiling(difftime(Scan_Date, min(Scan_Date), units = "weeks")))
         )

All_detections1 <- All_detections %>%
  distinct(TAG, Site_Code, weeks_since, .keep_all = TRUE)

test_weeks <- pivot_wider(data = All_detections1, id_cols = TAG, names_from = weeks_since, values_from = Site_Code)

## Days
All_detections_days <- All_detections %>%
  mutate(days_since = as.numeric(ceiling(difftime(Scan_Date, min(Scan_Date), units = "days")))
  )

All_detections_days1 <- All_detections_days %>%
  distinct(TAG, Site_Code, days_since, .keep_all = TRUE)

test_days <- pivot_wider(data = All_detections_days1, id_cols = TAG, names_from = days_since, values_from = Site_Code)

### Getting times correct

x <- Biomark$Scan.Time[71755]
y <- mdy(x)
str_length(x)
str_detect(x, "/")
# if the string sonctains //
# Stationary1 <- Stationary %>%
#   filter(TAG == "900_230000228791")
#   #mutate(ARR1 = hms(ARR))


Stationary12 <- Stationary11 %>%
  filter(
    # DTY >= as.Date("2021-03-02") & DTY <= as.Date("2021-04-06"),
    #      SCD == "RB1",
         TAG != "900_230000228791")
         #str_length(ARR) <8)

#this is the same as the filter right now in WGFP function
WGFP_NoMarkers_1 <- Stationary %>%
  mutate(TAG = str_replace(TAG, "\\_", "")) %>%
  filter(str_detect(TAG, "^900"), 
         !TAG %in% c("900230000102751","900226001581072","900230000004000"))
           # Stationary$TAG !=  &
           # Stationary$TAG !=  &
           # Stationary$TAG !=  &
           #this one is the ghost tag removed 4/6 from RB1
           # (Stationary$TAG != "900230000228791" | DTY <= as.Date("2021-12-01"))
  

WGFP_NoMarkers_11 <- Stationary11 %>%
  mutate(TAG = str_replace(TAG, "\\_", "")) %>%
  filter(str_detect(TAG, "^900"), 
         !TAG %in% c("900230000102751","900226001581072","900230000004000"),
         # Stationary$TAG !=  &
         # Stationary$TAG !=  &
         # Stationary$TAG !=  &
         #this one is the ghost tag removed 4/6 from RB1
         (Stationary$TAG != "900230000228791" | DTY <= as.Date("2021-03-02"))
  )


#see which rows in x are different from those in Y
diferences <- anti_join(WGFP_NoMarkers, WGFP_NoMarkers_1)

problem_times <- Stationary %>%
  filter(str_length(ARR) < 8) %>%
  mutate(month111 = month(DTY)) %>%
  distinct(month111, SCD,  .keep_all = TRUE)

no_problem_times <- Stationary %>%
  filter(str_length(ARR) >= 8) 


write_csv(Stationary1, "WGFP_Raw_20211122.csv")
# there was a problem with the Timestamps used in the files where 791 (the ghost tag) was present and also in the APril detections. So this part and above
# is a 1 time solution to figure that all out
new_times <- read.csv("new_times.csv", colClasses = c(rep("character",11)))
new_times1 <- new_times %>%
  filter(!TAG %in% c("900_230000228791"))

new_Stationary <- bind_rows(new_times1, no_problem_times)

diferences <- anti_join(x, Stationary)

write_csv(new_Stationary, "New_Stationary.csv")

# 
#   mutate(month111 = month(DTY)) %>%
#   distinct(month111, SCD, DTY, .keep_all = TRUE)

new_Stationary <- read.csv("New_Stationary.csv", colClasses = c(rep("character",11)))
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
