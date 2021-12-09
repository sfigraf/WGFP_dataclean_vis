library(tidyverse)
library(readxl)
library(lubridate)
library(fishualize)
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

All_detections <- df_list$All_Detections
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
All_detections_05 <- All_detections %>%
  mutate(weeks_since = as.numeric(ceiling(difftime(Scan_Date, min(Scan_Date), units = "weeks")))
         )

#unique tags by site and Day

# don't need this part anymore since all_detections now contains release data
# spc_enc_only <- Enc_release_data %>%
#   select(Species, Length, Weight, TAG)
# 
# detections_and_species <- left_join(All_detections_05, spc_enc_only, by = "TAG")

All_detections_05 %>%
  count(Site_Code, Species, weeks_since) %>%
  ggplot(aes(x = weeks_since, y = n, fill = Site_Code)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Weekly detections by Site")

All_detections_05 %>%
  count(Site_Code, Species) %>%
  ggplot(aes(x = Species, y = n, fill = Site_Code)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(title = "Raw Number of detections by Species and Site")

All_detections_05 %>%
  distinct(TAG, Site_Code, .keep_all = TRUE) %>%
  count(Site_Code,Species) %>%
  ggplot(aes(x = Site_Code, y = n, fill = Species)) +
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
All_detections_05 <- All_detections %>%
  mutate(weeks_since = as.numeric(ceiling(difftime(Scan_Date, min(Scan_Date), units = "weeks")))
  )

All_detections1 <- All_detections_05 %>%
  distinct(TAG, Site_Code, weeks_since, .keep_all = TRUE)

test_weeks <- pivot_wider(data = All_detections1, id_cols = TAG, names_from = weeks_since, values_from = Site_Code)

## Days
All_detections_days <- All_detections %>%
  mutate(days_since = as.numeric(ceiling(difftime(Scan_Date, min(Scan_Date), units = "days")))
  )

All_detections_days1 <- All_detections_days %>%
  distinct(TAG, Site_Code, days_since, .keep_all = TRUE)

test_days <- pivot_wider(data = All_detections_days1, id_cols = TAG, names_from = days_since, values_from = Site_Code)

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
