
# the EVENT field is not super helpful here; for most it doesn't matter if it hit RB1 or RB2; can be misleading; maybe omit it? same idea for states DF

get_movements_function <- function(combined_events_stations) {
  start_time <- Sys.time()
  
  
  movement_table_notrans <- combined_events_stations %>%
    select(Date, Datetime, TAG, det_type, Event, ET_STATION,  ReleaseSite, Release_Date, RecaptureSite, River, UTM_X, UTM_Y) %>%
    group_by(TAG) %>%
    mutate(dist_moved = ET_STATION - lag(ET_STATION, order_by = Datetime),
           sum_dist = (sum(abs(diff(ET_STATION)))),
           
           
           movement_only = case_when(Event %in% c("Release", "Recapture and Release")  ~ "Initial Release",
                                     dist_moved == 0 ~ "No Movement",
                                     dist_moved > 0 ~ "Upstream Movement",
                                     dist_moved < 0 ~ "Downstream Movement"),
           #this is for mapping later on
           marker_color = case_when(movement_only == "No Movement" ~ "black",
                                    movement_only == "Upstream Movement" ~ "green",
                                    movement_only == "Downstream Movement" ~ "red",
                                    movement_only == "Initial Release" ~ "blue"
                                    #str_detect(movement_only, "Initial Release (or recapture and release)") ~ "yellow"
                                    ),
           
           icon_color = case_when(str_detect(det_type, "Stationary Antenna") ~ "orange",
                                  str_detect(det_type, "Biomark Antenna") ~ "yellow",
                                  str_detect(det_type, "Mobile Run") ~ "purple",
                                  det_type %in% c("Release", "Recapture and Release") ~ "blue",
                                  det_type == "Recapture" ~ "brown",
           ),
           X = as.numeric(UTM_X),
           Y = as.numeric(UTM_Y)
    ) #end of mutate
    
  
  attr(movement_table_notrans, "zone") = "13"
  attr(movement_table_notrans, "projection") = "UTM"
  attr(movement_table_notrans, "datum") = "GRS80"
  
  # need a column that has x and Y for this 
  movement_table_notrans <- convUL(movement_table_notrans, km=FALSE, southern=NULL)
  movement_table_notrans1 <- movement_table_notrans %>%
    select(Date, Datetime, TAG, det_type, movement_only, dist_moved, ET_STATION,  ReleaseSite, Release_Date, RecaptureSite, River, UTM_X, UTM_Y, X, Y, Event, sum_dist, marker_color, icon_color)
  
  
  
  end_time <- Sys.time()
  print(paste("Movements Function took", round(end_time-start_time,2)))
  
  return(movement_table_notrans1)
}

#mvts <- get_movements_function(combined_events_stations)
