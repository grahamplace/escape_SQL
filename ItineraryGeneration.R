#* @post /hello
#' @param subtypes a character vector of subtypes
#' @param long numeric of the user's current longitude
#' @param lat numeric of the user's current latitude
#' @return a data frame that has the user's itinerary
#' @import RSQLite
#' @import lubridate
#' @import geosphere
#' @import dplyr
#' @import plumber
generator <- function(long, lat, subtypes = c("entertainment", "museum", "outdoors", "nightlife", "shopping")) {
  
  # Call libraries
  library(RSQLite)
  library(lubridate)
  library(geosphere)
  library(dplyr)
  library(plumber)
  
  # Connect to server
  con <- dbConnect(SQLite(), "escapeDB.sqlite")
  
  # Read in food and activity tables
  food <- dbReadTable(con, "food")
  activity <- dbReadTable(con, "activity")
  
  # Initialize variables: starttime, nextmeal, endday, itinerary data frame, cuisines tasted, activities completed, activities left
  starttime <- 10 #ifelse(hour(Sys.time()) < 10, 10, hour(Sys.time()))
  nextmeal <- 13
  endday <- 24
  itinerary <- data.frame(Event = character(),
                          Tag = character(),
                          StartTime = numeric(),
                          EndTime = numeric(),
                          Price = numeric(),
                          Popularity = numeric(),
                          Latitude = numeric(),
                          Longitude = numeric(),
                          OpeningHours = numeric(),
                          ClosingHours = numeric(),
                          EventLength = numeric(),
                          Description = character())
  cuisines <- character()
  activities <- character()
  activitiesleft <- setdiff(subtypes, activities)
  
  # Select breakfast
  breakfast <- foodselection(cuisines, "breakfast", starttime, lat, long)
  itinerary <- rbind(itinerary, breakfast[[1]])
  starttime <- breakfast[[2]]
  lat <- breakfast[[3]]
  long <- breakfast[[4]]
  cuisines <- breakfast[[5]]
  nextmeal <- starttime + 4
  
  # After breakfast and before the end of the day, the itinerary self-generates between meal and activity based on time of day.
  while(starttime < endday) {
    # Decide if another activity is next or a meal is next
    if(starttime >= nextmeal){
      # Decide next meal - lunch or dinner
      if(starttime < 16){
        lunch <- foodselection(cuisines, "lunch", starttime, lat, long)
        itinerary <- rbind(itinerary, lunch[[1]])
        starttime <- lunch[[2]]
        lat <- lunch[[3]]
        long <- lunch[[4]]
        cuisines <- lunch[[5]]
        nextmeal <- starttime + 6
      }
      else {
        dinner <- foodselection(cuisines, "dinner", starttime, lat, long)
        itinerary <- rbind(itinerary, dinner[[1]])
        starttime <- dinner[[2]]
        lat <- dinner[[3]]
        long <- dinner[[4]]
        cuisines <- dinner[[5]]
        nextmeal <- 24
      }
    }
    else {
      # Next activity
      if(length(activitiesleft) > 0){
        activity <- activityselection(activities, sample(activitiesleft, 1),  starttime, nextmeal, lat, long)
      }
      else {
        activity <- activityselection(activities, sample(subtypes, 1), starttime, nextmeal, lat, long)
      }
      itinerary <- rbind(itinerary, activity1[[1]])
      starttime <- activity1[[2]]
      lat <- activity1[[3]]
      long <- activity1[[4]]
      activities <- activity1[[5]]
      activitiesleft <- setdiff(activities, subtypes)
    }
  }
  dbDisconnect(con)
  return(itinerary)
}


# Function to calculate longitude range
longitude_range <- function(coord) {
  p1 <- destPoint(p=coord, b=0, d = 16000)[2]
  long_range <- p1 - coord[2]
  return(long_range)
}

# Variable of latitude range
latitude_range <- 0.01449275362

# Function to select dining
foodselection <- function(cuisines, subtypename, starttime, lat, long) {
  day <- as.POSIXlt(Sys.Date())$wday
  pop <- sample(1:3, 1)
  events <- food %>% filter(subtype == subtypename & 
                            popularity == pop &
                            latitude < (lat + latitude_range) & 
                            latitude > (lat - latitude_range) &
                            longitude < (long + longitude_range(c(lat, long))) & 
                            longitude > (long - longitude_range(c(lat, long))) &
                            !(cuisine %in% cuisines) &
                            if(day == 0) {starttime >= openSun & (starttime + eventlength) <= closeSun}
                            else if(day == 6) {starttime >= openSat & (starttime + eventlength) <= closeSat}
                            else {starttime >= openWeek & (starttime + eventlength) <= closeWeek})
  current <- events[sample(length(events), 1),]
  cuisines <- c(cuisines, current$cuisine)
  start <- starttime
  end <- start + current$eventlength
  lat <- current$latitude
  long <- current$longitude
  if (day == 0) {
    open = current$openSun
    close = current$closeSun
  }
  else if (day == 6) {
    open = current$openSat
    close = current$closeSat
  }
  else {
    open = current$openWeek
    close = current$closeWeek
  }
  
  list((data.frame(Events = current$eventname,
             Tag = current$subtype,
             StartTime = start,
             EndTime = end,
             Price = current$price,
             Popularity = current$popularity,
             Latitude = lat,
             Longitude = long,
             OpeningHours = open,
             ClosingHours = close,
             EventLength = current$eventlength,
             Description = current$cuisine)),
       end,
       lat,
       long,
       cuisines)
}

# Function to select activity
activityselection <- function(activities, subtypename, starttime, mealtime, lat, long) {
  day <- as.POSIXlt(Sys.Date())$wday
  pop <- sample(1:3, 1)
  events <- activity %>% filter(subtype == subtypename & 
                                  popularity == pop &
                                  latitude < (lat + latitude_range) & 
                                  latitude > (lat - latitude_range) &
                                  longitude < (long + longitude_range(c(lat, long))) & 
                                  longitude > (long - longitude_range(c(lat, long))) &
                                  starttime + eventlength <= mealtime &
                                  (if(day == 0) {starttime >= openSun & (starttime + eventlength) <= closeSun}
                                  else if(day == 1) {starttime >= openSat & (starttime + eventlength) <= closeSat}
                                  else {starttime >= openWeek & (starttime + eventlength) <= closeWeek}))
  current <- events[sample(length(events), 1),]
  activities <- c(activities, current$subtype)
  start <- starttime
  end <- start + current$eventlength
  lat <- current$latitude
  long <- current$longitude
  if (day == 0) {
    open = current$openSun
    close = current$closeSun
  }
  else if (day == 6) {
    open = current$openSat
    close = current$closeSat
  }
  else {
    open = current$openWeek
    close = current$closeWeek
  }
  
  list((data.frame(Events = current$eventname,
                   Tag = current$subtype,
                   StartTime = start,
                   EndTime = end,
                   Price = current$price,
                   Popularity = current$popularity,
                   Latitude = lat,
                   Longitude = long,
                   OpeningHours = open,
                   ClosingHours = close,
                   EventLength = current$eventlength,
                   Description = current$description)),
       end,
       lat,
       long,
       activities)
}

