the_whole_shibang <- function(city = "Cape Town"){
  
  #scrape and store
  activity <- activityScrape(city)
  food <- foodScrape(city)

  #make databases
  db <- generate_database(city)
  write_db_tables(db, activity, food, city)
  
  #make distance matrix 
  DM <- generate_dist_matrix(activity, food)
  store_dist_matrix(DM)
  
  #disconnect from database
  dbDisconnect(db)
  
}


