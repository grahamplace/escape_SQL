#function to add scraped dataframes as SQL tables in database
add_db_tables <- function(food, activity){
  
  #establish connection to database
  db <- dbConnect(SQLite(), "escapeDB.sqlite", cache_size = 5000, synchronous = "full")
  
  #check if connection was correctly established
  if(dbIsValid(db)){
    
    #check that activity table exists
    if(dbExistsTable(db, "activity")){
      #read current table to check its status
      checkActivity <- dbReadTable(db, "activity")
      
      #if the table is empty, all we need to do is append dataframe into table, keeping SQL types correct.
      if(dim(checkActivity)[1] == 0){
        dbWriteTable(db, "activity", activity, append = TRUE)
      }
      #otherwise, overwrite, don't append
      else {
        dbWriteTable(db, "activity", activity, overwrite = TRUE)
      }
    }
  }
  else {
    
  }
  
  dbDisconnect(db)
}