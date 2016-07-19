library(RSQLite)

#read in scraped csv files
activity <- read.csv("activity.csv")
food <- read.csv("food.csv")

#establish connection to database
db <- dbConnect(SQLite(), "escapeDB.sqlite")

#write activity (w/ append not overwrite)
dbWriteTable(db, "activity", activity, append = TRUE)

#write activity (w/ append not overwrite)
dbWriteTable(db, "food", food, append = TRUE)

checkA <- dbReadTable(db, "activity")
checkF <-  dbReadTable(db, "food")

dbDisconnect(db)
