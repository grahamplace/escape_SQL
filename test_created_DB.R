library(RSQLite)

#conncet to database 
db <- dbConnect(SQLite(), "escapeDB.sqlite", cache_size = 5000, synchronous = "full")

#check connection
dbIsValid(db)

#check fields of db tables 
dbListFields(db, "activity")
dbListFields(db, "food")

#should throw error, no longer using event table 
dbListFields(db, "event")

#read in tables to check out
activity <- dbReadTable(db, "activity")
food <- dbReadTable(db, "food")

#check class of each table's variables
sapply(activity[,1:length(activity)], FUN = function(x) {class(x)})
sapply(food[,1:length(food)], FUN = function(x) {class(x)})

#try updating tables in R and writing to tables already in db 
testAct <- as.data.frame(read_csv("activity.csv"))

dbWriteTable(db, "activitytest", testAct)
dbListFields(db, "activitytest")
activitytest <- dbReadTable(db, "activitytest")
sapply(activitytest[,1:length(activitytest)], FUN = function(x) {class(x)})
