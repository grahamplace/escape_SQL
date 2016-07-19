library(RSQLite)

#conncet to database 
db <- dbConnect(SQLite(), "escapeDB.sqlite")

#check connection
dbIsValid(db)

#check fields of db tables 
dbListFields(db, "activity")
dbListFields(db, "food")

#should throw error, no longer using event table 
dbListFields(db, "event")

#read in tables to check out: 
activity <- dbReadTable(db, "activity")
food <- dbReadTable(db, "food")


