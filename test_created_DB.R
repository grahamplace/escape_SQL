library(RSQLite)

db <- dbConnect(SQLite(), "escapeDB.sqlite")
dbIsValid(db)
dbListFields(db, "activity")
dbListFields(db, "food")
dbListFields(db, "event")

