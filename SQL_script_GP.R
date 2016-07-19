create_dataBase <- function(event, food, activity) {
  library(RSQLite)
  dB <- dbConnect(SQLite(), "", cache_size = 5000, synchronous = "full")
  dbWriteTable(dB, "event", event)
  dbWriteTable(dB, "activity", activity)
  dbWriteTable(dB, "food", food)
}


db <- dbConnect(SQLite(), "escapeDB.sqlite")

dbIsValid(db)


dbListFields(db, "activity")

dbSendQuery(db, "INSERT into activity VALUES (1, \"test\", \"testDes\")")


dbSendQuery(db, "INSERT into activity VALUES (c(1, 2, 3), c(\"test1\", \"test2\", \"test3\"), c(\"test1\", \"test2\", \"test3\")"


test <- dbReadTable(db, "activity")

dbDisconnect(con)

dbDisconnect(db)