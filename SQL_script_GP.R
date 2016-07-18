create_dataBase <- function(event, food, activity) {
  library(RSQLite)
  dB <- dbConnect(SQLite(), "", cache_size = 5000, synchronous = "full")
  dbWriteTable(dB, "event", event)
  dbWriteTable(dB, "activity", activity)
  dbWriteTable(dB, "food", food)
}
