dist <- function(id1, id2) {
  if(id1 %% 2 == 0){
    lat1 <- activity$latitude[which(activity$eventid == id1)]
    long1 <- activity$longitude[which(activity$eventid == id1)]
  }
  else {
    lat1 <- food$latitude[which(food$eventid == id1)]
    long1 <- food$longitude[which(food$eventid == id1)]
  }
  
  if(id2 %% 2 == 0){
    lat2 <- activity$latitude[which(activity$eventid == id2)]
    long2 <- activity$longitude[which(activity$eventid == id2)]
  }
  
  else {
    lat2 <- food$latitude[which(food$eventid == id2)]
    long2 <- food$longitude[which(food$eventid == id2)]
  }
  
  return(acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(long1 - long2)) * 6371)
}

create_dist_matrix <- function(activity, food){
  ids <- c(activity$eventid, food$eventid)
  DM = data.frame(matrix("", ncol = length(ids), nrow = length(ids))) 
  colnames(DM) <- ids
  rownames(DM) <- ids
  
  for(a in 1:length(ids)){
    for(b in 1:length(ids)){
      DM[a,b] = dist(a,b)
    }
  }
  return (DM)
}
