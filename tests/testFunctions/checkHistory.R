checkHistory <- function(cls, method, objectName, event) {
  events <- historian$searchEvents(cls = cls, method = method, objectName = objectName)
  event <- subset(events, event = event)
  if (nrow(event) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}
