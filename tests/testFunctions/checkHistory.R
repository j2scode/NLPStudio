checkHistory <- function(className, method, objectName, event) {
  events <- historian$searchEvents(className = className, method = method, objectName = objectName)
  event <- subset(events, event = event)
  if (nrow(event) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}
