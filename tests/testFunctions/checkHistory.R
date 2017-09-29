checkHistory <- function(class, method, objectName, event) {
  events <- historian$searchEvents(clas = class, method = method, objectName = objectName)
  event <- subset(events, event = event)
  if (nrow(event) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}
