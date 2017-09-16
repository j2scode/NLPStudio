logTests <- function(class, mthd, note) {

  newPost <- data.frame(date = Sys.time(),
                        class = class,
                        method = mthd,
                        note = note)
  if (!exists("testLog")) {
    testLog <<- newPost
  } else {
    testLog <<- rbind(testLog, newPost)
  }
}
