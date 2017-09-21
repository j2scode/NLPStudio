func1 <- function(a) {
  return(match.call()[[1]])
}

func2 <- function(d) {
  return(func1(d))
}



func2(Bart)
