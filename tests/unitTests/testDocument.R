testDocument <- function() {

  swipe <- function() {

    if (exists("alpha")) rm(alpha, envir = .GlobalEnv)
    if (exists("alpha2")) rm(alpha2, envir = .GlobalEnv)
    if (exists("brown")) rm(brown, envir = .GlobalEnv)
    if (exists("oxford")) rm(oxford, envir = .GlobalEnv)
  }

  # Test 0: Test document instantiation
  test0 <- function() {

    test <- "Test0"
    cat(paste("\n",test,"Commencing\r"))

    swipe()
    Document$new(name = "alpha", desc = "alpha document", fileName = "alpha.txt")
    doc <- alpha$getDocument(verbose = FALSE)
    stopifnot(doc$name == "alpha")
    stopifnot(doc$desc == "alpha document")
    stopifnot(doc$fileName == "alpha.txt")
    stopifnot("POSIXct" %in% class(doc$created))
    stopifnot("POSIXct" %in% class(doc$modified))
    # Document$new(name = "alpha", desc = "alpha document", fileName = "alpha.txt")   # should error, duplicate
    swipe()
    # Document$new(name = "alpha doc") # should error no spaces and/or filename is blank
    # Document$new(name = "alpha2", "alpha2 doc") # should error filename is blank
    # Document$new(name = "alpha2", "alpha2 doc", "alpha2.txt") # alpha or alpha2
    #Document$new()

    cat(paste("\n",test,"Completed: Success!\n"))
  }


  #Test 01 Test document collection instantiation
  test1 <- function() {

    test <- "Test1"
    cat(paste("\n",test,"Commencing\r"))

    DocumentCollection$new(name = "brown") # Description automated created
    doc <<- brown$getDocument()
    stopifnot(doc$collection$name == "brown")
    stopifnot(doc$collection$desc == "brown collection")
    stopifnot("POSIXct" %in% class(doc$collection$created))
    stopifnot("POSIXct" %in% class(doc$collection$modified))

    DocumentCollection$new(name = "oxford", desc = "Oxford University Corpus")
    doc <<- oxford$getDocument(verbose = FALSE)
    stopifnot(doc$collection$name == "oxford")
    stopifnot(doc$collection$desc == "Oxford University Corpus")
    stopifnot("POSIXct" %in% class(doc$collection$created))
    stopifnot("POSIXct" %in% class(doc$collection$modified))
    cat(paste("\n",test,"Completed: Success!\n"))
  }

  #Test 2 Restore objects in cache into the global environment.
  test2 <- function() {

    test <- "Test2"
    cat(paste("\n",test,"Commencing\r"))

    cat(paste("\n",test,"Completed: Success!\n"))
  }

  #Test 3 Get information from the cache
  test3 <- function() {

    test <- "Test3"
    cat(paste("\n",test,"Commencing\r"))

    cat(paste("\n",test,"Completed: Success!\n"))
  }

  #Test 4 Write information from the cache and save the cache
  test4 <- function() {

    test <- "Test4"
    cat(paste("\n",test,"Commencing\r"))

    cat(paste("\n",test,"Completed: Success!\n"))
  }
test0()
test1()
# test2()
# test3()
# test4()
}

  testDocument()
