testState <- function() {

  swipe <- function() {

    archiveDir <- "./Archive"
    labsDir <- "./Labs"
    stateFile <- "./.State.Rdata"
    if (dir.exists(archiveDir)) unlink(archiveDir, recursive = TRUE)
    if (dir.exists(labsDir)) unlink(labsDir, recursive = TRUE)
    if (file.exists(stateFile)) file.remove(stateFile)

    nlpStudioState <<- StateManager$new()$getInstance()
  }

  # Test 0: Set and get state
  test0 <- function() {

    test <- "Test0"
    cat(paste("\n",test,"Commencing\r"))
    swipe()
    if (length(nlpStudioState$lsState() > 0)) nlpStudioState$purgeState()
    nlpStudioState$setState(key = "Lab", value = Lab)
    nlpStudioState$setState(key = "swipe", value = swipe)
    nlpStudioState$setState(key = "Singleton", value = Singleton)


    stopifnot(class(nlpStudioState$getState("Lab")) == "R6ClassGenerator")
    stopifnot(class(nlpStudioState$getState("swipe")) == "function")
    stopifnot(class(nlpStudioState$getState("Singleton")) == "R6ClassGenerator")
    stopifnot(nlpStudioState$lsState() == c( "swipe", "Lab","Singleton"))

    cat(paste("\n",test,"Completed: Success!\n"))
  }


  #Test 01 Save and load state
  test1 <- function() {

    test <- "Test1"
    cat(paste("\n",test,"Commencing\r"))

    nlpStudioState$saveState()
    nlpStudioState$purgeState()
    stopifnot(nlpStudioState$lsState() == character(0))
    rm(nlpStudioState, envir = .GlobalEnv)

    nlpStudioState <<- StateManager$new()$getInstance()
    nlpStudioState$loadState()
    stopifnot(class(nlpStudioState$getState("Lab")) ==  "R6ClassGenerator")
    stopifnot(class(nlpStudioState$getState("swipe")) ==  "function")
    stopifnot(class(nlpStudioState$getState("Singleton")) ==  "R6ClassGenerator")
    stopifnot(nlpStudioState$lsState() == c("swipe", "Lab", "Singleton"))

    cat(paste("\n",test,"Completed: Success!\n"))
  }

  #Test 2 Restore objects in state into the global environment.
  test2 <- function() {

    test <- "Test2"
    cat(paste("\n",test,"Commencing\r"))

    if (exists("Lab", envir = .GlobalEnv)) rm(Lab, envir = .GlobalEnv)
    if (exists("swipe", envir = .GlobalEnv)) rm(swipe, envir = .GlobalEnv)
    if (exists("Singleton", envir = .GlobalEnv)) rm(Singleton, envir = .GlobalEnv)

    nlpStudioState$restoreState()

    stopifnot(exists("Lab"))
    stopifnot(exists("swipe"))
    stopifnot(exists("Singleton"))

    stopifnot(class(Lab) ==  "R6ClassGenerator")
    stopifnot(class(swipe) ==  "function")
    stopifnot(class(Singleton) ==  "R6ClassGenerator")

    cat(paste("\n",test,"Completed: Success!\n"))
  }

  #Test 3 Get information from the state
  test3 <- function() {

    test <- "Test3"
    cat(paste("\n",test,"Commencing\r"))

    rm(swipe, envir = .GlobalEnv)
    nlpStudioState$getState("swipe")
    stopifnot(class(swipe) ==  "function")

    cat(paste("\n",test,"Completed: Success!\n"))
  }

  #Test 4 Write information from the state and save the state
  test4 <- function() {

    test <- "Test4"
    cat(paste("\n",test,"Commencing\r"))

    stateNames <<- nlpStudioState$lsState()

    key <- "stateNames"
    nlpStudioState$setState(key, stateNames)
    rm(stateNames, envir = .GlobalEnv)
    stateNames <<- nlpStudioState$getState(key)
    stopifnot(class(stateNames) ==  "character")

    cat(paste("\n",test,"Completed: Success!\n"))
  }
test0()
test1()
test2()
test3()
test4()
}

  testState()
