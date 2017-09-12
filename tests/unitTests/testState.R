testState <- function() {

  swipe <- function() {

    archiveDir <- "./Archive"
    labsDir <- "./Labs"
    stateFile <- "./.State.Rdata"
    if (dir.exists(archiveDir)) unlink(archiveDir, recursive = TRUE)
    if (dir.exists(labsDir)) unlink(labsDir, recursive = TRUE)
    if (file.exists(stateFile)) file.remove(stateFile)

    stateManager <<- StateServer$new()$getInstance()
  }

  # Test 0: Set and get state
  test0 <- function() {

    test <- "Test0"
    cat(paste("\n",test,"Commencing\r"))
    swipe()
    if (length(stateManager$lsState() > 0)) stateManager$purgeState()
    stateManager$saveState(key = "Lab", value = Lab)
    stateManager$saveState(key = "swipe", value = swipe)
    stateManager$saveState(key = "Singleton", value = Singleton)


    stopifnot(class(stateManager$restoreState("Lab")) == "R6ClassGenerator")
    stopifnot(class(stateManager$restoreState("swipe")) == "function")
    stopifnot(class(stateManager$restoreState("Singleton")) == "R6ClassGenerator")
    stopifnot(stateManager$lsState() == c( "swipe", "Lab","Singleton"))

    cat(paste("\n",test,"Completed: Success!\n"))
  }


  #Test 01 Save and load state
  test1 <- function() {

    test <- "Test1"
    cat(paste("\n",test,"Commencing\r"))

    stateManager$saveState()
    stateManager$purgeState()
    stopifnot(stateManager$lsState() == character(0))
    rm(stateManager, envir = .GlobalEnv)

    stateManager <<- StateServer$new()$getInstance()
    stateManager$loadState()
    stopifnot(class(stateManager$restoreState("Lab")) ==  "R6ClassGenerator")
    stopifnot(class(stateManager$restoreState("swipe")) ==  "function")
    stopifnot(class(stateManager$restoreState("Singleton")) ==  "R6ClassGenerator")
    stopifnot(stateManager$lsState() == c("swipe", "Lab", "Singleton"))

    cat(paste("\n",test,"Completed: Success!\n"))
  }

  #Test 2 Restore objects in state into the global environment.
  test2 <- function() {

    test <- "Test2"
    cat(paste("\n",test,"Commencing\r"))

    if (exists("Lab", envir = .GlobalEnv)) rm(Lab, envir = .GlobalEnv)
    if (exists("swipe", envir = .GlobalEnv)) rm(swipe, envir = .GlobalEnv)
    if (exists("Singleton", envir = .GlobalEnv)) rm(Singleton, envir = .GlobalEnv)

    stateManager$restoreState()

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
    stateManager$restoreState("swipe")
    stopifnot(class(swipe) ==  "function")

    cat(paste("\n",test,"Completed: Success!\n"))
  }

  #Test 4 Write information from the state and save the state
  test4 <- function() {

    test <- "Test4"
    cat(paste("\n",test,"Commencing\r"))

    stateNames <<- stateManager$lsState()

    key <- "stateNames"
    stateManager$saveState(key, stateNames)
    rm(stateNames, envir = .GlobalEnv)
    stateNames <<- stateManager$restoreState(key)
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
