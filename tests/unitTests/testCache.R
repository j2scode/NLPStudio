testCache <- function() {

  swipe <- function() {

    archiveDir <- "./Archive"
    labsDir <- "./Labs"
    cacheFile <- "./.StudioCache.Rdata"
    if (dir.exists(archiveDir)) unlink(archiveDir, recursive = TRUE)
    if (dir.exists(labsDir)) unlink(labsDir, recursive = TRUE)
    if (file.exists(cacheFile)) file.remove(cacheFile)
  }

  # Test 0: Set and get cache
  test0 <- function() {

    test <- "Test0"
    cat(paste("\n",test,"Commencing\r"))
    swipe()
    if (length(nlpStudioCache$lsCache() > 0)) nlpStudioCache$purgeCache()
    nlpStudioCache$setCache(key = "Lab", value = Lab)
    nlpStudioCache$setCache(key = "swipe", value = swipe)
    nlpStudioCache$setCache(key = "Singleton", value = Singleton)


    stopifnot(class(nlpStudioCache$getCache("Lab")) == "R6ClassGenerator")
    stopifnot(class(nlpStudioCache$getCache("swipe")) == "function")
    stopifnot(class(nlpStudioCache$getCache("Singleton")) == "R6ClassGenerator")
    stopifnot(nlpStudioCache$lsCache() == c( "swipe", "Lab","Singleton"))

    cat(paste("\n",test,"Completed: Success!\n"))
  }


  #Test 01 Save and load cache
  test1 <- function() {

    test <- "Test1"
    cat(paste("\n",test,"Commencing\r"))

    nlpStudioCache$saveCache()
    nlpStudioCache$purgeCache()
    stopifnot(nlpStudioCache$lsCache() == character(0))
    rm(nlpStudioCache, envir = .GlobalEnv)

    nlpStudioCache <<- StudioCache$new()$getInstance()
    nlpStudioCache$loadCache()
    stopifnot(class(nlpStudioCache$getCache("Lab")) ==  "R6ClassGenerator")
    stopifnot(class(nlpStudioCache$getCache("swipe")) ==  "function")
    stopifnot(class(nlpStudioCache$getCache("Singleton")) ==  "R6ClassGenerator")
    stopifnot(nlpStudioCache$lsCache() == c("swipe", "Lab", "Singleton"))

    cat(paste("\n",test,"Completed: Success!\n"))
  }

  #Test 2 Restore objects in cache into the global environment.
  test2 <- function() {

    test <- "Test2"
    cat(paste("\n",test,"Commencing\r"))

    if (exists("Lab", envir = .GlobalEnv)) rm(Lab, envir = .GlobalEnv)
    if (exists("swipe", envir = .GlobalEnv)) rm(swipe, envir = .GlobalEnv)
    if (exists("Singleton", envir = .GlobalEnv)) rm(Singleton, envir = .GlobalEnv)

    nlpStudioCache$restoreCache()

    stopifnot(exists("Lab"))
    stopifnot(exists("swipe"))
    stopifnot(exists("Singleton"))

    stopifnot(class(Lab) ==  "R6ClassGenerator")
    stopifnot(class(swipe) ==  "function")
    stopifnot(class(Singleton) ==  "R6ClassGenerator")

    cat(paste("\n",test,"Completed: Success!\n"))
  }

  #Test 3 Get information from the cache
  test3 <- function() {

    test <- "Test3"
    cat(paste("\n",test,"Commencing\r"))

    rm(swipe, envir = .GlobalEnv)
    nlpStudioCache$getCache("swipe")
    stopifnot(class(swipe) ==  "function")

    cat(paste("\n",test,"Completed: Success!\n"))
  }

  #Test 4 Write information from the cache and save the cache
  test4 <- function() {

    test <- "Test4"
    cat(paste("\n",test,"Commencing\r"))

    cacheNames <<- nlpStudioCache$lsCache()

    key <- "cacheNames"
    nlpStudioCache$setCache(key, cacheNames)
    rm(cacheNames, envir = .GlobalEnv)
    cacheNames <<- nlpStudioCache$getCache(key)
    stopifnot(class(cacheNames) ==  "character")

    cat(paste("\n",test,"Completed: Success!\n"))
  }
test0()
test1()
test2()
test3()
test4()
}

  testCache()
