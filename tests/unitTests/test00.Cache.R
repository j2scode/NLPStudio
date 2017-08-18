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
    if (length(studioCache$lsCache() > 0)) studioCache$purgeCache()
    studioCache$setCache(key = "Lab", value = Lab)
    studioCache$setCache(key = "swipe", value = swipe)
    studioCache$setCache(key = "Singleton", value = Singleton)


    stopifnot(class(studioCache$getCache("Lab")) == "R6ClassGenerator")
    stopifnot(class(studioCache$getCache("swipe")) == "function")
    stopifnot(class(studioCache$getCache("Singleton")) == "R6ClassGenerator")
    stopifnot(studioCache$lsCache() == c( "swipe", "Lab","Singleton"))

    cat(paste("\n",test,"Completed: Success!\n"))
  }


  #Test 01 Save and load cache
  test1 <- function() {

    test <- "Test1"
    cat(paste("\n",test,"Commencing\r"))

    studioCache$saveCache()
    studioCache$purgeCache()
    stopifnot(studioCache$lsCache() == character(0))
    rm(studioCache, envir = .GlobalEnv)

    studioCache <<- StudioCache$new()$getInstance()
    studioCache$loadCache()
    stopifnot(class(studioCache$getCache("Lab")) ==  "R6ClassGenerator")
    stopifnot(class(studioCache$getCache("swipe")) ==  "function")
    stopifnot(class(studioCache$getCache("Singleton")) ==  "R6ClassGenerator")
    stopifnot(studioCache$lsCache() == c("swipe", "Lab", "Singleton"))

    cat(paste("\n",test,"Completed: Success!\n"))
  }

  #Test 2 Restore objects in cache into the global environment.
  test2 <- function() {

    test <- "Test2"
    cat(paste("\n",test,"Commencing\r"))

    if (exists("Lab", envir = .GlobalEnv)) rm(Lab, envir = .GlobalEnv)
    if (exists("swipe", envir = .GlobalEnv)) rm(swipe, envir = .GlobalEnv)
    if (exists("Singleton", envir = .GlobalEnv)) rm(Singleton, envir = .GlobalEnv)

    studioCache$restoreCache()

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
    studioCacheManager$getCache("swipe")
    stopifnot(class(swipe) ==  "function")

    cat(paste("\n",test,"Completed: Success!\n"))
  }

  #Test 4 Write information from the cache and save the cache
  test4 <- function() {

    test <- "Test4"
    cat(paste("\n",test,"Commencing\r"))

    cacheNames <<- studioCache$lsCache()

    key <- "cacheNames"
    studioCacheManager$setCache(key, cacheNames)
    rm(cacheNames, envir = .GlobalEnv)
    cacheNames <<- studioCacheManager$getCache(key)
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
