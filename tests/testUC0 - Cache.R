testCache <- function() {

  # Test 0: Set and get cache
  test0 <- function() {

    test <- "Test0"
    cat(paste("\n",test,"Commencing\r"))
    studioCache <<- StudioCache$new()$getInstance()
    if (length(studioCache$lsCache() > 0)) studioCache$purge()
    studioCache$setCache(key = "Lab", value = Lab)
    studioCache$setCache(key = "swipe", value = swipe)
    studioCache$setCache(key = "Cache", value = Cache)


    stopifnot(class(studioCache$getCache("Lab"))[1] == "R6ClassGenerator")
    stopifnot(class(studioCache$getCache("swipe"))[1] == "function")
    stopifnot(class(studioCache$getCache("Cache"))[1] == "R6ClassGenerator")
    stopifnot(studioCache$lsCache() == c( "Lab","swipe","Cache"))

    studioCache$printCache()

    cat(paste("\n",test,"Commpleted: Success!\r"))
  }


  #Test 01 Save and load cache
  test1 <- function() {

    test <- "Test1"
    cat(paste("\n",test,"Commencing\r"))

    studioCache$saveCache()
    studioCache$purgeCache()
    stopifnot(studioCache$lsCache() == character(0))
    #
    studioCache <- StudioCache$new()$getInstance()
    studioCache$loadCache()
    studioCache$printCache()
    stopifnot(class(studioCache$getCache("Lab")) ==  "R6ClassGenerator")
    stopifnot(class(studioCache$getCache("swipe")) ==  "function")
    stopifnot(class(studioCache$getCache("Cache")) ==  "R6ClassGenerator")
    stopifnot(studioCache$lsCache(), c("Lab", "self", "swipe", "Cache"))

    cat(paste("\n",test,"Commpleted: Success!\r"))
  }

  test0()
  test1()

}

  testCache()
