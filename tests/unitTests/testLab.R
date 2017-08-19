testLab <- function() {

  swipe <- function() {

  }

  # Test 0: Test Creation of Lab
  test0 <- function() {

    test <- "Test0"
    cat(paste("\n",test,"Commencing\r"))
    swipe()

    cat(paste("\n",test,"Completed: Success!\n"))
  }


  #Test 01 Save and load cache
  test1 <- function() {

    test <- "Test1"
    cat(paste("\n",test,"Commencing\r"))


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
# test1()
# test2()
# test3()
# test4()
}

  testLab()
