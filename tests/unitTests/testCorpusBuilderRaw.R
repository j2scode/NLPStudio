testLab <- function() {

  init <- function() {
    rm(list = ls(envir = .GlobalEnv)[grep("builder", ls(envir = .GlobalEnv))], pos = ".GlobalEnv")
    brown$removeDocument("news")
    brown$removeDocument("sports")
    brown$removeDocument("finance")
    rm(list = ls(envir = .GlobalEnv)[grep("brown", ls(envir = .GlobalEnv))], pos = ".GlobalEnv")
    rm(list = ls(envir = .GlobalEnv)[grep("news", ls(envir = .GlobalEnv))], pos = ".GlobalEnv")
    rm(list = ls(envir = .GlobalEnv)[grep("sports", ls(envir = .GlobalEnv))], pos = ".GlobalEnv")
    rm(list = ls(envir = .GlobalEnv)[grep("finance", ls(envir = .GlobalEnv))], pos = ".GlobalEnv")


    source("./tests/checkState.r")
    source("./tests/logTests.r")
  }

  # Test 0: Test instantiation and getBuilder
  test0 <- function() {
    test <- "test0"
    cat(paste("\n",test, " Commencing\r"))

    # Instantiate
    # CorpusBuilderRaw$new() # Invalid builder name  Success!
    CorpusBuilderRaw$new("builder") # Invalid builder name

    # # Confirm instantiation
    # b <- builder$getBuilder()
    # stopifnot(exists("builder"))
    # stopifnot(b$name == "builder")
    # stopifnot((Sys.time() - b$created) < 1)
    # stopifnot((Sys.time() - b$modified) < 1)
    #
    # # Check state
    # stopifnot(checkState("builder") == TRUE)
    #
    # # Logit
    # logTests(cls = cls, mthd = "initiate", note = "Successfully instantiated CorpusBuilderRaw")

    # Wrapup
    cat(paste("\n", test, " Completed: Success!\r"))
  }

  # Test 1: Test buildCollection
  test1 <- function() {
    test <- "test1"
    cat(paste("\n",test, " Commencing\r"))

    # Instantiate collection
    # builder$buildCollection() # Should fail, Success
    builder$buildCollection("brown", "Brown corpus")
    b <- brown$getDocument()

    # Confirm instantiation
    stopifnot(b$collection$name == "brown")
    stopifnot(b$collection$desc == "Brown Corpus")
    stopifnot((Sys.time() - b$collection$created) < 1)
    stopifnot((Sys.time() - b$collection$modified) < 1)

    # Check state
    stopifnot(checkState("brown") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "buildCollection", note = "Successfully instantiated collection")

    # Wrapup
    cat(paste("\n", test, " Completed: Success!\r"))
  }

  # Test 2: Test buildDocument
  test2 <- function() {
    test <- "test2"
    cat(paste("\n",test, " Commencing\r"))

    # Instantiate documents
    builder$buildDocument("news", "news.txt", "News of the world")
    builder$buildDocument("sports", "sports.txt", "Sports headlines")
    builder$buildDocument("finance", "finance.txt", "Financial Reports")

    # Obtain documents
    b <- brown$getDocument()
    n <- news$getDocument()
    s <- sports$getDocument()
    f <- finance@getDocument

    # Validate documents
    stopifnot(n$name == "news")
    stopifnot(s$name == "sports")
    stopifnot(f$name == "finance")

    stopifnot(n$path == "brown")
    stopifnot(s$path == "brown")
    stopifnot(f$path == "brown")

    stopifnot(n$fileName == "news.txt")
    stopifnot(s$fileName == "sports.txt")
    stopifnot(f$fileName == "finance.txt")

    stopifnot(n$desc == "News of the world")
    stopifnot(s$desc == "Sports headlines")
    stopifnot(f$desc == "Financial Reports")

    # Check state
    stopifnot(checkState("news") == TRUE)
    stopifnot(checkState("sports") == TRUE)
    stopifnot(checkState("finance") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "buildDocument", note = "Successfully instantiated documents")

    # Wrapup
    cat(paste("\n", test, " Completed: Success!\r"))
  }

  # Test 3: Test addDocument
  test3 <- function() {
    test <- "test3"
    cat(paste("\n",test, " Commencing\r"))

    # Add documents to collection
    brown$addDocument(news)
    brown$addDocument(sports)
    brown$addDocument(finance)

    # Get the collection
    b <- brown$getDocument()

    # Confirm dates modfied date updated appropriately
    stopifnot(b$modified > b$created)
    stopifnot((Sys.time - b$modified) < 1)

    # Confirm addition of documents
    stopifnot(b$documents[[1]]$name == "news")
    stopifnot(b$documents[[2]]$name == "sports")
    stopifnot(b$documents[[3]]$name == "finance")

    stopifnot(b$documents[[1]]$fileName == "news.txt")
    stopifnot(b$documents[[2]]$fileName == "sports.txt")
    stopifnot(b$documents[[3]]$fileName == "finance.txt")

    stopifnot(b$documents[[1]]$desc == "News of the world")
    stopifnot(b$documents[[2]]$desc == "Sports headlines")
    stopifnot(b$documents[[3]]$desc == "Financial Reports")

    stopifnot(b$documents[[1]]$path == "brown")
    stopifnot(b$documents[[2]]$path == "brown")
    stopifnot(b$documents[[3]]$path == "brown")

    # Check state
    stopifnot(checkState("brown") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "addDocument", note = "Successfully added document to collection")

    cat(paste("\n", test, " Completed: Success!\r"))
  }

cls <- "CorpusBuilderRaw"

init()
test0()
test1()
test2()
test3()
# test4()
# test5()
# test6()
# test7()
}

devtools::load_all()
testLab()
