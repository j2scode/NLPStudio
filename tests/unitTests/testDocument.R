testDocument <- function() {

  init <- function() {

    # Clean up
    if (exists("news", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("news", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
    if (exists("sports", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("sports", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
    if (exists("finance", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("finance", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }

    # Source state and log
    source("./tests/checkState.r")
    source("./tests/logTests.r")
  }

  test0 <- function() {
    test <- "test0: Document Instantiation"
    cat(paste("\n",test, " Commencing\r"))

    # Validation
    Document$new() # Fail, name required
    Document$new("news") # Fail filename required
    Document$new("news", "news.txt", "./Labs/blue/brown") # Fail path doesn't exist

    # Successful Instantiation, no path
    Document$new(name = "news", fileName = "en_US.news.txt",
                 desc = "News of the World") # No path, can update when adding to collection
    Document$new(name = "sports", fileName = "sports.txt",
                 path = "./Labs/blue/oxford", desc = "Sports Headlines") # Validates path
    Document$new(name = "finance", fileName = "finance.txt",
                 path = "./Labs/blue/oxford", desc = "Finance Reports") # Validates path

    # Validate Instantiation
    n <- news$getDocument(type = "list")
    s <- sports$getDocument(type = "list")
    f <- finance$getDocument(type = "list")

    stopifnot(n$name == "news")
    stopifnot(n$desc == "News of the World")
    stopifnot(is.null(n$path))
    stopifnot(n$fileName == "news.txt")
    stopifnot((Sys.time() - n$modified) < 1)
    stopifnot((Sys.time() - n$created) < 1)

    stopifnot(s$name == "sports")
    stopifnot(s$desc == "Sports Headlines")
    stopifnot(s$path == "./Labs/blue/oxford")
    stopifnot(s$fileName == "sports.txt")
    stopifnot((Sys.time() - s$modified) < 1)
    stopifnot((Sys.time() - s$created) < 1)

    stopifnot(f$name == "financials")
    stopifnot(f$desc == "Finance Reports")
    stopifnot(f$path == "./Labs/blue/oxford")
    stopifnot(f$fileName == "finance.txt")
    stopifnot((Sys.time() - f$modified) < 1)
    stopifnot((Sys.time() - f$created) < 1)

    # Check State
    stopifnot(checkState('news') == TRUE)
    stopifnot(checkState('sports') == TRUE)
    stopifnot(checkState('finance') == TRUE)


    # Logit
    logTests(cls = cls, mthd = "initiate", note = "Blocked invalid document variables")
    logTests(cls = cls, mthd = "initiate", note = "Created document w/o path")
    logTests(cls = cls, mthd = "getDocument", note = "Tested list type")
    logTests(cls = cls, mthd = "initiate", note = "Created document with path")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test1 <- function() {
    test <- "test1: Test getDocument"
    cat(paste("\n",test, " Commencing\r"))

    # Test object
    n <- news$getDocument(type = "object")
    s <- sports$getDocument(type = "object")
    f <- finance$getDocument(type = "object")

    stopifnot(isTRUE(all.equal(news, n)))
    stopifnot(isTRUE(all.equal(sports, s)))
    stopifnot(isTRUE(all.equal(finance, f)))

    # Test Data frame
    n <- news$getDocument(type = "df")
    s <- sports$getDocument(type = "df")
    f <- finance$getDocument(type = "df")

    stopifnot(n$name[1] == "news")
    stopifnot(n$desc[1] == "News of the World")
    stopifnot(is.null(n$path[1]))
    stopifnot(n$fileName[1] == "news.txt")
    stopifnot((Sys.time() - n$modified[1]) < 1)
    stopifnot((Sys.time() - n$created[1]) < 1)

    stopifnot(s$name[1] == "sports")
    stopifnot(s$desc[1] == "Sports Headlines")
    stopifnot(s$path[1] == "./Labs/blue/oxford")
    stopifnot(s$fileName[1] == "sports.txt")
    stopifnot((Sys.time() - s$modified[1]) < 1)
    stopifnot((Sys.time() - s$created[1]) < 1)

    stopifnot(f$name[1] == "financials")
    stopifnot(f$desc[1] == "Finance Reports")
    stopifnot(f$path[1] == "./Labs/blue/oxford")
    stopifnot(f$fileName[3] == "finance.txt")
    stopifnot((Sys.time() - f$modified[1]) < 1)
    stopifnot((Sys.time() - f$created[1]) < 1)


    # Logit
    logTests(cls = cls, mthd = "getDocument", note = "Tested object type")
    logTests(cls = cls, mthd = "getDocument", note = "Tested data frame type")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test2 <- function() {
    test <- "test2: Add path"
    cat(paste("\n",test, " Commencing\r"))

    # Validation
    news$path <- "./Labs/blue/dada" # should fail, invalid path
    news$path <- "./Labs/blue/oxford" # Update missing

    # Get document
    n <- news$getDocument(type = "list")

    # Verify
    stopifnot(n$path == "./Labs/blue/oxford")
    stopifnot(n$created < n$modified)

    # Check State
    stopifnot(checkState('news') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "path", note = "Blocked invalid path")
    logTests(cls = cls, mthd = "path", note = "Updated the path with valid value")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test3 <- function() {
    test <- "test3: Test ReadBin"
    cat(paste("\n",test, " Commencing\r"))

    # Validation
    content <<- news$readDocument() # should fail, reader required
    content <<- news$readDocument(ReadNoise$new()) # should fail, invalid reader

    # Verify
    content <<- news$readDocument(ReadBin$new())
    stopifnot(object.size(content) > 100000)

    # Check State
    stopifnot(checkState('oxford') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "readDocument", note = "Validated reader")
    logTests(cls = cls, mthd = "readDocument", note = "Successfully read document with ReadBin")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test4 <- function() {
    test <- "test4: Test WriteBin"
    cat(paste("\n",test, " Commencing\r"))

    # Validation
    news$writeDocument() # should fail, reader required
    news$writeDocument(ReadNoise$new()) # should fail, invalid reader

    # Verify
    news$writeDocument(WriteBin$new(), content)
    content2 <<- news$readDocument(ReadBin$new())
    stopifnot(object.size(content2) > 100000)

    # Check State
    stopifnot(checkState('oxford') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "writeDocument", note = "Validated writer")
    logTests(cls = cls, mthd = "writeDocument", note = "Successfully wrote document with WriteBin")

    cat(paste("\n", test, " Completed: Success!\n"))
  }


  test5 <- function() {
    test <- "test5: Test ReadText"
    cat(paste("\n",test, " Commencing\r"))

    # Validation
    content <<- news$readDocument() # should fail, reader required
    content <<- news$readDocument(ReadNoise$new()) # should fail, invalid reader

    # Verify
    content <<- news$readDocument(ReadText$new())
    stopifnot(object.size(content) > 100000)

    # Check State
    stopifnot(checkState('oxford') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "readDocument", note = "Validated reader")
    logTests(cls = cls, mthd = "readDocument", note = "Successfully read document with ReadText")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test6 <- function() {
    test <- "test6: Test WriteText"
    cat(paste("\n",test, " Commencing\r"))

    # Validation
    news$writeDocument() # should fail, reader required
    news$writeDocument(ReadNoise$new()) # should fail, invalid reader

    # Verify
    news$writeDocument(WriteText$new(), content)
    content2 <<- news$readDocument(ReadText$new())
    stopifnot(object.size(content2) > 100000)

    # Check State
    stopifnot(checkState('oxford') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "writeDocument", note = "Validated writer")
    logTests(cls = cls, mthd = "writeDocument", note = "Successfully wrote document w/ WriteText")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test7 <- function() {
    test <- "test5: Test ReadCsv"
    cat(paste("\n",test, " Commencing\r"))

    # Validation
    content <<- news$readDocument() # should fail, reader required
    content <<- news$readDocument(ReadNoise$new()) # should fail, invalid reader

    # Verify
    content <<- news$readDocument(ReadCsv$new())
    stopifnot(object.size(content) > 100000)

    # Check State
    stopifnot(checkState('oxford') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "readDocument", note = "Validated reader")
    logTests(cls = cls, mthd = "readDocument", note = "Successfully read document with ReadCsv")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test8 <- function() {
    test <- "test6: Test WriteCsv"
    cat(paste("\n",test, " Commencing\r"))

    # Validation
    news$writeDocument() # should fail, reader required
    news$writeDocument(ReadNoise$new()) # should fail, invalid reader

    # Verify
    news$writeDocument(WriteCsv$new(), content)
    content2 <<- news$readDocument(ReadCsv$new())
    stopifnot(object.size(content2) > 100000)

    # Check State
    stopifnot(checkState('oxford') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "writeDocument", note = "Validated writer")
    logTests(cls = cls, mthd = "writeDocument", note = "Successfully wrote document w/ WriteCsv")

    cat(paste("\n", test, " Completed: Success!\n"))
  }


  test9 <- function() {
    test <- "test5: Test ReadRdata"
    cat(paste("\n",test, " Commencing\r"))

    # Validation
    content <<- news$readDocument() # should fail, reader required
    content <<- news$readDocument(ReadNoise$new()) # should fail, invalid reader

    # Verify
    content <<- news$readDocument(ReadRdata$new())
    stopifnot(object.size(content) > 100000)

    # Check State
    stopifnot(checkState('oxford') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "readDocument", note = "Validated reader")
    logTests(cls = cls, mthd = "readDocument", note = "Successfully read document with ReadRdata")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test10 <- function() {
    test <- "test6: Test WriteRdata"
    cat(paste("\n",test, " Commencing\r"))

    # Validation
    news$writeDocument() # should fail, reader required
    news$writeDocument(ReadNoise$new()) # should fail, invalid reader

    # Verify
    news$writeDocument(WriteRdata$new(), content)
    content2 <<- news$readDocument(ReadRdata$new())
    stopifnot(object.size(content2) > 100000)

    # Check State
    stopifnot(checkState('oxford') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "writeDocument", note = "Validated writer")
    logTests(cls = cls, mthd = "writeDocument", note = "Successfully wrote document w/ WriteRdata")

    cat(paste("\n", test, " Completed: Success!\n"))
  }
  init()
  test0()
  test1()
  test2()
  test3()
  test4()
  test5()
  test6()

}

cls <- "Document"

devtools::load_all()
testDocument()
