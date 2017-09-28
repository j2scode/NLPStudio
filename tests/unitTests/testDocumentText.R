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
                 desc = "News of the World")
    Document$new(name = "sports", fileName = "sports.txt",
                 desc = "Sports Headlines")
    Document$new(name = "finance", fileName = "finance.txt",
                 desc = "Finance Reports")

    # Obtain document information
    n <- news$getObject()
    s <- sports$getObject()
    f <- finance$getObject()

    # Validate instantiation and getObject
    stopifnot(n$name == "news")
    stopifnot(n$desc == "News of the World")
    stopifnot(is.TRUE(all.equal(n$parent, orphanCollection)))
    stopifnot(n$parentName == "orphanCollection")
    stopifnot(n$path == "./NLPStudio/Labs/OrphanCollection/news")
    stopifnot(n$fileName == "news.txt")
    stopifnot((Sys.time() - n$modified) < 1)
    stopifnot((Sys.time() - n$created) < 1)

    stopifnot(s$name == "sports")
    stopifnot(s$desc == "Sports Headlines")
    stopifnot(is.TRUE(all.equal(n$parent, orphanCollection)))
    stopifnot(n$parentName == "orphanCollection")
    stopifnot(n$path == "./NLPStudio/Labs/OrphanCollection/sports")
    stopifnot(s$fileName == "sports.txt")
    stopifnot((Sys.time() - s$modified) < 1)
    stopifnot((Sys.time() - s$created) < 1)

    stopifnot(f$name == "financials")
    stopifnot(f$desc == "Finance Reports")
    stopifnot(is.TRUE(all.equal(n$parent, orphanCollection)))
    stopifnot(n$parentName == "orphanCollection")
    stopifnot(n$path == "./NLPStudio/Labs/OrphanCollection/financials")
    stopifnot(f$fileName == "finance.txt")
    stopifnot((Sys.time() - f$modified) < 1)
    stopifnot((Sys.time() - f$created) < 1)

    # Check State
    stopifnot(checkState('news') == TRUE)
    stopifnot(checkState('sports') == TRUE)
    stopifnot(checkState('finance') == TRUE)


    # Logit
    logTests(class = class, mthd = "initiate", note = "Blocked invalid document variables")
    logTests(class = class, mthd = "initiate", note = "Successfully created 3 Documents and assigned to orphan collection.")
    logTests(class = class, mthd = "getObject", note = "Successfully obtained document information.")

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
    logTests(class = class, mthd = "readDocument", note = "Validated reader")
    logTests(class = class, mthd = "readDocument", note = "Successfully read document with ReadBin")

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
    logTests(class = class, mthd = "writeDocument", note = "Validated writer")
    logTests(class = class, mthd = "writeDocument", note = "Successfully wrote document with WriteBin")

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
    logTests(class = class, mthd = "readDocument", note = "Validated reader")
    logTests(class = class, mthd = "readDocument", note = "Successfully read document with ReadText")

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
    logTests(class = class, mthd = "writeDocument", note = "Validated writer")
    logTests(class = class, mthd = "writeDocument", note = "Successfully wrote document w/ WriteText")

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
    logTests(class = class, mthd = "readDocument", note = "Validated reader")
    logTests(class = class, mthd = "readDocument", note = "Successfully read document with ReadCsv")

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
    logTests(class = class, mthd = "writeDocument", note = "Validated writer")
    logTests(class = class, mthd = "writeDocument", note = "Successfully wrote document w/ WriteCsv")

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
    logTests(class = class, mthd = "readDocument", note = "Validated reader")
    logTests(class = class, mthd = "readDocument", note = "Successfully read document with ReadRdata")

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
    logTests(class = class, mthd = "writeDocument", note = "Validated writer")
    logTests(class = class, mthd = "writeDocument", note = "Successfully wrote document w/ WriteRdata")

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

class <- "Document"

devtools::load_all()
testDocument()
