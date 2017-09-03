testWrite <- function() {

  init <- function() {

    binPath <<- "./Labs/blue/oxford/raw/en_US.news.txt"
    textPath <<- "./Labs/blue/oxford/raw/en_US.news.txt"
    csvPath <<- "./Labs/blue/oxford/raw/contractions.csv"
    rdataPath <<- "./Labs/blue/oxford/raw/quadgrams.Rdata"

    # Source state and log
    source("./tests/checkState.r")
    source("./tests/logTests.r")
  }

  test0 <- function() {
    test <- "test0: Test WriteBin"
    cat(paste("\n",test, " Commencing\r"))
    cls = "WriteBin"
    mthd = "writeData"

    # Validation
    w <- WriteBin$new()
    #w$writeData() # should fail, path is missing: success
    #w$writeData(path = blue) # should fail, path is invalid: success
    #w$writeData(path = binPath) # should fail, no content

    # Successful write
    w$writeData(path = binPath, content = binContent)
    r <- ReadBin$new()
    c <- r$readData(binPath)
    finfo <- file.info(binPath, extra_cols = FALSE)
    stopifnot(length(finfo[difftime(Sys.time(),finfo[,"mtime"], units = "secs") < 5, 4]) == 1)
    stopifnot(identical(binContent, c))

    # Logit
    logTests(cls = cls, mthd = mthd, note = "WriteBin tested with appropriate validation")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test1 <- function() {
    test <- "test1: Test WriteText"
    cat(paste("\n",test, " Commencing\r"))
    cls = "WriteText"
    mthd = "writeData"

    # Validation
    w <- WriteText$new()
    #w$writeData() # should fail, path is missing: success
    #w$writeData(path = blue) # should fail, path is invalid: success
    #w$writeData(path = textPath) # should fail, no content

    # Successful write
    w$writeData(path = textPath, content = textContent)
    r <- ReadText$new()
    c <- r$readData(textPath)
    finfo <- file.info(textPath, extra_cols = FALSE)
    stopifnot(length(finfo[difftime(Sys.time(),finfo[,"mtime"], units = "secs") < 5, 4]) == 1)
    stopifnot(identical(textContent, c))


    # Logit
    logTests(cls = cls, mthd = mthd, note = "WriteText tested with appropriate validation")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test2 <- function() {
    test <- "test2: Test WriteCsv"
    cat(paste("\n",test, " Commencing\r"))
    cls = "WriteCsv"
    mthd = "writeData"

    # Validation
    w <- WriteCsv$new()
    #w$writeData() # should fail, path is missing: success
    #w$writeData(path = blue) # should fail, path is invalid: success
    #w$writeData(path = csvPath) # should fail, no content

    # Successful write
    w$writeData(path = csvPath, content = csvContent)
    r <- ReadCsv$new()
    c <- r$readData(csvPath)
    finfo <- file.info(csvPath, extra_cols = FALSE)
    stopifnot(length(finfo[difftime(Sys.time(),finfo[,"mtime"], units = "secs") < 5, 4]) == 1)
    stopifnot(identical(csvContent, c))


    # Logit
    logTests(cls = cls, mthd = mthd, note = "WriteCsv tested with appropriate validation")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test3 <- function() {
    test <- "test3: Test WriteRdata"
    cat(paste("\n",test, " Commencing\r"))
    cls = "WriteRdata"
    mthd = "writeData"

    # Validation
    w <- WriteRdata$new()
    #w$writeData() # should fail, path is missing: success
    #w$writeData(path = blue) # should fail, name  is missing: success
    #w$writeData(name = "rdataContent", path = rdataPath) # should fail, no content
    #w$writeData(path = rdataPath, content = rdataContent)# Fail, name is missing

    # Successful write
    w$writeData(name = "rdataContent", path = rdataPath, content = rdataContent)
    r <- ReadRdata$new()
    c <- r$readData(rdataPath)
    finfo <- file.info(rdataPath, extra_cols = FALSE)
    stopifnot(length(finfo[difftime(Sys.time(),finfo[,"mtime"], units = "secs") < 5, 4]) == 1)
    stopifnot(identical(rdataContent, c))


    # Logit
    logTests(cls = cls, mthd = mthd, note = "WriteRdata tested with appropriate validation")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  init()
  test0()
  test1()
  test2()
  test3()

}


devtools::load_all()
testWrite()
