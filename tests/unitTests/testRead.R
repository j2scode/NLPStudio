testRead <- function() {

  init <- function() {

    binPath <<- "./Labs/blue/oxford/raw/en_US.news.txt"
    textPath <<- "./Labs/blue/oxford/raw/en_US.news.txt"
    csvPath <<- "./Labs/blue/oxford/raw/contractions.csv"
    rdataPath <<- "./Labs/blue/oxford/raw/quadgrams.Rdata"

    # Source cache and log
    source("./tests/checkCache.r")
    source("./tests/logTests.r")
  }

  test0 <- function() {
    test <- "test0: Test ReadBin"
    cat(paste("\n",test, " Commencing\r"))
    cls = "ReadBin"
    mthd = "readData"

    # Validation
    r <- ReadBin$new()
    # binContent <<- r$readData() # should fail, path is missing: success
    # binContent <<- r$readData(blue) # should fail, path is invalid: success

    # Successful read
    binContent <<- r$readData(binPath)
    stopifnot(object.size(binContent) > 100000)

    # Logit
    logTests(cls = cls, mthd = mthd, note = "ReadBin tested with appropriate validation")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test1 <- function() {
    test <- "test1: Test ReadText"
    cat(paste("\n",test, " Commencing\r"))
    cls = "ReadText"
    mthd = "readData"

    # Validation
    r <- ReadText$new()
    # textContent <<- r$readData() # should fail, path is missing
    # textContent <<- r$readData(blue) # should fail, path is invalid

    # Successful read
    textContent <<- r$readData(textPath)
    stopifnot(object.size(textContent) > 100000)

    # Logit
    logTests(cls = cls, mthd = mthd, note = "ReadText tested with appropriate validation")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test2 <- function() {
    test <- "test2: Test ReadCsv"
    cat(paste("\n",test, " Commencing\r"))
    cls = "ReadCsv"
    mthd = "readData"

    # Validation
    r <- ReadCsv$new()
    # csvContent <<- r$readData() # should fail, path is missing
    # csvContent <<- r$readData(blue) # should fail, path is invalid

    # Successful read
    csvContent <<- r$readData(csvPath)
    stopifnot(object.size(csvContent) > 200)

    # Logit
    logTests(cls = cls, mthd = mthd, note = "ReadCsv tested with appropriate validation")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test3 <- function() {
    test <- "test3: Test ReadRdata"
    cat(paste("\n",test, " Commencing\r"))
    cls = "ReadRdata"
    mthd = "readData"

    # Validation
    r <- ReadRdata$new()
    #rdataContent <<- r$readData() # should fail, path is missing
    #rdataContent <<- r$readData(blue) # should fail, path is invalid

    # Successful read
    rdataContent <<- r$readData(rdataPath)
    stopifnot(object.size(rdataContent) > 1000)

    # Logit
    logTests(cls = cls, mthd = mthd, note = "ReadRdata tested with appropriate validation")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  init()
  test0()
  test1()
  test2()
  test3()

}


devtools::load_all()
testRead()
