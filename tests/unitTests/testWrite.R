testWrite <- function() {

  init <- function() {

    # Clean up
    # Source cache and log
    source("./tests/checkCache.r")
    source("./tests/logTests.r")
  }

  test0 <- function() {
    test <- "test0: Test WriteBin"
    cat(paste("\n",test, " Commencing\r"))
    cls = "WriteBin"
    mthd = "writeData"

    # Validation
    w <- WriteBin$new()
    w$writeData()
    w$writeData(ReadBin$new())

    # Successful write
    w$writeData(news, binContent)
    meta <- binContent$getDocument(type = "list")
    stopifnot((Sys.time() - file.mtime(file.path(meta$path, meta$fileName))) < 1)
    stopifnot(file.size(file.path(meta$path, meta$fileName) > 100000))

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
    w$writeData()
    w$writeData(ReadText$new())

    # Successful write
    w$writeData(news, textContent)
    meta <- textContent$getDocument(type = "list")
    stopifnot((Sys.time() - file.mtime(file.path(meta$path, meta$fileName))) < 1)
    stopifnot(file.size(file.path(meta$path, meta$fileName) > 100000))

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
    w$writeData()
    w$writeData(ReadCsv$new())

    # Successful write
    w$writeData(contractions, csvContent)
    meta <- csvContent$getDocument(type = "list")
    stopifnot((Sys.time() - file.mtime(file.path(meta$path, meta$fileName))) < 1)
    stopifnot(file.size(file.path(meta$path, meta$fileName) > 100000))

    # Logit
    logTests(cls = cls, mthd = mthd, note = "WriteCsv tested with appropriate validation")

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
