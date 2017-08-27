testRead <- function() {

  init <- function() {

    if (exists("news", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("news", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
    if (exists("contractions", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("contractions", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
    if (exists("quadgrams", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("quadgrams", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
    if (exists("binContent", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("binContent", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
    if (exists("textContent", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("textContent", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
    if (exists("csvContent", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("csvContent", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
    if (exists("rdataContent", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("rdataContent", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }

    Document$new(name = "news", fileName = "en_US.news.txt", path = "./Labs/blue/oxford/raw", desc = "News of the World")
    Document$new(name = "contractions", fileName = "contractions.csv", path = "./Labs/blue/oxford/raw", desc = "Contractions data")
    Document$new(name = "quadgrams", fileName = "quadgrams.Rdata", path = "./Labs/blue/oxford/raw", desc = "Quadgrams data")

    # Clean up
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
    news$reader <- ""
    r <- ReadBin$new()
    binContent <<- r$readData(nlpStudioCache)

    # Successful read
    binContent <<- r$readData(news)
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
    textContent <<- r$readData(nlpStudioCache)

    # Successful read
    textContent <<- r$readData(news)
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
    csv <- r$readData(nlpStudioCache)

    # Successful read
    csv <- r$readData(contractions)
    stopifnot(object.size(csv) > 200)

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
    r <- ReadCsv$new()
    content <- r$readData(nlpStudioCache)

    # Successful read
    content <- r$readData(quadgrams)
    stopifnot(object.size(content) > 200)

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
