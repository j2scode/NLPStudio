testLab <- function() {

  init <- function() {
    if (exists("Bart", envir = .GlobalEnv)) rm("Bart", envir = .GlobalEnv)
    if (exists("Lisa", envir = .GlobalEnv))rm("Lisa", envir = .GlobalEnv)
    nlpStudio$currentLab <- Development
    source("./tests/checkCache.r")
    source("./tests/logTests.r")
  }

  # Test 0: Confirm instantiation of lab with no description
  test0 <- function() {
    test <- "test0"
    cat(paste("\n",test, " Commencing\r"))

    # Errors and Validation
    #lisaLab <- Lab$new("Lisa's  ")  # Fail no spaces: Success
    #lisaLab <- Lab$new("Development") # Already exists Success

    # Valid attempts; Check Lab
    lisaLab <- Lab$new("Lisa")
    nlpStudio$addLab(Lisa)
    lisas <- lisaLab$getLab()
    stopifnot(lisas$name == "Lisa")
    stopifnot(lisas$desc == "Lisa Lab")
    stopifnot(length(lisas$collections) == 0)
    stopifnot((lisas$modified - Sys.time()) < 1)
    stopifnot((lisas$created - Sys.time()) < 1)

    # Valid attempts: Check NLPStudio
    stopifnot(all.equal(nlpStudio$currentLab, Development))
    labs <- nlpStudio$listlabs()
    stopifnot(labs$name[[1]] == "Development")
    stopifnot(labs$desc[[1]] == "Development Lab")
    stopifnot(labs$name[[2]] == "Lisa")
    stopifnot(labs$desc[[2]] == "Lisa Lab")
    stopifnot((labs$created[[2]] - Sys.time()) < 1)
    stopifnot((labs$modified[[2]] - Sys.time()) < 1)

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)
    stopifnot(checkCache("Lisa") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "initiate", note = "Successfully created lab w/o desc. Updated nlpStudio lablist")
    logTests(cls = cls, mthd = "getLab", note = "Successfully retrieved lab information")

    cat(paste("\n", test, " Completed: Success!\r"))
  }

  # Test 1: Confirm instantiation of lab with description
  test1 <- function() {
    test <- "test1"
    cat(paste("\r",test, " Commencing\r"))

    # Valid attempts; Check Lab
    bartLab <- Lab$new("Bart")
    nlpStudio$addLab(Bart, current = TRUE)
    barts <- bartLab$getLab()
    stopifnot(barts$name == "Bart")
    stopifnot(barts$desc == "Bart Lab")
    stopifnot(length(barts$collections) == 0)
    stopifnot((barts$modified - Sys.time()) < 1)
    stopifnot((barts$created - Sys.time()) < 1)

    # Valid attempts: Check NLPStudio
    stopifnot(all.equal(nlpStudio$currentLab, Bart))
    labs <- nlpStudio$listlabs()
    stopifnot(labs$name[[1]] == "Development")
    stopifnot(labs$desc[[1]] == "Development Lab")
    stopifnot(labs$name[[2]] == "Lisa")
    stopifnot(labs$desc[[2]] == "Lisa Lab")
    stopifnot(labs$name[[3]] == "Bart")
    stopifnot(labs$desc[[3]] == "Bart Lab")
    stopifnot((labs$created[[3]] - Sys.time()) < 1)
    stopifnot((labs$modified[[3]] - Sys.time()) < 1)

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)
    stopifnot(checkCache("Bart") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "initiate", note = "Successfully created lab with desc. Updated nlpStudio lablist")
    logTests(cls = cls, mthd = "getLab", note = "Successfully retrieved lab information")

    cat(paste("\n", test, " Completed: Success!\r"))
  }



init()
test0()
test1()
# test2()
# test3()
# test4()
# test5()
# test6()
# test7()
}

cls <- "Lab"
cacheFile <- "./.StudioCache.Rdata"

devtools::load_all()
testLab()
