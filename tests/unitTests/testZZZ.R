testZZZ <- function() {

  # Test 0: Test initial launch without cache.
  test0 <- function() {
    test <- "test0"
    cat(paste("\r",test, " Commencing\r"))

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)

    # Logit
    logTests(cls = "zzz", mthd = "NA", note = "Test cache of initial NLPStudio")

    cat(paste("\n", test, " Completed: Success!\r"))
  }




test0()
#test1()
# test2()
# test3()
# # test4()
# test5()
# test6()
# test7()
}

now <- Sys.time()
labsDir <- "./Labs"
archiveDir <- "./Archive"
cacheFile <- "./.StudioCache.Rdata"

base::unlink(labsDir, recursive = FALSE)
base::unlink(archiveDir, recursive = TRUE)
#base::unlink(cacheFile)

devtools::load_all()

testNLPStudio()
