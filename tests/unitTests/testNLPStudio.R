testNLPStudio <- function() {

  init <- function() {

    # Clean up
    if (exists("Development", envir = .GlobalEnv)) rm("Development", envir = .GlobalEnv)
    if (exists("Bart", envir = .GlobalEnv))rm("Bart", envir = .GlobalEnv)

    # Source cache and log
    source("./tests/checkCache.r")
    source("./tests/logTests.r")
  }

  # Test 0: Confirm instantiation of nlpStudio
  test0 <- function() {
    test <- "test0: Instantiation"
    cat(paste("\n",test, " Commencing\r"))

    # Test Instantiation
    stopifnot("NLPStudio" %in% class(nlpStudio))

    # Error processing
    #studio <<- nlpStudio$getStudio(format = "xxx") # Should fail, invalid format type, Success

    # Confirm modified date updated
    studio <<- nlpStudio$getStudio(format = "list")
    stopifnot((Sys.time() - studio$created) < 1)
    stopifnot((Sys.time() - studio$modified) < 1)

    # Logit
    logTests(cls = cls, mthd = "initiate", note = "Successfully created nlpStudio")
    logTests(cls = cls, mthd = "initiate", note = "Create and modified dates initialized correctly")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test1 = function() {
    test <- "test1: getStudio('object')"
    cat(paste("\n",test, " Commencing\r"))

    Sys.sleep(2)

    # Test getStudio object format
    studio <<- nlpStudio$getStudio(format = "object")
    stopifnot(isTRUE(all.equal(studio, nlpStudio)))

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getStudio('object')", note = "Successfully returned object.")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test2 = function() {
    test <- "test2: getStudio('list') No labs"
    cat(paste("\n",test, " Commencing\r"))

    # Test getStudio, list format
    studio <<- nlpStudio$getStudio(format = "list")
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot(studio$current == "None")
    stopifnot(length(studio$labs) == 0)
    stopifnot((Sys.time() - studio$created) > 1)
    stopifnot((Sys.time() - studio$modified) > 1)

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getStudio('list')", note = "Successfully returned list")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test3 = function() {
    test <- "test3: getStudio('df') No labs"
    cat(paste("\n",test, " Commencing\r"))

    # Test getStudio, data frame
    studio <<- nlpStudio$getStudio(format = "df")
    stopifnot(nrow(studio$studioDf) == 1)
    stopifnot(studio$studioDf$name[1] == "nlpStudio")
    stopifnot(studio$studioDf$desc[1] == "NLPStudio: Natural Language Processing Studio")
    stopifnot(studio$studioDf$current[1] == "None")
    stopifnot((Sys.time() - studio$studioDf$created[1]) > 1)
    stopifnot((Sys.time() - studio$studioDf$modified[1]) > 1)
    stopifnot(nrow(studio$labsDf) == 0)

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getStudio(format = 'df')", note = "Data frame returned successfully")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test4 <- function() {
    test <- "test4: Add labs"
    cat(paste("\n",test, " Commencing\r"))

    Lab$new(name = "Development", desc = "Development Lab")
    nlpStudio$addLab(Development, enter = FALSE)

    # Confirm modified date updated
    studio <<- nlpStudio$getStudio(format = "list")
    stopifnot((Sys.time() - studio$created) > 1)
    stopifnot((Sys.time() - studio$modified) < 1)

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)
    stopifnot(checkCache("Development") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "addLab", note = "Development lab, entry = FALSE")
    logTests(cls = cls, mthd = "addLab", note = "Date modified updated correctly")


    cat(paste("\n", test, " Completed: Success!\n"))
  }


  test5 <- function() {
    test <- "test5: getStudio('object') with Lab"
    cat(paste("\n",test, " Commencing\r"))

    studio <<- nlpStudio$getStudio(format = "object")
    stopifnot(isTRUE(all.equal(studio, nlpStudio)))

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)
    stopifnot(checkCache("Development") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab(format = 'object')", note = "Successfully returned object type")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test6 <- function() {
    test <- "test6: getStudio('list') with Lab"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab list format
    studio <<- nlpStudio$getStudio(format = "list")
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot(studio$current == "None")
    stopifnot((Sys.time() - studio$created) > 1)
    stopifnot((Sys.time() - studio$modified) < 1)

    stopifnot(length(studio$labs) == 1)
    stopifnot(studio$labs[[1]]$name == "Development")
    stopifnot(studio$labs[[1]]$desc == "Development Lab")
    stopifnot((Sys.time() - studio$labs[[1]]$created) < 1)
    stopifnot((Sys.time() - studio$labs[[1]]$modified) < 1)

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)
    stopifnot(checkCache("Development") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab(format = 'list')", note = "Successfully returned list type")
    logTests(cls = cls, mthd = "addLab", note = "Successfully added lab")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test7 <- function() {
    test <- "test7: getStudio('df') with Lab"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab df format
    studio <<- nlpStudio$getStudio(format = "df")
    stopifnot(studio$studioDf$name == "nlpStudio")
    stopifnot(studio$studioDf$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot(studio$studioDf$current == "None")
    stopifnot((Sys.time() - studio$studioDf$created) > 1)
    stopifnot((Sys.time() - studio$studioDf$modified) < 1)

    stopifnot(nrow(studio$labs) == 1)
    stopifnot(studio$labsDf$name[1] == "Development")
    stopifnot(studio$labsDf$desc[1] == "Development Lab")
    stopifnot((Sys.time() - studio$labsDf$created[1]) < 1)
    stopifnot((Sys.time() - studio$labsDf$modified[1]) < 1)

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)
    stopifnot(checkCache("Development") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab(format = 'df')", note = "Successfully returned df type")
    logTests(cls = cls, mthd = "addLab", note = "Successfully added lab")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test8 <- function() {
    test <- "test8: addLab(enter = TRUE)"
    cat(paste("\n",test, " Commencing\r"))

    Lab$new(name = "Bart",desc = "Simpsons Lab")
    nlpStudio$addLab(Bart, enter = TRUE)

    # Confirm modified date updated
    studio <<- nlpStudio$getStudio(format = "list")
    stopifnot((Sys.time() - studio$created) > 1)
    stopifnot((Sys.time() - studio$modified) < 1)

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)
    stopifnot(checkCache("Development") == TRUE)
    stopifnot(checkCache("Bart") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "addLab(enter = 'TRUE')", note = "Successfully added lab")
    logTests(cls = cls, mthd = "addLab(enter = 'TRUE')", note = "Date modified updated correctly")

    cat(paste("\n", test, " Completed: Success!\n"))
  }


  test9 <- function() {
    test <- "test9: getLab('object') with two labs, one current"
    cat(paste("\n",test, " Commencing\r"))

    studio <<- nlpStudio$getStudio(format = "object")
    stopifnot(isTRUE(all.equal(studio, nlpStudio)))

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)
    stopifnot(checkCache("Development") == TRUE)
    stopifnot(checkCache("Bart") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getStudio('object')", note = "Successfully returned nlpStudio object")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test10 <- function() {
    test <- "test10: getLab('list') with two labs, one current"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab list format
    studio <<- nlpStudio$getStudio(format = "list")
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot(isTRUE(all.equal(studio$currentLab, "Bart")))
    stopifnot((Sys.time() - studio$created) > 1)
    stopifnot((Sys.time() - studio$modified) < 1)

    stopifnot(length(studio$labs) == 2)
    stopifnot(studio$labs[[1]]$name == "Development")
    stopifnot(studio$labs[[1]]$desc == "Development Lab")
    stopifnot((Sys.time() - studio$labs[[1]]$created) > 0.5)
    stopifnot((Sys.time() - studio$labs[[1]]$modified) > 0.5)

    stopifnot(studio$labs[[2]]$name == "Bart")
    stopifnot(studio$labs[[2]]$desc == "Simpsons Lab")
    stopifnot((Sys.time() - studio$labs[[2]]$created) < 1)
    stopifnot((Sys.time() - studio$labs[[2]]$modified) < 1)

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)
    stopifnot(checkCache("Development") == TRUE)
    stopifnot(checkCache("Bart") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getStudio('list')", note = "Successfully returned nlpStudio list with two labs")
    logTests(cls = cls, mthd = "addLab", note = "Successfully added 2nd lab")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test11 <- function() {
    test <- "test11: getLab('df') with two labs, one current"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab df format
    studio <<- nlpStudio$getStudio(format = "df")
    stopifnot(studio$studioDf$name == "nlpStudio")
    stopifnot(studio$studioDf$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot(isTRUE(all.equal(studio$studioDf$currentLab, "Bart")))
    stopifnot((Sys.time() - studio$studioDf$created) > 1)
    stopifnot((Sys.time() - studio$studioDf$modified) < 1)

    stopifnot(nrow(studio$labsDf) == 2)
    stopifnot(studio$labsDf$name[1] == "Development")
    stopifnot(studio$labsDf$desc[1] == "Development Lab")
    stopifnot((Sys.time() - studio$labsDf$created[1]) > 0.5)
    stopifnot((Sys.time() - studio$labsDf$modified[1]) > 0.5)

    stopifnot(studio$labsDf$name[2] == "Bart")
    stopifnot(studio$labsDf$desc[2] == "Simpsons Lab")
    stopifnot((Sys.time() - studio$labsDf$created[2]) < 1)
    stopifnot((Sys.time() - studio$labsDf$modified[2]) < 1)

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)
    stopifnot(checkCache('Development') == TRUE)
    stopifnot(checkCache('Bart') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getStudio('df')", note = "Successfully returned df type")
    logTests(cls = cls, mthd = "addLab", note = "Successfully added 2nd lab")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test12 <- function() {
    test <- "test12: Enter lab"
    cat(paste("\n",test, " Commencing\r"))

    Bart$enterLab() # Should issue information, already entered lab
    Development$enterLab() # Should issue informational message about switch

    # Confirm modified date updated
    studio <<- nlpStudio$getStudio(format = "list")
    stopifnot((Sys.time() - studio$created) > 1)
    stopifnot((Sys.time() - studio$modified) < 1)

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)
    stopifnot(checkCache('Development') == TRUE)
    stopifnot(checkCache('Bart') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "enterLab", note = "Successfully notify user when entering a current lab")
    logTests(cls = cls, mthd = "enterLab", note = "Successfully change current lab and notify user of change")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test13 <- function() {
    test <- "test13: Confirm current lab changed"
    cat(paste("\n",test, " Commencing\r"))

    studio <<- nlpStudio$getStudio(format = "list")
    stopifnot(isTRUE(all.equal(studio$currentLab, "Development")))

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)
    stopifnot(checkCache('Development') == TRUE)
    stopifnot(checkCache('Bart') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "enterLab", note = "Successfully change current lab and notify user of change")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test14 = function() {
    test <- "test14: Leave lab"
    cat(paste("\n",test, " Commencing\r"))

    Bart$leaveLab() # Should warning, attempt to leave a non-current lab
    studio <<- nlpStudio$getStudio(format = "list")
    stopifnot(isTRUE(all.equal(studio$currentLab, "Development")))

    Development$leaveLab()
    studio <<- nlpStudio$getStudio(format = "list")
    stopifnot(isTRUE(all.equal(studio$currentLab, "None")))

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)
    stopifnot(checkCache('Development') == TRUE)
    stopifnot(checkCache('Bart') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "leaveLab", note = "Successfully warned when attempting to leave a non-current lab. No change to current")
    logTests(cls = cls, mthd = "leaveLab", note = "Successfully leave current lab, changing currentLab to 'None'")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  # Test 5: Test remove lab
  test15 <- function() {
    test <- "test15: Remove lab"
    cat(paste("\n",test, " Commencing\r"))

    # Attempt to remove a non-existent lab ( a function): Success
    #nlpStudio$removeLab(logTests)

    # Attempt to remove a current lab. should fail
    Development$enterLab()
    #nlpStudio$removeLab(Development)# Success

    # Successfuly remove Bart
    nlpStudio$removeLab(Bart)
    stopifnot(exists("Bart") == FALSE)
    stopifnot(exists("Development") == TRUE)

    # Confirm date modified updated correctly
    studio <- nlpStudio$getStudio()
    stopifnot((Sys.time() - studio$created) > 1)
    stopifnot((Sys.time() - studio$modified) < 1)

    # # Confirm removed from list
    labs <<- nlpStudio$getLabs(format = "list")
    for (l in 1:length(labs)) {
      stopifnot(!isTRUE(all.equal(labs[[l]]$name, "Bart")))
    }

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)
    stopifnot(checkCache('Development') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "removeLab", note = "Tested validation and removal of lab")
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
test7()
test8()
test9()
test10()
test11()
test12()
test13()
test14()
test15()


}

cls <- "NLPStudio"
labsDir <- "./Labs"
archiveDir <- "./Archive"
nlpStudioFile <- "./NLPStudio.Rdata"
cacheFile <- "./.StudioCache.Rdata"

base::unlink(labsDir, recursive = FALSE)
base::unlink(archiveDir, recursive = TRUE)
base::unlink(nlpStudioFile)
base::unlink(cacheFile)

devtools::load_all()
testNLPStudio()
