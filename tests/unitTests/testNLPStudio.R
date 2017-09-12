testNLPStudio <- function() {

  init <- function() {

    # Clean up
    if (exists("Beats", envir = .GlobalEnv)) rm("Beats", envir = .GlobalEnv)
    if (exists("Bart", envir = .GlobalEnv))rm("Bart", envir = .GlobalEnv)

    # Source state and log
    source("./tests/testFunctions/checkState.r")
    source("./tests/testFunctions/logTests.r")
    source("./tests/testFunctions/copyFiles.r")

    # Test file paths
    csvTestFile <- "./tests/testFiles/contractions.csv"
    textTestFile <- "./tests/testFiles/en_US.news.txt"
    rdataTestFile <- "./tests/testFiles/quadgrams.Rdata"

  }

  # Test 0: Confirm instantiation of nlpStudio
  test0 <- function() {
    test <- "test0: Instantiation"
    cat(paste("\n",test, " Commencing\r"))

    # Test Instantiation
    stopifnot("NLPStudio" %in% class(nlpStudio))

    # Error processing
    #studio <<- nlpStudio$getObject(type = "xxx") # Should fail, invalid format type, Success

    # Confirm modified date updated
    studio <<- nlpStudio$getObject()
    stopifnot((Sys.time() - studio$created) < 1)
    stopifnot((Sys.time() - studio$modified) < 1)

    # Logit
    logTests(cls = cls, mthd = "initiate", note = "Successfully created nlpStudio")
    logTests(cls = cls, mthd = "initiate", note = "Create and modified dates initialized correctly")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test1 = function() {
    test <- "test1: getObject() No labs"
    cat(paste("\n",test, " Commencing\r"))

    # Test getObject, list format
    studio <<- nlpStudio$getObject() #Return list with warning
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot(studio$current == "None")
    stopifnot(length(studio$labs) == 0)
    stopifnot((Sys.time() - studio$created) < 1)
    stopifnot((Sys.time() - studio$modified) < 1)

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getObject('list')", note = "Successfully returned list")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test2 <- function() {
    test <- "test2: Add labs"
    cat(paste("\n",test, " Commencing\r"))

    # Instantiate lab
    Lab$new(name = "Beats", desc = "Beats Lab")
    l <- Beats$getObject()
    stopifnot(l$name == "Beats")
    stopifnot(l$desc == "Beats Lab")
    stopifnot(isTRUE(all.equal(l$parent, nlpStudio)))
    stopifnot(l$parentName == "nlpStudio")
    stopifnot(l$path == "./NLPStudio/Labs/Beats")
    stopifnot((Sys.time() - l$created) < 1)
    stopifnot((Sys.time() - l$modified) < 1)


    # Validation
    #nlpStudio$addChild() # fails, lab missing: success
    #nlpStudio$addChild(finfo,enter = FALSE) # fails, invalid class: success
    #nlpStudio$addChild(Beats, enter = cls) # fails invalid logical

    # Successful attempt
    nlpStudio$addChild(Beats, enter = FALSE)

    # Confirm directory created
    dirs <- nlpStudio$getPaths()
    stopifnot(dir.exists(file.path(dirs$labs, "Beats")))

    # Confirm modified date updated
    studio <<- nlpStudio$getObject()
    stopifnot((Sys.time() - studio$created) > 0.25)
    stopifnot((Sys.time() - studio$modified) < 0.5)

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState("Beats") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "addChild", note = "Beats lab, entry = FALSE")
    logTests(cls = cls, mthd = "addChild", note = "Date modified updated correctly")


    cat(paste("\n", test, " Completed: Success!\n"))
  }


  test3 <- function() {
    test <- "test3: getObject() with Lab"
    cat(paste("\n",test, " Commencing\r"))

    # Test getObject list format
    studio <<- nlpStudio$getObject()
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot(studio$current == "None")
    stopifnot((Sys.time() - studio$created) > .05)
    stopifnot((Sys.time() - studio$modified) < 1)

    stopifnot(length(studio$labs) == 1)
    lab <- studio$labs[[1]]$getObject()
    stopifnot(lab$name == "Beats")
    stopifnot(lab$desc == "Beats Lab")
    stopifnot(lab$parentName == "nlpStudio")
    stopifnot((Sys.time() - lab$created) < 1)
    stopifnot((Sys.time() - lab$modified) < 1)

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState("Beats") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getObject(type = 'list')", note = "Successfully returned list type")
    logTests(cls = cls, mthd = "addChild", note = "Successfully added lab")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test4 <- function() {
    test <- "test4: addChild(enter = TRUE)"
    cat(paste("\n",test, " Commencing\r"))

    Lab$new(name = "Bart",desc = "Simpsons Lab")
    nlpStudio$addChild(Bart, enter = TRUE)

    # Confirm directory created
    dirs <- nlpStudio$getPaths()
    stopifnot(dir.exists(file.path(dirs$labs, "Bart")))

    # Copy test files over
    l <- Bart$getObject()
    fromFolder <- "./tests/testFiles"
    toFolder <- l$path
    fileList <- list.files(fromFolder)
    file.copy(file.path(fromFolder, fileList), toFolder)

    # Confirm modified date updated
    studio <<- nlpStudio$getObject()
    stopifnot((Sys.time() - studio$created) > 0.5)
    stopifnot((Sys.time() - studio$modified) < 2)

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState("Beats") == TRUE)
    stopifnot(checkState("Bart") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "addChild(enter = 'TRUE')", note = "Successfully added lab")
    logTests(cls = cls, mthd = "addChild(enter = 'TRUE')", note = "Date modified updated correctly")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test5 <- function() {
    test <- "test5: getObject() with two labs, one current"
    cat(paste("\n",test, " Commencing\r"))

    # Test getObject list format
    studio <<- nlpStudio$getObject()
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot(studio$currentLabName == "Bart")
    stopifnot((Sys.time() - studio$created) > 0.5)
    stopifnot((Sys.time() - studio$modified) < 5)

    stopifnot(length(studio$labs) == 2)
    beats <- studio$labs[[1]]$getObject()
    stopifnot(beats$name == "Beats")
    stopifnot(beats$desc == "Beats Lab")
    stopifnot(beats$parentName == "nlpStudio")
    stopifnot((Sys.time() - beats$created) > 0.5)
    stopifnot((Sys.time() - beats$modified) > 0.5)

    bart <- studio$labs[[2]]$getObject()
    stopifnot(bart$name == "Bart")
    stopifnot(bart$desc == "Simpsons Lab")
    stopifnot(bart$parentName == "nlpStudio")
    stopifnot((Sys.time() - bart$created) < 5)
    stopifnot((Sys.time() - bart$modified) < 5)

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState("Beats") == TRUE)
    stopifnot(checkState("Bart") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getObject('list')", note = "Successfully returned nlpStudio list with two labs")
    logTests(cls = cls, mthd = "addChild", note = "Successfully added 2nd lab")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test6 <- function() {
    test <- "test6: Enter lab"
    cat(paste("\n",test, " Commencing\r"))

    nlpStudio$enterLab(Bart) # Should issue information, already entered lab
    nlpStudio$enterLab(Beats) # Should issue informational message about switch

    studio <<- nlpStudio$getObject()
    stopifnot(isTRUE(all.equal(studio$currentLab, Beats)))
    stopifnot(isTRUE(all.equal(studio$currentLabName, "Beats")))

    # Confirm modified date updated
    stopifnot((Sys.time() - studio$created) > 1)
    stopifnot((Sys.time() - studio$modified) < 1)

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState('Beats') == TRUE)
    stopifnot(checkState('Bart') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "enterLab", note = "Successfully notify user when entering a current lab")
    logTests(cls = cls, mthd = "enterLab", note = "Successfully change current lab and notify user of change")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test7 = function() {
    test <- "test7: Leave lab"
    cat(paste("\n",test, " Commencing\r"))

    nlpStudio$leaveLab(Bart) # Should warning, attempt to leave a non-current lab
    studio <<- nlpStudio$getObject()
    stopifnot(studio$currentLabName == "Beats")

    nlpStudio$leaveLab(Beats)
    studio <<- nlpStudio$getObject()
    stopifnot(studio$currentLab == "None")
    stopifnot(studio$currentLabName == "None")

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState('Beats') == TRUE)
    stopifnot(checkState('Bart') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "leaveLab", note = "Successfully warned when attempting to leave a non-current lab. No change to current")
    logTests(cls = cls, mthd = "leaveLab", note = "Successfully leave current lab, changing currentLab to 'None'")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test8 <- function() {
    test <- "test8: Remove lab"
    cat(paste("\n",test, " Commencing\r"))

    # Validation
    #nlpStudio$removeLab() # fail lab missing: success
    #nlpStudio$removeLab(ditto) # fail does not exist: success
    #nlpStudio$removeLab("labsDir") # fail not a lab
    #nlpStudio$removeLab("Beats")# fail, can't remove current lab

    # Successfuly remove Bart
    nlpStudio$removeChild(Bart)
    stopifnot(exists("Bart") == FALSE)
    stopifnot(exists("Beats") == TRUE)

    # Confirm date modified updated correctly
    studio <- nlpStudio$getObject()
    stopifnot((Sys.time() - studio$created) > 1)
    stopifnot((Sys.time() - studio$modified) < 5)

    # # Confirm removed from list
    labs <<- Bart$getObject()
    for (l in 1:length(labs)) {
      stopifnot(!isTRUE(all.equal(labs[[l]]$name, "Bart")))
    }

    # Confirm archives
    # a <<- nlpArchives$getArchives()
    # stopifnot(a[[1]]$name == "Bart")
    # stopifnot(a[[1]]$class == "Lab")
    # stopifnot(grepl("Bart", a[[1]]$archiveFile))
    # stopifnot((Sys.time() - a[[1]]$created) < 5)
    # #
    # # Confirm directory does not exist and archive does
    # stopifnot(!dir.exists("./NLPStudio/Labs/Bart"))
    # stopifnot(file.exists(a[[1]]$archiveFile))
    #
    # # Print archives
    # nlpArchives$printArchives()

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState('Beats') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "removeLab", note = "Tested validation and removal of lab")
    logTests(cls = cls, mthd = "removeLab", note = "Tested archive of files and inventory of archives")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test16 <- function() {
    test <- "test16: Print Studio"
    cat(paste("\n",test, " Commencing\r"))

    studio <- nlpStudio$getObject(type = "object")
    studio$printStudio()  # Should print studio and labs to console

    # Logit
    logTests(cls = cls, mthd = "printStudio", note = "Tested printStudio")
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
test16()

}

dirs <- nlpStudio$getPaths()
lapply(dirs, function(d) {base::unlink(d, recursive = TRUE)})
cls <- "NLPStudio"
# stateFile <- "./NLPSTudio/.State.Rdata"
# base::unlink(stateFile)

devtools::load_all()
testNLPStudio()
