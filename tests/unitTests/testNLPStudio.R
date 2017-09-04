testNLPStudio <- function() {

  init <- function() {

    # Clean up
    if (exists("Development", envir = .GlobalEnv)) rm("Development", envir = .GlobalEnv)
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
    #studio <<- nlpStudio$getStudio(type = "xxx") # Should fail, invalid format type, Success

    # Confirm modified date updated
    studio <<- nlpStudio$getStudio(type = "list")
    stopifnot((Sys.time() - studio$metaData$created) < 1)
    stopifnot((Sys.time() - studio$metaData$modified) < 1)

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
    studio <<- nlpStudio$getStudio(type = "object")
    stopifnot(isTRUE(all.equal(studio, nlpStudio)))

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getStudio('object')", note = "Successfully returned object.")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test2 = function() {
    test <- "test2: getStudio('list') No labs"
    cat(paste("\n",test, " Commencing\r"))

    # Test getStudio, list format
    studio <<- nlpStudio$getStudio(type = "xxx") #Return list with warning
    stopifnot(studio$metaData$name == "nlpStudio")
    stopifnot(studio$metaData$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot(studio$metaData$current == "None")
    stopifnot(length(studio$labs) == 0)
    stopifnot((Sys.time() - studio$metaData$created) > 1)
    stopifnot((Sys.time() - studio$metaData$modified) > 1)

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getStudio('list')", note = "Successfully returned list")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test3 = function() {
    test <- "test3: getStudio('df') No labs"
    cat(paste("\n",test, " Commencing\r"))

    # Test getStudio, data frame
    studio <<- nlpStudio$getStudio(type = "df")
    stopifnot(nrow(studio$metaData) == 1)
    stopifnot(studio$metaData$name[1] == "nlpStudio")
    stopifnot(studio$metaData$desc[1] == "NLPStudio: Natural Language Processing Studio")
    stopifnot(studio$metaData$current[1] == "None")
    stopifnot((Sys.time() - studio$metaData$created[1]) > 1)
    stopifnot((Sys.time() - studio$metaData$modified[1]) > 1)
    stopifnot(nrow(studio$labs) == 0)

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getStudio(type = 'df')", note = "Data frame returned successfully")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test4 <- function() {
    test <- "test4: Add labs"
    cat(paste("\n",test, " Commencing\r"))

    # Instantiate lab
    Lab$new(name = "Development", desc = "Development Lab")


    # Validation
    #nlpStudio$addLab() # fails, lab missing: success
    #nlpStudio$addLab(finfo,enter = FALSE) # fails, invalid class: success
    #nlpStudio$addLab(Development, enter = cls) # fails invalid logical

    # Successful attempt
    nlpStudio$addLab(Development, enter = FALSE)

    # Confirm directory created
    dirs <- nlpStudio$getDirectories()
    stopifnot(dir.exists(file.path(dirs$labs, "Development")))

    # Confirm modified date updated
    studio <<- nlpStudio$getStudio(type = "list")
    stopifnot((Sys.time() - studio$metaData$created) > 1)
    stopifnot((Sys.time() - studio$metaData$modified) < 1)

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState("Development") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "addLab", note = "Development lab, entry = FALSE")
    logTests(cls = cls, mthd = "addLab", note = "Date modified updated correctly")


    cat(paste("\n", test, " Completed: Success!\n"))
  }


  test5 <- function() {
    test <- "test5: getStudio('object') with Lab"
    cat(paste("\n",test, " Commencing\r"))

    studio <<- nlpStudio$getStudio(type = "object")
    stopifnot(isTRUE(all.equal(studio, nlpStudio)))

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState("Development") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab(type = 'object')", note = "Successfully returned object type")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test6 <- function() {
    test <- "test6: getStudio('list') with Lab"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab list format
    studio <<- nlpStudio$getStudio(type = "list")
    stopifnot(studio$metaData$name == "nlpStudio")
    stopifnot(studio$metaData$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot(studio$metaData$current == "None")
    stopifnot((Sys.time() - studio$metaData$created) > 1)
    stopifnot((Sys.time() - studio$metaData$modified) < 1)

    stopifnot(length(studio$labs) == 1)
    stopifnot(studio$labs[[1]]$metaData$name == "Development")
    stopifnot(studio$labs[[1]]$metaData$desc == "Development Lab")
    stopifnot(studio$labs[[1]]$metaData$parentName == "nlpStudio")
    stopifnot((Sys.time() - studio$labs[[1]]$metaData$created) < 1)
    stopifnot((Sys.time() - studio$labs[[1]]$metaData$modified) < 1)

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState("Development") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab(type = 'list')", note = "Successfully returned list type")
    logTests(cls = cls, mthd = "addLab", note = "Successfully added lab")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test7 <- function() {
    test <- "test7: getStudio('df') with Lab"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab df format
    studio <<- nlpStudio$getStudio(type = "df")
    stopifnot(studio$metaData$name == "nlpStudio")
    stopifnot(studio$metaData$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot(studio$metaData$current == "None")
    stopifnot((Sys.time() - studio$metaData$created) > 1)
    stopifnot((Sys.time() - studio$metaData$modified) < 1)

    stopifnot(nrow(studio$labs) == 1)
    stopifnot(studio$labs$metaData$name[1] == "Development")
    stopifnot(studio$labs$metaData$desc[1] == "Development Lab")
    stopifnot(studio$labs[1]$metaData$parentName == "nlpStudio")
    stopifnot((Sys.time() - studio$labs$metaData$created[1]) < 1)
    stopifnot((Sys.time() - studio$labs$metaData$modified[1]) < 1)

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState("Development") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab(type = 'df')", note = "Successfully returned df type")
    logTests(cls = cls, mthd = "addLab", note = "Successfully added lab")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test8 <- function() {
    test <- "test8: addLab(enter = TRUE)"
    cat(paste("\n",test, " Commencing\r"))

    Lab$new(name = "Bart",desc = "Simpsons Lab")
    nlpStudio$addLab(Bart, enter = TRUE)

    # Confirm directory created
    dirs <- nlpStudio$getDirectories()
    stopifnot(dir.exists(file.path(dirs$labs, "Bart")))

    # Copy test files over
    l <- Bart$getLab(type = "list")
    fromFolder <- "./tests/testFiles"
    toFolder <- l$metaData$path
    fileList <- list.files(fromFolder)
    file.copy(file.path(fromFolder, fileList), toFolder)

    # Confirm modified date updated
    studio <<- nlpStudio$getStudio(type = "list")
    stopifnot((Sys.time() - studio$metaData$created) > 1)
    stopifnot((Sys.time() - studio$metaData$modified) < 1.5)

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState("Development") == TRUE)
    stopifnot(checkState("Bart") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "addLab(enter = 'TRUE')", note = "Successfully added lab")
    logTests(cls = cls, mthd = "addLab(enter = 'TRUE')", note = "Date modified updated correctly")

    cat(paste("\n", test, " Completed: Success!\n"))
  }


  test9 <- function() {
    test <- "test9: getLab('object') with two labs, one current"
    cat(paste("\n",test, " Commencing\r"))

    studio <<- nlpStudio$getStudio(type = "object")
    stopifnot(isTRUE(all.equal(studio, nlpStudio)))

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState("Development") == TRUE)
    stopifnot(checkState("Bart") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getStudio('object')", note = "Successfully returned nlpStudio object")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test10 <- function() {
    test <- "test10: getLab('list') with two labs, one current"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab list format
    studio <<- nlpStudio$getStudio(type = "list")
    stopifnot(studio$metaData$name == "nlpStudio")
    stopifnot(studio$metaData$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot(studio$currentLab == "Bart")
    stopifnot((Sys.time() - studio$created) > 1)
    stopifnot((Sys.time() - studio$modified) < 5)

    stopifnot(length(studio$labs) == 2)
    stopifnot(studio$labs[[1]]$name == "Development")
    stopifnot(studio$labs[[1]]$desc == "Development Lab")
    stopifnot(studio$labs[[1]]$metaData$parentName == "nlpStudio")
    stopifnot((Sys.time() - studio$labs[[1]]$created) > 0.5)
    stopifnot((Sys.time() - studio$labs[[1]]$modified) > 0.5)

    stopifnot(studio$labs[[2]]$name == "Bart")
    stopifnot(studio$labs[[2]]$desc == "Simpsons Lab")
    stopifnot(studio$labs[[2]]$metaData$parentName == "nlpStudio")
    stopifnot((Sys.time() - studio$labs[[2]]$created) < 5)
    stopifnot((Sys.time() - studio$labs[[2]]$modified) < 5)

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState("Development") == TRUE)
    stopifnot(checkState("Bart") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getStudio('list')", note = "Successfully returned nlpStudio list with two labs")
    logTests(cls = cls, mthd = "addLab", note = "Successfully added 2nd lab")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test11 <- function() {
    test <- "test11: getLab('df') with two labs, one current"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab df format
    studio <<- nlpStudio$getStudio(type = "df")
    stopifnot(studio$metaData$name == "nlpStudio")
    stopifnot(studio$metaData$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot(isTRUE(all.equal(studio$metaData$currentLab, "Bart")))
    stopifnot((Sys.time() - studio$metaData$created) > 1)
    stopifnot((Sys.time() - studio$metaData$modified) < 5)

    stopifnot(nrow(studio$labs) == 2)
    stopifnot(studio$labs$name[1] == "Development")
    stopifnot(studio$labs$desc[1] == "Development Lab")
    stopifnot(studio$labs[1]$metaData$parentName == "nlpStudio")
    stopifnot((Sys.time() - studio$labs$created[1]) > 0.5)
    stopifnot((Sys.time() - studio$labs$modified[1]) > 0.5)

    stopifnot(studio$labs$name[2] == "Bart")
    stopifnot(studio$labs$desc[2] == "Simpsons Lab")
    stopifnot(studio$labs[1]$metaData$parentName == "nlpStudio")
    stopifnot((Sys.time() - studio$labs$created[2]) < 5)
    stopifnot((Sys.time() - studio$labs$modified[2]) < 5)

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState('Development') == TRUE)
    stopifnot(checkState('Bart') == TRUE)

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
    studio <<- nlpStudio$getStudio(type = "list")
    stopifnot((Sys.time() - studio$created) > 1)
    stopifnot((Sys.time() - studio$modified) < 1)

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState('Development') == TRUE)
    stopifnot(checkState('Bart') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "enterLab", note = "Successfully notify user when entering a current lab")
    logTests(cls = cls, mthd = "enterLab", note = "Successfully change current lab and notify user of change")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test13 <- function() {
    test <- "test13: Confirm current lab changed"
    cat(paste("\n",test, " Commencing\r"))

    studio <<- nlpStudio$getStudio(type = "list")
    stopifnot(studio$metaData$currentLab == "Development")

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState('Development') == TRUE)
    stopifnot(checkState('Bart') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "enterLab", note = "Successfully change current lab and notify user of change")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test14 = function() {
    test <- "test14: Leave lab"
    cat(paste("\n",test, " Commencing\r"))

    Bart$leaveLab() # Should warning, attempt to leave a non-current lab
    studio <<- nlpStudio$getStudio(type = "list")
    stopifnot(studio$metaData$currentLab == "Development")

    Development$leaveLab()
    studio <<- nlpStudio$getStudio(type = "list")
    stopifnot(studio$metaData$currentLab == "None")

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState('Development') == TRUE)
    stopifnot(checkState('Bart') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "leaveLab", note = "Successfully warned when attempting to leave a non-current lab. No change to current")
    logTests(cls = cls, mthd = "leaveLab", note = "Successfully leave current lab, changing currentLab to 'None'")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test15 <- function() {
    test <- "test15: Remove lab"
    cat(paste("\n",test, " Commencing\r"))

    # Validation
    #nlpStudio$removeLab() # fail lab missing: success
    #nlpStudio$removeLab(ditto) # fail does not exist: success
    #nlpStudio$removeLab("labsDir") # fail not a lab
    Development$enterLab()
    #nlpStudio$removeLab("Development")# fail, can't remove current lab

    # Successfuly remove Bart
    nlpStudio$removeLab(Bart, purge = TRUE)
    stopifnot(exists("Bart") == FALSE)
    stopifnot(exists("Development") == TRUE)

    # Confirm date modified updated correctly
    studio <- nlpStudio$getStudio(type = "list")
    stopifnot((Sys.time() - studio$metaData$created) > 1)
    stopifnot((Sys.time() - studio$metaData$modified) < 5)

    # # Confirm removed from list
    labs <<- nlpStudio$getLabs(type = "list")
    for (l in 1:length(labs)) {
      stopifnot(!isTRUE(all.equal(labs[[l]]$metaData$name, "Bart")))
    }

    # Confirm archives
    a <<- nlpArchives$getArchives(type = "list")
    stopifnot(a[[1]]$name == "Bart")
    stopifnot(a[[1]]$class == "Lab")
    stopifnot(grepl("Bart", a[[1]]$archiveFile))
    stopifnot((Sys.time() - a[[1]]$created) < 5)
    #
    # Confirm directory does not exist and archive does
    stopifnot(!dir.exists("./NLPStudio/Labs/Bart"))
    stopifnot(file.exists(a[[1]]$archiveFile))

    # Print archives
    nlpArchives$printArchives()

    # Check state
    stopifnot(checkState("nlpStudio") == TRUE)
    stopifnot(checkState('Development') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "removeLab", note = "Tested validation and removal of lab")
    logTests(cls = cls, mthd = "removeLab", note = "Tested archive of files and inventory of archives")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test16 <- function() {
    test <- "test16: Print Studio"
    cat(paste("\n",test, " Commencing\r"))

    studio <- nlpStudio$getStudio(type = "object")
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

dirs <- nlpStudio$getDirectories()
lapply(dirs, function(d) {base::unlink(d, recursive = TRUE)})
cls <- "NLPStudio"
stateFile <- "./NLPSTudio/.State.Rdata"
base::unlink(stateFile)

devtools::load_all()
testNLPStudio()
