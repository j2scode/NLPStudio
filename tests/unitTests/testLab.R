testLab <- function() {
  init <- function() {

    # Clean up
    if (exists("blue", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("blue", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
    if (exists("brown", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("brown", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
    if (exists("oxford", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("oxford", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
    if (exists("penn", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("penn", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
    if (exists("stanford", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("stanford", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }

    if (dir.exists("./NLPStudio/Labs/blue") == TRUE) base::unlink("./NLPStudio/Labs/blue", recursive = TRUE)

    # Source state and log
    source("./tests/testFunctions/checkState.r")
    source("./tests/testFunctions/logTests.r")
    source("./tests/testFunctions/copyFiles.r")
  }

  # Test 0: Confirm instantiation of lab
  test0 <- function() {
    test <- "test0: Lab Instantiation"
    cat(paste("\n",test, " Commencing\r"))

    # Test Instantiation
    # lab$metaData$new() # should fail, name is required: Success
    lab <<- Lab$new(name = "blue", "Blue Lab")
    #lab$metaData$new(name = "blue", "Blue Lab") # Error lab exists and directory already exists: success
    stopifnot("Lab" %in% class(blue))
    stopifnot(dir.exists("./NLPStudio/Labs/blue"))

    # Confirm instantiation
    lab <<- blue$getLab(type = "list")
    stopifnot((Sys.time() - lab$metaData$created) < 1)
    stopifnot((Sys.time() - lab$metaData$modified) < 1)

    # Confirm directory created
    stopifnot(dir.exists("./NLPStudio/Labs/blue"))

    # Logit
    logTests(cls = cls, mthd = "initiate", note = "Successfully created lab")
    logTests(cls = cls, mthd = "initiate", note = "Create and modified dates initialized correctly")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test1 = function() {
    test <- "test1: getLab('object'), no documents"
    cat(paste("\n",test, " Commencing\r"))

    Sys.sleep(2)

    # Test getLab object format
    lab <<- blue$getLab(type = "object")
    stopifnot(isTRUE(all.equal(lab, blue)))
    stopifnot("Lab" %in% class(blue))

    # Check state
    stopifnot(checkState("blue") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab('object')", note = "Successfully returned object.")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test2 = function() {
    test <- "test2: getLab('list'), no documents"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab, list format
    lab <<- blue$getLab(type = "list")
    stopifnot(lab$metaData$name == "blue")
    stopifnot(lab$metaData$desc == "Blue Lab")
    stopifnot(lab$metaData$parent == "nlpStudio")
    stopifnot(lab$metaData$path == "./NLPStudio/Labs/blue")
    stopifnot(length(lab$documents) == 0)
    stopifnot((Sys.time() - lab$metaData$created) > 1)
    stopifnot((Sys.time() - lab$metaData$modified) > 1)

    # Check state
    stopifnot(checkState("blue") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab('list')", note = "Successfully returned list")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test3 = function() {
    test <- "test3: getLab('df') with no documents"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab, data frame
    lab <<- blue$getLab(type = "df")
    stopifnot(nrow(lab$metaData$labDf) == 1)
    stopifnot(lab$metaData$labDf$name[1] == "blue")
    stopifnot(lab$metaData$labDf$desc[1] == "Blue Lab")
    stopifnot((Sys.time() - lab$metaData$labDf$created[1]) > 1)
    stopifnot((Sys.time() - lab$metaData$labDf$modified[1]) > 1)
    stopifnot(nrow(lab$documentsDf) == 0)

    # Check state
    stopifnot(checkState("blue") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab(format = 'df')", note = "Data frame returned successfully")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test4 <- function() {
    test <- "test4: Add Collection"
    cat(paste("\n",test, " Commencing\r"))

    # Get lab name, which is also the path for the collection
    lab <<- blue$getLab(type = "list")
    path <- lab$metaData$name

    # Create and add collection
    DocumentCollection$new(name = "brown", parent = blue, desc = "Brown Corpus")
    stopifnot("DocumentCollection" %in% class(brown))
    stopifnot("Lab" %in% class(blue))
    blue$addDocument(brown)
    copyFiles("./NLPStudio/Labs/blue/brown")

    # Confirm directory created
    stopifnot(dir.exists("./NLPStudio/Labs/blue/brown"))

    # Confirm modified date updated
    lab <<- blue$getLab(type = "list")
    stopifnot((Sys.time() - lab$metaData$created) > 1)
    stopifnot((Sys.time() - lab$metaData$modified) < 1)

    # Check state
    stopifnot(checkState("blue") == TRUE)
    stopifnot(checkState("brown") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "addDocument", note = "Created collection and added to lab")
    logTests(cls = cls, mthd = "addDocument", note = "Date modified updated correctly")
    logTests(cls = cls, mthd = "addDocument", note = "Directory created")


    cat(paste("\n", test, " Completed: Success!\n"))
  }


  test5 <- function() {
    test <- "test5: getLab('object') with Collection"
    cat(paste("\n",test, " Commencing\r"))

    lab <<- blue$getLab(type = "object")
    stopifnot(isTRUE(all.equal(lab, blue)))
    stopifnot("Lab" %in% class(blue))

    # Check existence of documents
    documents <- blue$getDocuments(type = "object")
    for (i in length(documents)) {
      stopifnot("DocumentCollection" %in% class(documents[[i]])[1])
    }

    # Check state
    stopifnot(checkState("blue") == TRUE)
    stopifnot(checkState("brown") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab(format = 'object')", note = "Successfully returned object type")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test6 <- function() {
    test <- "test6: getLab('list') with Collection"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab list format
    lab <<- blue$getLab(type = "list")
    stopifnot(lab$metaData$name == "blue")
    stopifnot(lab$metaData$desc == "Blue Lab")
    stopifnot((Sys.time() - lab$metaData$created) > 1)
    stopifnot((Sys.time() - lab$metaData$modified) < 1.5)

    stopifnot(nrow(lab$documents) == 1)
    stopifnot(lab$documents[[1]]$name == "brown")
    stopifnot(lab$documents[[1]]$desc == "Brown Corpus")
    stopifnot((Sys.time() - lab$documents[[1]]$created) < 1.5)
    stopifnot((Sys.time() - lab$documents[[1]]$modified) < 1.5)

    # Check state
    stopifnot(checkState("blue") == TRUE)
    stopifnot(checkState("brown") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab(format = 'list')", note = "Successfully returned list type with added collection")
    logTests(cls = cls, mthd = "addDocument", note = "Successfully added collection")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test7 <- function() {
    test <- "test7: getLab('df') with Collection"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab df format
    lab <<- blue$getLab(type = "df")
    stopifnot(lab$metaData$labDf$name == "blue")
    stopifnot(lab$metaData$labDf$desc == "Blue Lab")
    stopifnot((Sys.time() - lab$metaData$labDf$created) > 1)
    stopifnot((Sys.time() - lab$metaData$labDf$modified) < 1)

    stopifnot(nrow(lab$documentsDf) == 1)
    stopifnot(lab$documentsDf$name[1] == "brown")
    stopifnot(lab$documentsDf$desc[1] == "Brown Corpus")
    stopifnot((Sys.time() - lab$documentsDf$created[1]) < 1)
    stopifnot((Sys.time() - lab$documentsDf$modified[1]) < 1)

    # Check state
    stopifnot(checkState("blue") == TRUE)
    stopifnot(checkState("brown") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab(format = 'df')", note = "Successfully returned df type")
    logTests(cls = cls, mthd = "addDocument", note = "Successfully added collection")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test8 <- function() {
    test <- "test8: addDocument() 2nd Collection"
    cat(paste("\n",test, " Commencing\r"))

    # Get lab name, which is also the path for the collection
    lab <<- blue$getLab(type = "list")
    path <- lab$metaData$name

    DocumentCollection$new(name = "oxford", parent = blue, desc = "Oxford Corpus")
    blue$addDocument(oxford)
    copyFiles("./NLPStudio/Labs/blue/oxford")

    # Confirm directory created
    stopifnot(dir.exists("./NLPStudio/Labs/blue/oxford"))

    # Confirm modified date updated
    lab <<- blue$getLab(type = "list")
    stopifnot((Sys.time() - lab$metaData$created) > 1)
    stopifnot((Sys.time() - lab$metaData$modified) < 1.5)

    # Check state
    stopifnot(checkState("blue") == TRUE)
    stopifnot(checkState("brown") == TRUE)
    stopifnot(checkState("oxford") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "addDocument(oxford)", note = "Successfully added 2nd collection")
    logTests(cls = cls, mthd = "addDocument(oxford)", note = "Date modified updated correctly")
    logTests(cls = cls, mthd = "addDocument(oxford)", note = "Directory created")

    cat(paste("\n", test, " Completed: Success!\n"))
  }


  test9 <- function() {
    test <- "test9: getLab('object') with two documents"
    cat(paste("\n",test, " Commencing\r"))

    lab <<- blue$getLab(type = "object")
    stopifnot(isTRUE(all.equal(lab, blue)))

    # Check existence of documents
    documents <- blue$getDocuments(type = "object")
    for (i in length(documents)) {
      stopifnot("DocumentCollection" %in% class(documents[[i]])[1])
    }
    stopifnot(i == 2)

    # Check state
    stopifnot(checkState("blue") == TRUE)
    stopifnot(checkState("brown") == TRUE)
    stopifnot(checkState("oxford") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab('object')", note = "Successfully returned blue object")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test10 <- function() {
    test <- "test10: getLab('list') with two documents"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab list format
    lab <<- blue$getLab(type = "list")
    stopifnot(lab$metaData$name == "blue")
    stopifnot(lab$metaData$desc == "Blue Lab")
    stopifnot((Sys.time() - lab$metaData$created) > 1)
    stopifnot((Sys.time() - lab$metaData$modified) < 2)

    stopifnot(length(lab$documents) == 2)
    stopifnot(lab$documents[[1]]$name == "brown")
    stopifnot(lab$documents[[1]]$desc == "Brown Corpus")
    stopifnot((Sys.time() - lab$documents[[1]]$created) > 0.5)
    stopifnot((Sys.time() - lab$documents[[1]]$modified) > 0.5)

    stopifnot(lab$documents[[2]]$name == "oxford")
    stopifnot(lab$documents[[2]]$desc == "Oxford Corpus")
    stopifnot((Sys.time() - lab$documents[[2]]$created) < 2)
    stopifnot((Sys.time() - lab$documents[[2]]$modified) < 2)

    # Check state
    stopifnot(checkState("blue") == TRUE)
    stopifnot(checkState("brown") == TRUE)
    stopifnot(checkState("oxford") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab('list')", note = "Successfully returned blue list with two documents")
    logTests(cls = cls, mthd = "addDocument", note = "Successfully added 2nd collection")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test11 <- function() {
    test <- "test11: getLab('df') with two documents"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab df format
    lab <<- blue$getLab(type = "df")
    stopifnot(lab$metaData$labDf$name == "blue")
    stopifnot(lab$metaData$labDf$desc == "Blue Lab")
    stopifnot((Sys.time() - lab$metaData$labDf$created) > 1)
    stopifnot((Sys.time() - lab$metaData$labDf$modified) < 1)

    stopifnot(nrow(lab$documentsDf) == 2)
    stopifnot(lab$documentsDf$name[1] == "brown")
    stopifnot(lab$documentsDf$desc[1] == "Brown Corpus")
    stopifnot((Sys.time() - lab$documentsDf$created[1]) > 0.5)
    stopifnot((Sys.time() - lab$documentsDf$modified[1]) > 0.5)

    stopifnot(lab$documentsDf$name[2] == "oxford")
    stopifnot(lab$documentsDf$desc[2] == "Oxford Corpus")
    stopifnot((Sys.time() - lab$documentsDf$created[2]) < 1)
    stopifnot((Sys.time() - lab$documentsDf$modified[2]) < 1)

    # Check state
    stopifnot(checkState("blue") == TRUE)
    stopifnot(checkState('brown') == TRUE)
    stopifnot(checkState('oxford') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab('df')", note = "Successfully returned df type with 2 documents")
    logTests(cls = cls, mthd = "addDocument", note = "Successfully added 2nd lab")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test12 <- function() {
    test <- "test12: Remove collection, purge = FALSE"
    cat(paste("\n",test, " Commencing\r"))

    # Attempt to remove a non-existent collection ( a function): Success
    # blue$removeDocument(logTests)

    # Successfuly remove oxford from collection list
    blue$removeDocument(oxford, purge = FALSE)
    documents <<- blue$getDocuments(type = "list")
    for (d in 1:length(documents)) {
      stopifnot(!isTRUE(all.equal(documents[[d]]$name, "oxford")))
    }

    # Confirm object exists in global environment
    stopifnot(exists('oxford'))

    # Confirm directory has not been  deleted
    stopifnot(dir.exists("./NLPStudio/Labs/blue/oxford"))

    # Confirm document exists in archive
    archives <- nlpArchives$getArchives(type = "list")
    stopifnot(archives[[1]]$objectName == "oxford")
    stopifnot(archives[[1]]$archiveName == "oxford-2017-09-03-1")
    stopifnot(archives[[1]]$seqNum == 1)
    stopifnot(archives[[1]]$numFiles == 4)

    # Confirm date modified updated correctly
    lab <- blue$getLab()
    stopifnot((Sys.time() - lab$metaData$created) > 1)
    stopifnot((Sys.time() - lab$metaData$modified) < 1)

    # Check state
    stopifnot(checkState("blue") == TRUE)
    stopifnot(checkState("brown") == TRUE)
    stopifnot(checkState('oxford') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "removeLab", note = "Remove lab, purge = FALSE, tested")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test13 <- function() {
    test <- "test13: Remove collection, purge = TRUE"
    cat(paste("\n",test, " Commencing\r"))

    # Create  new document
    lab <- blue$getLab(type = "list")
    path <- lab$metaData$name
    DocumentCollection$new(name = "penn", parent = blue, desc = "Penn Corpus")

    # Add new document
    blue$addDocument(penn)
    copyFiles("./NLPStudio/Labs/blue/penn")

    # Confirm object exists as class DocumentCollection
    stopifnot("DocumentCollection" %in% class(penn))

    # Confirm directory created
    stopifnot(dir.exists("./NLPStudio/Labs/blue/penn"))

    # Remove document with a vengeance
    blue$removeDocument(penn, purge = TRUE)

    # Confirm document removed from lab
    documents <<- blue$getDocuments(type = "list")
    if (length(documents) > 0) {
      for (c in 1:length(documents)) {
        stopifnot(!isTRUE(all.equal(documents[[c]]$name, "penn")))
      }
    }

    # Confirm object has been removed from global environment
    stopifnot(!exists('penn'))

    # Confirm files deleted
    stopifnot(!dir.exists("./NLPStudio/Labs/blue/penn"))

    # Confirm Archived
    archives <- nlpArchives$getArchives(type = "list")
    stopifnot(archives[[2]]$objectName == "penn")
    stopifnot(archives[[2]]$archiveName == "penn-2017-09-03-1")
    stopifnot(archives[[2]]$numFiles == 4)
    stopifnot(archives[[2]]$seqNum == 1)
    stopifnot(as.Date(archives[[2]]$created) == as.Date(Sys.time()))

    # Confirm date modified updated correctly
    lab <- blue$getLab(type = "list")
    stopifnot((Sys.time() - lab$metaData$created) > 1)
    stopifnot((Sys.time() - lab$metaData$modified) < 1.5)

    # Check state
    stopifnot(checkState("blue") == TRUE)
    #stopifnot(checkState("penn") == TRUE) # should fail, not existing in state: success
    stopifnot(checkState('oxford') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "removeLab", note = "Remove lab, purge = TRUE, tested")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test14 <- function() {
    test <- "test14: Print lab"
    cat(paste("\n",test, " Commencing\r"))

    # Add lab
    DocumentCollection$new(name = "stanford", parent = blue, desc = "Stanford Corpus")
    blue$addDocument(stanford)
    copyFiles("./NLPStudio/Labs/blue/stanford")

    # Confirm object exists as class DocumentCollection
    stopifnot("DocumentCollection" %in% class(stanford))

    # Confirm directory created
    stopifnot(dir.exists("./NLPStudio/Labs/blue/stanford"))

    blue$printLab()  # should show 2 documents

    # Logit
    logTests(cls = cls, mthd = "printLab", note = "Print lab tested")
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

}

cls <- "Lab"

devtools::load_all()
testLab()
