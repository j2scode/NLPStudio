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
    if (dir.exists("./Labs/blue") == TRUE) unlink("./Labs/blue", recursive = TRUE)

    # Source cache and log
    source("./tests/checkCache.r")
    source("./tests/logTests.r")
  }

  # Test 0: Confirm instantiation of lab
  test0 <- function() {
    test <- "test0: Lab Instantiation"
    cat(paste("\n",test, " Commencing\r"))

    # Test Instantiation
    # Lab$new() # should fail, name is required: Success
    Lab$new(name = "blue", "Blue Lab")
    #Lab$new(name = "blue", "Blue Lab") # Error lab exists and directory already exists: success
    stopifnot("Lab" %in% class(blue))
    stopifnot(dir.exists("./Labs/blue"))

    # Confirm modified date updated
    lab <<- blue$getLab(format = "list")
    stopifnot((Sys.time() - lab$created) < 1)
    stopifnot((Sys.time() - lab$modified) < 1)

    # Logit
    logTests(cls = cls, mthd = "initiate", note = "Successfully created lab")
    logTests(cls = cls, mthd = "initiate", note = "Create and modified dates initialized correctly")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test1 = function() {
    test <- "test1: getLab('object'), no collections"
    cat(paste("\n",test, " Commencing\r"))

    Sys.sleep(2)

    # Test getLab object format
    lab <<- blue$getLab(format = "object")
    stopifnot(isTRUE(all.equal(lab, blue)))

    # Check cache
    stopifnot(checkCache("blue") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab('object')", note = "Successfully returned object.")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test2 = function() {
    test <- "test2: getLab('list'), no collections"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab, list format
    lab <<- blue$getLab(format = "list")
    stopifnot(lab$name == "blue")
    stopifnot(lab$desc == "Blue Lab")
    stopifnot(length(lab$collections) == 0)
    stopifnot((Sys.time() - lab$created) > 1)
    stopifnot((Sys.time() - lab$modified) > 1)

    # Check cache
    stopifnot(checkCache("blue") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab('list')", note = "Successfully returned list")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test3 = function() {
    test <- "test3: getLab('df') No labs"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab, data frame
    lab <<- blue$getLab(format = "df")
    stopifnot(nrow(lab$labDf) == 1)
    stopifnot(lab$labDf$name[1] == "blue")
    stopifnot(lab$labDf$desc[1] == "Blue Lab")
    stopifnot((Sys.time() - lab$labDf$created[1]) > 1)
    stopifnot((Sys.time() - lab$labDf$modified[1]) > 1)
    stopifnot(nrow(lab$collectionsDf) == 0)

    # Check cache
    stopifnot(checkCache("blue") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab(format = 'df')", note = "Data frame returned successfully")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test4 <- function() {
    test <- "test4: Add Collection"
    cat(paste("\n",test, " Commencing\r"))

    # Get lab name, which is also the path for the collection
    lab <<- blue$getLab(format = "list")
    path <- lab$name

    # Create and add collection
    DocumentCollection$new(name = "brown", path = path, desc = "Brown Corpus")
    stopifnot("DocumentCollection" %in% class(brown))
    stopifnot("Lab" %in% class(blue))
    blue$addDocument(brown)

    # Confirm modified date updated
    lab <<- blue$getLab(format = "list")
    stopifnot((Sys.time() - lab$created) > 1)
    stopifnot((Sys.time() - lab$modified) < 1)

    # Check cache
    stopifnot(checkCache("blue") == TRUE)
    stopifnot(checkCache("brown") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "addDocument", note = "Created collection and added to lab")
    logTests(cls = cls, mthd = "addDocument", note = "Date modified updated correctly")


    cat(paste("\n", test, " Completed: Success!\n"))
  }


  test5 <- function() {
    test <- "test5: getLab('object') with Collection"
    cat(paste("\n",test, " Commencing\r"))

    lab <<- blue$getLab(format = "object")
    stopifnot(isTRUE(all.equal(lab, blue)))

    # Check cache
    stopifnot(checkCache("blue") == TRUE)
    stopifnot(checkCache("brown") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab(format = 'object')", note = "Successfully returned object type")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test6 <- function() {
    test <- "test6: getLab('list') with Collection"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab list format
    lab <<- blue$getLab(format = "list")
    stopifnot(lab$name == "blue")
    stopifnot(lab$desc == "Blue Lab")
    stopifnot((Sys.time() - lab$created) > 1)
    stopifnot((Sys.time() - lab$modified) < 1)

    stopifnot(length(lab$collections) == 1)
    stopifnot(lab$collections[[1]]$name == "brown")
    stopifnot(lab$collections[[1]]$desc == "Brown Corpus")
    stopifnot((Sys.time() - lab$collections[[1]]$created) < 1)
    stopifnot((Sys.time() - lab$collections[[1]]$modified) < 1)

    # Check cache
    stopifnot(checkCache("blue") == TRUE)
    stopifnot(checkCache("brown") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab(format = 'list')", note = "Successfully returned list type with added collection")
    logTests(cls = cls, mthd = "addDocument", note = "Successfully added collection")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test7 <- function() {
    test <- "test7: getLab('df') with Collection"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab df format
    lab <<- blue$getLab(format = "df")
    stopifnot(lab$labDf$name == "blue")
    stopifnot(lab$labDf$desc == "Blue Lab")
    stopifnot((Sys.time() - lab$labDf$created) > 1)
    stopifnot((Sys.time() - lab$labDf$modified) < 1)

    stopifnot(nrow(lab$collectionsDf) == 1)
    stopifnot(lab$collectionsDf$name[1] == "brown")
    stopifnot(lab$collectionsDf$desc[1] == "Brown Corpus")
    stopifnot((Sys.time() - lab$collectionsDf$created[1]) < 1)
    stopifnot((Sys.time() - lab$collectionsDf$modified[1]) < 1)

    # Check cache
    stopifnot(checkCache("blue") == TRUE)
    stopifnot(checkCache("brown") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab(format = 'df')", note = "Successfully returned df type")
    logTests(cls = cls, mthd = "addDocument", note = "Successfully added collection")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test8 <- function() {
    test <- "test8: addDocument() 2nd Collection"
    cat(paste("\n",test, " Commencing\r"))

    # Get lab name, which is also the path for the collection
    lab <<- blue$getLab(format = "list")
    path <- lab$name

    DocumentCollection$new(name = "oxford", path = path, desc = "Oxford Corpus")
    blue$addDocument(oxford)

    # Confirm modified date updated
    lab <<- blue$getLab(format = "list")
    stopifnot((Sys.time() - lab$created) > 1)
    stopifnot((Sys.time() - lab$modified) < 1)

    # Check cache
    stopifnot(checkCache("blue") == TRUE)
    stopifnot(checkCache("brown") == TRUE)
    stopifnot(checkCache("oxford") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "addDocument(oxford)", note = "Successfully added 2nd collection")
    logTests(cls = cls, mthd = "addDocument(oxford)", note = "Date modified updated correctly")

    cat(paste("\n", test, " Completed: Success!\n"))
  }


  test9 <- function() {
    test <- "test9: getLab('object') with two collections"
    cat(paste("\n",test, " Commencing\r"))

    lab <<- blue$getLab(format = "object")
    stopifnot(isTRUE(all.equal(lab, blue)))

    # Check cache
    stopifnot(checkCache("blue") == TRUE)
    stopifnot(checkCache("brown") == TRUE)
    stopifnot(checkCache("oxford") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab('object')", note = "Successfully returned blue object")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test10 <- function() {
    test <- "test10: getLab('list') with two collections"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab list format
    lab <<- blue$getLab(format = "list")
    stopifnot(lab$name == "blue")
    stopifnot(lab$desc == "Blue Lab")
    stopifnot((Sys.time() - lab$created) > 1)
    stopifnot((Sys.time() - lab$modified) < 1)

    stopifnot(length(lab$collections) == 2)
    stopifnot(lab$collections[[1]]$name == "brown")
    stopifnot(lab$collections[[1]]$desc == "Brown Corpus")
    stopifnot((Sys.time() - lab$collections[[1]]$created) > 0.5)
    stopifnot((Sys.time() - lab$collections[[1]]$modified) > 0.5)

    stopifnot(lab$collections[[2]]$name == "oxford")
    stopifnot(lab$collections[[2]]$desc == "Oxford Corpus")
    stopifnot((Sys.time() - lab$collections[[2]]$created) < 1)
    stopifnot((Sys.time() - lab$collections[[2]]$modified) < 1)

    # Check cache
    stopifnot(checkCache("blue") == TRUE)
    stopifnot(checkCache("brown") == TRUE)
    stopifnot(checkCache("oxford") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab('list')", note = "Successfully returned blue list with two collections")
    logTests(cls = cls, mthd = "addDocument", note = "Successfully added 2nd collection")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test11 <- function() {
    test <- "test11: getLab('df') with two collections"
    cat(paste("\n",test, " Commencing\r"))

    # Test getLab df format
    lab <<- blue$getLab(format = "df")
    stopifnot(lab$labDf$name == "blue")
    stopifnot(lab$labDf$desc == "Blue Lab")
    stopifnot((Sys.time() - lab$labDf$created) > 1)
    stopifnot((Sys.time() - lab$labDf$modified) < 1)

    stopifnot(nrow(lab$collectionsDf) == 2)
    stopifnot(lab$collectionsDf$name[1] == "brown")
    stopifnot(lab$collectionsDf$desc[1] == "Brown Corpus")
    stopifnot((Sys.time() - lab$collectionsDf$created[1]) > 0.5)
    stopifnot((Sys.time() - lab$collectionsDf$modified[1]) > 0.5)

    stopifnot(lab$collectionsDf$name[2] == "oxford")
    stopifnot(lab$collectionsDf$desc[2] == "Oxford Corpus")
    stopifnot((Sys.time() - lab$collectionsDf$created[2]) < 1)
    stopifnot((Sys.time() - lab$collectionsDf$modified[2]) < 1)

    # Check cache
    stopifnot(checkCache("blue") == TRUE)
    stopifnot(checkCache('brown') == TRUE)
    stopifnot(checkCache('oxford') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "getLab('df')", note = "Successfully returned df type with 2 collections")
    logTests(cls = cls, mthd = "addDocument", note = "Successfully added 2nd lab")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test12 <- function() {
    test <- "test12: Remove collection, purge = FALSE"
    cat(paste("\n",test, " Commencing\r"))

    # Attempt to remove a non-existent collection ( a function): Success
    # blue$removeDocument(logTests)

    # Successfuly remove oxford from collection list
    blue$removeDocument(oxford)
    collections <<- blue$getDocuments(format = "list")
    for (c in 1:length(collections)) {
      stopifnot(!isTRUE(all.equal(collections[[c]]$name, "oxford")))
    }

    # Confirm object exists in global environment
    stopifnot(exists('oxford'))

    # Confirm date modified updated correctly
    lab <- blue$getLab()
    stopifnot((Sys.time() - lab$created) > 1)
    stopifnot((Sys.time() - lab$modified) < 1)

    # Check cache
    stopifnot(checkCache("blue") == TRUE)
    stopifnot(checkCache("brown") == TRUE)
    stopifnot(checkCache('oxford') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "removeLab", note = "Remove lab, purge = FALSE, tested")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test13 <- function() {
    test <- "test13: Remove collection, purge = TRUE"
    cat(paste("\n",test, " Commencing\r"))

    # Successfuly remove purge document from document list
    lab <- blue$getLab(format = "list")
    path <- lab$name
    DocumentCollection$new(name = "penn", path = path, desc = "Penn Corpus")
    blue$removeDocument(penn, purge = TRUE)
    collections <<- blue$getDocuments(format = "list")
    if (length(collections) > 0) {
      for (c in 1:length(collections)) {
        stopifnot(!isTRUE(all.equal(collections[[c]]$name, "penn")))
      }
    }

    # Confirm object exists in global environment
    stopifnot(!exists('penn'))

    # Confirm date modified updated correctly
    lab <- blue$getLab(format = "list")
    stopifnot((Sys.time() - lab$created) > 1)
    stopifnot((Sys.time() - lab$modified) < 1)

    # Check cache
    stopifnot(checkCache("blue") == TRUE)
    #stopifnot(checkCache("penn") == TRUE) # should fail, not existing in cache: success
    stopifnot(checkCache('oxford') == TRUE)

    # Logit
    logTests(cls = cls, mthd = "removeLab", note = "Remove lab, purge = TRUE, tested")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test14 <- function() {
    test <- "test14: Print lab"
    cat(paste("\n",test, " Commencing\r"))

    blue$printLab()

    # Logit
    logTests(cls = cls, mthd = "printLab", note = "Print lab tested")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test15 <- function() {
    test <- "test15: Test Archive"
    cat(paste("\n",test, " Commencing\r"))

    blue$printLab()

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
