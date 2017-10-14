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

    # Source state and log
    source("./tests/testFunctions/logTests.r")
  }

  # Test 0: Confirm instantiation of lab
  test0 <- function() {
    test <- "test0: Lab Instantiation"
    cat(paste("\n",test, " Commencing\n\n"))

    # Test Instantiation
    #Lab$new() # should fail, name is required: Success
    Lab$new(name = "blue", "Blue Lab")
    #Lab$new(name = "blue", "Blue Lab") # Error lab exists and directory already exists: success


    # Confirm instantiation
    b <- blue$exposeObject()
    stopifnot("Lab" %in% class(blue))
    stopifnot(b$name == "blue")
    stopifnot(b$desc == "Blue Lab")
    stopifnot(isTRUE(all.equal(b$parent, nlpStudio)))
    stopifnot((Sys.time() - b$created) < 1)
    stopifnot((Sys.time() - b$modified) < 1)
    cat(paste("\n               Object State:", b$stateDesc))

    # Logit
    logTests(class = class, mthd = "initiate", note = "Successfully created lab")
    cat(paste("\n\n", test, " Completed: Success, state is", studio$stateDesc, "!\n"))
  }


  test1 <- function() {
    test <- "test1: Add Collection"
    cat(paste("\n",test, " Commencing\r"))

    # Create and verify document collection
    DocumentCollection$new(name = "brown", desc = "Brown Corpus")
    collection <<- brown$exposeObject()
    stopifnot("DocumentCollection" %in% class(brown))
    stopifnot(collection$name == "brown")
    stopifnot(collection$desc == "Brown Corpus")
    stopifnot((Sys.time() - collection$created) < 1)
    stopifnot((Sys.time() - collection$modified) < 1)
    cat(paste("\n               Object State:", collection$stateDesc))

    # Add collection to lab
    blue$addChild(brown)

    # Confirm Lab is updated
    lab <<- blue$exposeObject()
    stopifnot(nrow(lab$collections) == 1)
    stopifnot(lab$collections[[1]]$name == "brown")
    stopifnot(lab$collections[[1]]$desc == "Brown Corpus")
    stopifnot((Sys.time() - lab$collections[[1]]$created) < 1.5)
    stopifnot((Sys.time() - lab$collections[[1]]$modified) < 1.5)
    stopifnot((Sys.time() - lab$created) < 1)
    stopifnot((Sys.time() - lab$modified) < 1)
    cat(paste("\n               Object State:", lab$stateDesc))

    # Confirm collection parent is updated
    collection <<- brown$exposeObject()
    stopifnot("DocumentCollection" %in% class(brown))
    stopifnot(collection$name == "brown")
    stopifnot(collection$desc == "Brown Corpus")
    stopifnot(isTRUE(all.equal(collection$parent, blue)))
    stopifnot((Sys.time() - collection$created) < 1)
    stopifnot((Sys.time() - collection$modified) < 1)
    cat(paste("\n               Object State:", collection$stateDesc))

    # Logit
    logTests(class = class, mthd = "addChild", note = "Created collection and added to lab")


    cat(paste("\n\n", test, " Completed: Success!\n"))
  }

  test2 <- function() {
    test <- "test2: addChild() 2nd Collection"
    cat(paste("\n",test, " Commencing\r"))

    # Instantiate and confirm instantiation
    DocumentCollection$new(name = "oxford", desc = "Oxford Corpus")
    o <- oxford$exposeObject()
    stopifnot("DocumentCollection" %in% class(oxford))
    stopifnot(o$name == "oxford")
    stopifnot(o$desc == "Oxford Corpus")
    stopifnot((Sys.time() - o$created) < 1)
    stopifnot((Sys.time() - o$modified) < 1)
    cat(paste("\n               Object State:", o$stateDesc))

    # Add collection to lab
    blue$addChild(oxford)

    # Confirm document added to lab
    lab <<- blue$exposeObject()
    stopifnot(nrow(lab$collections) == 2)
    stopifnot(lab$collections[[2]]$name == "oxford")
    stopifnot(lab$collections[[2]]$desc == "Oxford Corpus")
    stopifnot((Sys.time() - lab$collections[[2]]$created) < 1.5)
    stopifnot((Sys.time() - lab$collections[[2]]$modified) < 1.5)
    stopifnot((Sys.time() - lab$created) < 1)
    stopifnot((Sys.time() - lab$modified) < 1)
    cat(paste("\n               Object State:", lab$stateDesc))

    # Confirm collection parent is updated
    collection <<- oxford$exposeObject()
    stopifnot("DocumentCollection" %in% class(oxford))
    stopifnot(collection$name == "oxford")
    stopifnot(collection$desc == "Oxford Corpus")
    stopifnot(isTRUE(all.equal(collection$parent, blue)))
    stopifnot((Sys.time() - collection$created) < 1)
    stopifnot((Sys.time() - collection$modified) < 1)
    cat(paste("\n               Object State:", collection$stateDesc))


    # Logit
    logTests(class = class, mthd = "addChild(oxford)", note = "Successfully added 2nd collection")

    cat(paste("\n\n", test, " Completed: Success!\n"))
  }

  test3 <- function() {
    test <- "test3: Remove collection"
    cat(paste("\n",test, " Commencing\r"))

    Sys.sleep(1)

    # Attempt to remove a non-existent collection ( a function): Success
    #blue$removeChild(logTests)  # Failed successfully

    # Remove oxford from collection list
    blue$removeChild(oxford)

    # Verify object removed from lab object
    collections <<- blue$getChildren()
    for (c in 1:length(collections)) {
      stopifnot(!isTRUE(all.equal(collections[[c]]$name, "oxford")))
    }

    # Verify States
    b <- blue$exposeObject()
    o <- oxford$exposeObject()
    cat(paste("\n               Object State:", b$stateDesc))
    cat(paste("\n               Object State:", o$stateDesc))

    # Verify parent of removed object set to null
    o <- oxford$exposeObject()
    stopifnot(is.null(o$parent))
    stopifnot((Sys.time() - o$created) > 1)
    stopifnot((Sys.time() - o$modified) < 1)

    # Logit
    logTests(class = class, mthd = "removeLab", note = "Remove lab, purge = FALSE, tested")
    cat(paste("\n", test, " Completed: Success!\n"))
  }


  init()
  test0()
  test1()
  test2()
  test3()

}

class <- "Lab"

devtools::load_all()
testLab()
