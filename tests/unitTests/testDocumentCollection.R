testDocumentCollection <- function() {

  init <- function() {

    # Clean up
    if (exists("oxford", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("oxford", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
    if (exists("oxford_1", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("oxford-1", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }
    if (exists("stanford", envir = .GlobalEnv)) {
      rm(list = ls(envir = .GlobalEnv)[grep("stanford", ls(envir = .GlobalEnv))], envir = .GlobalEnv)
    }

    # Source state and log
    source("./tests/logTests.r")
  }

  test0 <- function() {
    test <- "test0: Document Instantiation"
    cat(paste("\n",test, " Commencing\n"))

    # Validation
    Document$new() # Fail, name required
    Document$new("oxford") # Fail filename required
    Document$new("oxford stuff") # Fail name invalid

    # Successful Instantiation, no path
    Document$new(name = "oxford", desc = "Oxford Collection")
    Document$new(name = "oxford_1", desc = "Oxford_1 Collection")
    Document$new(name = "stanford", desc = "Stanford Collection")

    # Obtain document information
    o <- oxford$exposeObject()
    o1 <- oxford_1$exposeObject()
    s <- stanford$exposeObject()

    # Print states
    cat(paste("\n               Object State:", o$stateDesc))
    cat(paste("\n               Object State:", o1$stateDesc))
    cat(paste("\n               Object State:", s$stateDesc))

    # Validate instantiation and exposeObject
    stopifnot(o$name == "oxford")
    stopifnot(o$desc == "Oxford Collection")
    stopifnot((Sys.time() - o$modified) < 1)
    stopifnot((Sys.time() - o$created) < 1)

    stopifnot(o1$name == "oxford_1")
    stopifnot(o1$desc == "Oxford_1 Collection")
    stopifnot((Sys.time() - o1$modified) < 1)
    stopifnot((Sys.time() - o1$created) < 1)

    stopifnot(s$name == "stanford")
    stopifnot(s$desc == "Stanford Collection")
    stopifnot((Sys.time() - s$modified) < 1)
    stopifnot((Sys.time() - s$created) < 1)


    # Logit
    logTests(class = class, mthd = "initiate", note = "Instantiation Tested")

    cat(paste("\n\n", test, " Completed: Success!\n"))
  }

  test1 <- function() {
    test <- "test1: Test addChild"
    cat(paste("\n",test, " Commencing\n"))

    # Validate
    oxford$addChild() # Fail object missing
    oxford$addChild(Bart) # Can't add lab as child

    # Add collection to collection
    oxford$addChild(oxford_1)

    # Confirm parent
    o <- oxford$exposeObject()
    children <- oxford$getChildren()
    stopifnot(length(children) == 1)
    child <- children[[1]]$exposeObject()
    stopifnot(child$name == "oxford_1")
    stopifnot(child$desc == "Oxford_1 Collection")
    stopifnot(o$created != o$modified)

    # Confirm Child
    o1 <- oxford_1$exposeObject()
    parent <- oxford_1$getAncestor()
    parent <- parent$exposeObject()
    stopifnot(parent$name == "oxford")
    stopifnot(o1$created != o1$modified)

    # Add collection to collection
    oxford$addChild(stanford)

    # Confirm parent
    o <- oxford$exposeObject()
    children <- oxford$getChildren()
    stopifnot(length(children) == 2)
    child <- children[[2]]$exposeObject()
    stopifnot(child$name == "stanford")
    stopifnot(child$desc == "Stanford Collection")
    stopifnot(o$created != o$modified)

    # Confirm Child
    s <- stanford$exposeObject()
    parent <- stanford$getAncestor()
    parent <- parent$exposeObject()
    stopifnot(parent$name == "oxford")
    stopifnot(s$created != s$modified)


    # Print states
    cat(paste("\n               Object State:", o$stateDesc))
    cat(paste("\n               Object State:", o1$stateDesc))
    cat(paste("\n               Object State:", s$stateDesc))

    # Logit
    logTests(class = class, mthd = "readDocument", note = "addChild Tested!")

    cat(paste("\n\n", test, " Completed: Success!\n"))
  }

  test2 <- function() {
    test <- "test2: Test addChild"
    cat(paste("\n",test, " Commencing\n"))

    # Validate
    oxford$addChild() # Fail object missing
    oxford$addChild(Bart) # Can't add lab as child


    # Print states
    cat(paste("\n               Object State:", o$stateDesc))
    cat(paste("\n               Object State:", o1$stateDesc))
    cat(paste("\n               Object State:", s$stateDesc))

    # Logit
    logTests(class = class, mthd = "readDocument", note = "addChild Tested!")

    cat(paste("\n\n", test, " Completed: Success!\n"))
  }

  init()
  test0()
  test1()
  test2()
  test3()
  test4()
  test5()
  test6()

}

class <- "DocumentCollection"

devtools::load_all()
testDocumentCollection()
