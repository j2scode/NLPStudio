testLab <- function() {
  init <- function() {

    # Source state and log
    source("./tests/testFunctions/logTests.r")
  }

  test0 <- function() {
    test <- "test0: Authorizer Instantiation"
    cat(paste("\n\n",test, " Commencing"))

    a <<- Authorizer$new()$getInstance()


    # Logit
    logTests(class = class, mthd = "initiate", note = "Successfully instantiation")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test1 <- function() {
    test <- "test1: Add Class"
    cat(paste("\n\n",test, " Commencing"))

    methods <- c("name", "desc", "labs", "stateId", "stateDesc")
    a$addClass("NLPStudio", methods)


    # Logit
    logTests(class = class, mthd = "initiate", note = "Successfully instantiation")
    cat(paste("\n", test, " Completed: Success!\n"))
  }

  init()
  test0()
  test1()
  test2()
  test3()

}

class <- "Authorizer"
testAuthorizer()
