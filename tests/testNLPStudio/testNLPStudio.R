testNLPStudio <- function() {

  # Test 0: Confirm instantiation of nlpStudio
  test0 <- function() {
    cat("\rTest0 Commencing\r")
    studio <<- nlpStudio$getStudio()
    stopifnot("NLPStudio" %in% class(nlpStudio))
    stopifnot(studio$name == "nlpStudio")
    stopifnot(studio$desc == "NLPStudio: Natural Language Processing Studio")
    stopifnot(studio$current == "None")
    stopifnot("POSIXct" %in% class(studio$created))
    cat("\nTest0 Completed: Success!\r")
  }


  # Test 1: Confirm nlpStudio directories created
  test1 <- function() {
    cat("\nTest1 Commencing\r")
    stopifnot(dir.exists(archiveDir))
    stopifnot(dir.exists(labsDir))
    cat("\nTest1 Completed: Success!\r")
  }

  # Test 2: Confirm initial environment automatically created properly
  test2 <- function() {
    cat("\n\nTest2 Commencing\r")
    stopifnot(nrow(studio$environment) == 1)
    stopifnot(studio$environment$name[1] == "Main")
    stopifnot(studio$environment$desc[1] == "Main NLP Context")
    stopifnot(studio$environment$path[1] == "./Contexts/Main")
    stopifnot(studio$current == "Main")
    cat("\nTest2 Completed: Success!\r")
  }

  # Test 3: Add environment with current = FALSE
  test3 <- function() {
    cat("\n\nTest3 Commencing\r")
    nlpStudio$addContext(environment = "Development",
                         environmentDesc = "Development Context",
                         environmentCurrent = FALSE)
    studio <<- nlpStudio$getStudio()
    test1()
    stopifnot(nrow(studio$environment) == 2)
    stopifnot(studio$environment$id[1] == "Main")
    stopifnot(studio$environment$desc[1] == "Main NLP Context")
    stopifnot(studio$environment$path[1] == "./Contexts/Main")
    stopifnot(studio$environment$id[2] == "Development")
    stopifnot(studio$environment$desc[2] == "Development Context")
    stopifnot(studio$environment$path[2] == "./Contexts/Development")
    stopifnot(studio$current == "Main")
    cat("\nTest3 Completed: Success!\r")
  }

  # Test 4: Add environment with current = TRUE
  test4 <- function() {
    cat("\n\nTest4 Commencing\r")
    nlpStudio$addContext(environmentId = "Tuesday",
                         environmentDesc = "Tuesday Context",
                         environmentCurrent = TRUE)
    studio <<- nlpStudio$getStudio()
    test1()
    stopifnot(nrow(studio$environment) == 3)
    stopifnot(studio$environment$id[1] == "Main")
    stopifnot(studio$environment$desc[1] == "Main NLP Context")
    stopifnot(studio$environment$path[1] == "./Contexts/Main")
    stopifnot(studio$environment$id[2] == "Development")
    stopifnot(studio$environment$desc[2] == "Development Context")
    stopifnot(studio$environment$path[2] == "./Contexts/Development")
    stopifnot(studio$environment$id[3] == "Tuesday")
    stopifnot(studio$environment$desc[3] == "Tuesday Context")
    stopifnot(studio$environment$path[3] == "./Contexts/Tuesday")
    stopifnot(studio$current == "Tuesday")
    cat("\nTest4 Completed: Success!\r")
  }

  # Test 5: Change current environment
  test5 <- function() {
    cat("\n\nTest5 Commencing\r")

    nlpStudio$currentContext <- "Main"
    studio <<- nlpStudio$getStudio()
    test1()
    stopifnot(nrow(studio$environment) == 3)
    stopifnot(studio$environment$id[1] == "Main")
    stopifnot(studio$environment$desc[1] == "Main NLP Context")
    stopifnot(studio$environment$path[1] == "./Contexts/Main")
    stopifnot(studio$environment$id[2] == "Development")
    stopifnot(studio$environment$desc[2] == "Development Context")
    stopifnot(studio$environment$path[2] == "./Contexts/Development")
    stopifnot(studio$environment$id[3] == "Tuesday")
    stopifnot(studio$environment$desc[3] == "Tuesday Context")
    stopifnot(studio$environment$path[3] == "./Contexts/Tuesday")
    stopifnot(studio$current == "Main")
    cat("\nTest5 Completed: Success!\r")
  }

  # Test6: Test validation of environments all should stop processing with log
  test6 <- function() {
    cat("\n\nTest6 Commencing\r")
    #nlpStudio$currentContext <- "DAKDSAS" Works
    #nlpStudio$addContext(92)
    #nlpStudio$addContext("Test", "Test Description", environmentCurrent = 9)
    cat("\nTest6 Completed: Success!\r")
  }

  test7 <- function() {
    # Tests for NLPStudio recall after reboot
    cat("\n\nTest7 Commencing\r")
    environments <- nlpStudio$listContexts()
    stopifnot(environments$environmentId[1] == "Main")
    stopifnot(environments$environmentId[2] == "Development")
    stopifnot(environments$environmentId[3] == "Tuesday")

    stopifnot(environments$environmentDesc[1] == "Main NLP Context")
    stopifnot(environments$environmentDesc[2] == "Development Context")
    stopifnot(environments$environmentDesc[3] == "Tuesday Context")

    stopifnot(environments$environmentPath[1] == "./Contexts/Main")
    stopifnot(environments$environmentPath[2] == "./Contexts/Development")
    stopifnot(environments$environmentPath[3] == "./Contexts/Tuesday")

    #stopifnot(nlpStudio$currentContext != "Tuesday")
    cat("\nTest7 Completed: Success!\r")
  }

test0()
test1()
# test2()
# test3()
# test4()
# test5()
# test6()
# test7()
}


labsDir <- "./Labs"
archiveDir <- "./Archive"
nlpStudioFile <- "./NLPStudio.Rdata"

# base::unlink(labsDir, recursive = FALSE)
# base::unlink(archiveDir, recursive = TRUE)
# base::file.remove(nlpStudioFile)

devtools::load_all()

studio <- nlpStudio$getStudio()

testNLPStudio()
