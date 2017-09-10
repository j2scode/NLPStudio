testSnapSave <- function() {

  init <- function() {

    Lab$new(name = "Irma", desc = "Cat 5 Hurricane Lab")
    Document$new(name = "oxford-news", desc = "Oxford News Register")
    Document$new(name = "oxford-finance", desc = "Oxford Finance Register")
    Document$new(name = "oxford-sports", desc = "Oxford Sports Register")
    Document$new(name = "stanford-finance", desc = "Stanford Finance Register")
    Document$new(name = "stanford-sports", desc = "Stanford Sports Register")
    DocumentCollection$new(name = "oxford", "Oxford Collection")
    DocumentCollection$new(name = "stanford", "Stanford Collection")
    Lab$new(name = "Irma", "Irma Lab")
    Lab$new(name = "Dev", "Development Lab")

    nlpStudio$addChild(blue)
    nlpStudio$addChild(Development)
    Development$addChild(oxford)
    oxford$addChild(brown)
    nlpStudio$addChild(Irma, enter = TRUE)
    copyFiles("./NLPStudio/Labs/blue")
    copyFiles("./NLPStudio/Labs/Development")
    copyFiles("./NLPStudio/Labs/Development/oxford")
    copyFiles("./NLPStudio/Labs/Development/oxford/brown")


  }

  # Test 0: Test SnapSave Validation
  test0 <- function() {
    test <- "test0: Validate Parameters"
    cat(paste("\n",test, " Commencing\r"))

    # Test validation
    SnapSave$save()
    SnapSave$save("Development")
    SnapSave$save("oxford", oxford)
    SnapSave$save("brown", brown, "./NxLPStudio/Snapshots/Collections")

    # Logit
    logTests(cls = "Lab", mthd = "initiate", note = "Successfully created appropriate directories and saved objects and files")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  # Test 0: Test SnapSave
  test1 <- function() {
    test <- "test1: Save Object"
    cat(paste("\n",test, " Commencing\r"))

    # Save Objects
    SnapSave$save("blue", blue, "./NLPStudio/Snapshots/Labs")
    SnapSave$save("Development", Development, "./NLPStudio/Snapshots/Labs")
    SnapSave$save("oxford", oxford, "./NLPStudio/Snapshots/Collections")
    SnapSave$save("brown", brown, "./NLPStudio/Snapshots/Collections")
    SnapSave$save("Irma", Irma, "./NLPStudio/Snapshots/Labs")

    # Confirm directory exists
    stopifnot(dir.exists("./NLPStudio/Snapshots/Labs/blue-2017-09-09-1"))
    stopifnot(dir.exists("./NLPStudio/Snapshots/Labs/Development-2017-09-09-1"))
    stopifnot(dir.exists("./NLPStudio/Snapshots/Collections/oxford-2017-09-09-1"))
    stopifnot(dir.exists("./NLPStudio/Snapshots/Collections/brown-2017-09-09-1"))
    stopifnot(dir.exists("./NLPStudio/Snapshots/Labs/Irma-2017-09-09-1"))

    # Confirm files exist
    stopifnot(file.exists("./NLPStudio/Snapshots/Labs/blue-2017-09-09-1/blue-2017-09-09-1-object"))
    stopifnot(file.exists("./NLPStudio/Snapshots/Labs/blue-2017-09-09-1/blue-2017-09-09-1-files"))
    stopifnot(file.exists("./NLPStudio/Snapshots/Labs/Development-2017-09-09-1/Development-2017-09-09-1-object"))
    stopifnot(file.exists("./NLPStudio/Snapshots/Labs/Development-2017-09-09-1/Development-2017-09-09-1-files"))
    stopifnot(file.exists("./NLPStudio/Snapshots/Collections/oxford-2017-09-09-1/oxford-2017-09-09-1-object"))
    stopifnot(file.exists("./NLPStudio/Snapshots/Collections/oxford-2017-09-09-1/oxford-2017-09-09-1-files"))
    stopifnot(file.exists("./NLPStudio/Snapshots/Collections/brown-2017-09-09-1/brown-2017-09-09-1-object"))
    stopifnot(file.exists("./NLPStudio/Snapshots/Collections/brown-2017-09-09-1/brown-2017-09-09-1-files"))
    stopifnot(file.exists("./NLPStudio/Snapshots/Labs/Irma-2017-09-09-1/Irma-2017-09-09-1-object"))
    stopifnot(!file.exists("./NLPStudio/Snapshots/Labs/Irma-2017-09-09-1/Irma-2017-09-09-1-files"))

    # Logit
    logTests(cls = "Lab", mthd = "initiate", note = "Successfully created appropriate directories and saved objects and files")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

init()
test0()
test1()

}

testSnapSave()
