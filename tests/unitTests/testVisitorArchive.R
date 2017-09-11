testVisitorArchive <- function() {

  init <- function() {

    dirs <- nlpStudio$getDirectories()
    lapply(dirs, function(d) {base::unlink(d, recursive = TRUE)})
    base::unlink("./NLPStudio")

    devtools::load_all()

    # Create objects
    Document$new(name = "oxford-news", desc = "Oxford News Register")
    Document$new(name = "oxford-finance", desc = "Oxford Finance Register")
    Document$new(name = "oxford-sports", desc = "Oxford Sports Register")
    Document$new(name = "stanford-finance", desc = "Stanford Finance Register")
    Document$new(name = "stanford-sports", desc = "Stanford Sports Register")
    DocumentCollection$new(name = "oxford", "Oxford Collection")
    DocumentCollection$new(name = "stanford", "Stanford Collection")
    Lab$new(name = "Development", "Development Lab")
    Lab$new(name = "Irma", desc = "Cat 5 Hurricane Lab")
    Lab$new(name = "blue", desc = "Blue Lab")

    # Add development lab relationships
    nlpStudio$addChild(Development)
    Development$addChild(oxford)
    oxford$addChild(oxford-news)
    oxford$addChild(oxford-finance)
    oxford$addChild(oxford-sports)
    copyFiles("./NLPStudio/Labs/Development/oxford")

    # Add Irma lab relationships
    nlpStudio$addChild(Irma, enter = TRUE)
    Irma$addChild(stanford)
    stanford$addChild(stanford-finance)
    stanford$addChild(stanford-sports)
    copyFiles("./NLPStudio/Labs/blue")
    copyFiles("./NLPStudio/Labs/Irma/stanford")

  }

  # Test 0: Test VisitorArchive Validation
  test0 <- function() {
    test <- "test0: Validate Parameters"
    cat(paste("\n",test, " Commencing\r"))

    # Test validation
    VisitorArchive$visitLab() # Fail, stateId missing
    VisitorArchive$visitLab("Irma-2017-09-10-1") # Fail, lab object missing
    VisitorArchive$visitLab("Irma-2017-09-10-1", cls) # Fail, object wrong class

    # Logit
    logTests(cls = "VisitorArchive", mthd = "archive", note = "Validation logic performs as expected")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  # Test 1: Test VisitorArchive Archive Lab Functionality
  test1 <- function() {
    test <- "test1: VisitorArchive: Archive Lab Functionality"
    cat(paste("\n",test, " Commencing\r"))

    # Archive Labs
    d <- VisitorArchive$visitLab("Development-2017-09-11-1", Development)
    i <- VisitorArchive$visitLab("Irma-2017-09-11-1", Irma)
    b <- VisitorArchive$visitLab("blue-2017-09-11-1", blue)

    # Validate Development Lab
    stopifnot(d$stateId == "Development-2017-09-11-1")
    stopifnot(d$class == "Lab")
    stopifnot(d$objectName == "Development")
    stopifnot(d$path == "./NLPStudio/Achives/Labs/Development")
    stopifnot(d$fileName == "Development-2017-09-11-1-Lab-class-object-files")
    stopifnot(c("contractions.csv", "en_US.news.txt", "quadgrams.Rdata", "testgrams.Rdata")
                %in% d$files)
    stopifnot((d$completed - d$requested) > 1)
    stopifnot(dir.exists(d$path))
    stopifnot(file.exists(file.path(d$path, d$fileName)))

    # Validate Irma Lab
    stopifnot(i$stateId == "Irma-2017-09-11-1")
    stopifnot(i$class == "Lab")
    stopifnot(i$objectName == "Irma")
    stopifnot(i$path == "./NLPStudio/Achives/Labs/Irma")
    stopifnot(i$fileName == "Irma-2017-09-11-1-Lab-class-object-files")
    stopifnot(c("contractions.csv", "en_US.news.txt", "quadgrams.Rdata", "testgrams.Rdata")
              %in% i$files)
    stopifnot((i$completed - i$requested) > 1)
    stopifnot(dir.exists(i$path))
    stopifnot(file.exists(file.path(i$path, i$fileName)))

    # Validate blue Lab
    stopifnot(b$stateId == "blue-2017-09-11-1")
    stopifnot(b$class == "Lab")
    stopifnot(b$objectName == "blue")
    stopifnot(b$path == "./NLPStudio/Achives/Labs/blue")
    stopifnot(b$fileName == "blue-2017-09-11-1-Lab-class-object-files")
    stopifnot(b$files = character(0))
    stopifnot((b$completed - b$requested) < 1)
    stopifnot(!dir.exists(b$path))

    # Logit
    logTests(cls = "VisitorArchive", mthd = "visitLab", note = "Successfully compressed and saved files for the Lab objects.")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  #TODO: Design test for document collections and documents.
init()
test0()
test1()

}

testVisitorArchive()
