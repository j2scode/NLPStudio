testState <- function() {

  test0 <- function() {

    test <- "test0: Initialization"
    cat(paste("\n",test, " Commencing\r"))

    # Create Objects
    Lab$new("alex", "Alex's Lab")
    DocumentCollection$new("stanford", "Standford Collection")
    Document$new("stanford-news", "Stanford News")
    Document$new("stanford-finance", "Stanford Finance")
    Document$new("stanford-sports", "Stanford Sports")

    # Create relationships
    stanford$addChild(stanford-news)
    alex$addChild(stanford)

    # Create Files
    copyFiles("stanford", "./")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  test1 <- function() {
    test <- "test1: Validate Parameters"
    cat(paste("\n",test, " Commencing\r"))

    # Get Key
    tools <- Tools$new()
    key <- tools$makeRandomString()

    # Validation
    State

    # Test validation
    StateServer$saveState() # Fail, stateId missing
    StateServer$saveState(object = Development) # Fail, stateId missing
    StateServer$saveState("oxford-2017-09-12-1") # Fail,  object missing
    StateServer$saveState("oxford-2017-09-12-1", class) # Fail, object wrong class As Expected!

    # Logit
    logTests(class = "StateServer", mthd = "saveState", note = "Validation logic performs as expected")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  # Test 1: Test StateServer saveState for Document Functionality
  test1 <- function() {
    test <- "test1: StateServer: saveState Document Functionality"
    cat(paste("\n",test, " Commencing\r"))

    # Archive Labs
    d <- StateServer$saveState("oxfordNews-2017-09-12-1", Development)

    # Check Receipt for Document Object
    stopifnot(d$request == "Save")
    stopifnot(d$stateId == "oxfordNews-2017-09-12-1")
    stopifnot(d$class == "Document")
    stopifnot(d$name == "oxfordNewst")
    stopifnot(d$desc == "Oxford News Register")
    stopifnot(d$parentName == "oxford")
    stopifnot(d$path == "./NLPStudio/Labs/Development/oxford/oxford-news-csv")
    stopifnot(d$fileName == "oxford-news-csv"),
    stopifnot(isTRUE(all.equal(asDate(Sys.time()), as.Date(d$saved))))

    # Check object
    stopifnot(exists(d$name, envir = .GlobalEnv))

    # Confirm files are restored
    stopifnot(dir.exists(d$path))
    if (length(d$fileName) > 0)  stopifnot(file.exists(d$fileName))

    # Validate Irma Lab
    stopifnot(i$stateId == "Irma-2017-09-12-1")
    stopifnot(i$class == "Lab")
    stopifnot(i$objectName == "Irma")
    stopifnot(i$path == "./NLPStudio/Achives/Labs/Irma")
    stopifnot(i$fileName == "Irma-2017-09-12-1-Lab-class-object-files")
    stopifnot(c("contractions.csv", "en_US.news.txt", "quadgrams.Rdata", "testgrams.Rdata")
              %in% i$files)
    stopifnot((i$completed - i$requested) > 1)
    stopifnot(dir.exists(i$path))
    stopifnot(file.exists(file.path(i$path, i$fileName)))

    # Validate blue Lab
    stopifnot(b$stateId == "blue-2017-09-12-1")
    stopifnot(b$class == "Lab")
    stopifnot(b$objectName == "blue")
    stopifnot(b$path == "./NLPStudio/Achives/Labs/blue")
    stopifnot(b$fileName == "blue-2017-09-12-1-Lab-class-object-files")
    stopifnot(b$files == character(0))
    stopifnot((b$completed - b$requested) < 1)
    stopifnot(!dir.exists(b$path))

    # Logit
    logTests(class = "StateServer", mthd = "saveState", note = "Successfully compressed and saved files for the Lab objects.")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  # Test 2: Test StateServer Archive Collection Functionality
  test2 <- function() {
    test <- "test2: StateServer: Archive Collection Functionality"
    cat(paste("\n",test, " Commencing\r"))

    # Archive Labs
    o <- StateServer$visitDocumentCollection("stanford-2017-09-12-1", stanford)
    s <- StateServer$visitDocumentCollection("oxford-2017-09-12-1", oxford)

    # Get Archive Information
    o <- o$getArchive()
    s <- s$getArchive()

    # Validate Stanford Collection
    stopifnot(s$stateId == "stanford-2017-09-12-1")
    stopifnot(s$class == "DocumentCollection")
    stopifnot(s$objectName == "stanford")
    stopifnot(s$path == "./NLPStudio/Achives/Collections/stanford")
    stopifnot(s$fileName == "stanford-2017-09-12-1-DocumentCollection-class-object-files")
    stopifnot(c("contractions.csv", "en_US.news.txt", "quadgrams.Rdata", "testgrams.Rdata")
              %in% s$files)
    stopifnot((s$completed - s$requested) > 1)
    stopifnot(dir.exists(s$path))
    stopifnot(file.exists(file.path(s$path, s$fileName)))

    # Validate Oxford Collection
    stopifnot(o$stateId == "oxford-2017-09-12-1")
    stopifnot(o$class == "DocumentCollection")
    stopifnot(o$objectName == "oxford")
    stopifnot(o$path == "./NLPStudio/Achives/Collections/oxford")
    stopifnot(o$fileName == "oxford-2017-09-12-1-DocumentCollection-class-object-files")
    stopifnot(c("contractions.csv", "en_US.news.txt", "quadgrams.Rdata", "testgrams.Rdata")
              %in% o$files)
    stopifnot((o$completed - o$requested) > 1)
    stopifnot(dir.exists(o$path))
    stopifnot(file.exists(file.path(o$path, o$fileName)))

    # Logit
    logTests(class = "StateServer", mthd = "visitDocumentCollection", note = "Successfully compressed and saved files for the DocumentCollection objects.")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

  # Test 3: Test StateServer Archive Document Functionality
  test3 <- function() {
    test <- "test3: StateServer: Archive Document Functionality"
    cat(paste("\n",test, " Commencing\r"))

    # Archive Labs
    sf1 <- StateServer$visitDocument("stanfordFinance-2017-09-12-1", stanfordFinance)
    sf2 <- StateServer$visitDocument("stanfordFinance-2017-09-12-2", stanfordFinance)
    ss1 <- StateServer$visitDocument("stanfordSports-2017-09-12-1", stanfordSports)
    ss2 <- StateServer$visitDocument("stanfordSports-2017-09-12-2", stanfordSports)
    on <- StateServer$visitDocument("oxfordNews-2017-09-12-1", oxfordNews)
    of <- StateServer$visitDocument("oxfordFinance-2017-09-12-1", oxfordFinance)
    os <- StateServer$visitDocument("oxfordSports-2017-09-12-1", oxfordSports)

    # Get Archive Information
    sf1 <- sf1$getArchive()
    sf2 <- sf2$getArchive()
    ss1 <- ss1$getArchive()
    ss2 <- ss2$getArchive()
    on <- on$getArchive()
    of <- of$getArchive()
    os <- os$getArchive()


    # Validate Stanford Finance Document1
    stopifnot(sf1$stateId == "stanfordFinance-2017-09-12-1")
    stopifnot(sf1$class == "Document")
    stopifnot(sf1$objectName == "stanfordFinance")
    stopifnot(sf1$path == "./NLPStudio/Achives/Documents/stanford")
    stopifnot(sf1$fileName == "stanfordFinance-2017-09-12-1-Document-class-object-files")
    stopifnot(c("contractions.csv", "en_US.news.txt", "quadgrams.Rdata", "testgrams.Rdata")
              %in% sf1$files)
    stopifnot((sf1$completed - sf1$requested) > 1)
    stopifnot(dir.exists(sf1$path))
    stopifnot(file.exists(file.path(sf1$path, sf1$fileName)))

    # Validate Stanford Finance Document2
    stopifnot(sf2$stateId == "stanfordFinance-2017-09-12-2")
    stopifnot(sf2$class == "Document")
    stopifnot(sf2$objectName == "stanfordFinance")
    stopifnot(sf2$path == "./NLPStudio/Achives/Documents/stanford")
    stopifnot(sf2$fileName == "stanfordFinance-2017-09-12-2-Document-class-object-files")
    stopifnot(c("contractions.csv", "en_US.news.txt", "quadgrams.Rdata", "testgrams.Rdata")
              %in% sf2$files)
    stopifnot((sf2$completed - sf2$requested) > 1)
    stopifnot(dir.exists(sf2$path))
    stopifnot(file.exists(file.path(sf2$path, sf2$fileName)))

    # Validate Stanford Sports Document1
    stopifnot(ss1$stateId == "stanfordSports-2017-09-12-1")
    stopifnot(ss1$class == "Document")
    stopifnot(ss1$objectName == "stanfordSports")
    stopifnot(ss1$path == "./NLPStudio/Achives/Documents/stanford")
    stopifnot(ss1$fileName == "stanfordSports-2017-09-12-1-Document-class-object-files")
    stopifnot(c("contractions.csv", "en_US.news.txt", "quadgrams.Rdata", "testgrams.Rdata")
              %in% ss1$files)
    stopifnot((ss1$completed - ss1$requested) > 1)
    stopifnot(dir.exists(ss1$path))
    stopifnot(file.exists(file.path(ss1$path, ss1$fileName)))

    # Validate Stanford Sports Document2
    stopifnot(ss2$stateId == "stanfordSports-2017-09-12-2")
    stopifnot(ss2$class == "Document")
    stopifnot(ss2$objectName == "stanfordSports")
    stopifnot(ss2$path == "./NLPStudio/Achives/Documents/stanford")
    stopifnot(ss2$fileName == "stanfordSports-2017-09-12-2-Document-class-object-files")
    stopifnot(c("contractions.csv", "en_US.news.txt", "quadgrams.Rdata", "testgrams.Rdata")
              %in% ss2$files)
    stopifnot((ss2$completed - ss2$requested) > 1)
    stopifnot(dir.exists(ss2$path))
    stopifnot(file.exists(file.path(ss2$path, ss2$fileName)))


    # Validate Stanford Oxford News Document
    stopifnot(on$stateId == "oxfordNews-2017-09-12-1")
    stopifnot(on$class == "Document")
    stopifnot(on$objectName == "oxfordNews")
    stopifnot(on$path == "./NLPStudio/Achives/Documents/oxford")
    stopifnot(on$fileName == "oxfordNews-2017-09-12-1-Document-class-object-files")
    stopifnot(c("contractions.csv", "en_US.news.txt", "quadgrams.Rdata", "testgrams.Rdata")
              %in% on$files)
    stopifnot((on$completed - on$requested) > 1)
    stopifnot(dir.exists(on$path))
    stopifnot(file.exists(file.path(on$path, on$fileName)))

    # Validate Stanford Oxford Sports Document
    stopifnot(os$stateId == "oxfordSports-2017-09-12-1")
    stopifnot(os$class == "Document")
    stopifnot(os$objectName == "oxfordSports")
    stopifnot(os$path == "./NLPStudio/Achives/Documents/oxford")
    stopifnot(os$fileName == "oxfordSports-2017-09-12-1-Document-class-object-files")
    stopifnot(c("contractions.csv", "en_US.news.txt", "quadgrams.Rdata", "testgrams.Rdata")
              %in% os$files)
    stopifnot((os$completed - os$requested) > 1)
    stopifnot(dir.exists(os$path))
    stopifnot(file.exists(file.path(os$path, os$fileName)))


    # Validate Stanford Oxford Finance Document
    stopifnot(of$stateId == "oxfordFinance-2017-09-12-1")
    stopifnot(of$class == "Document")
    stopifnot(of$objectName == "oxfordFinance")
    stopifnot(of$path == "./NLPStudio/Achives/Documnents/oxford")
    stopifnot(of$fileName == "oxfordFinance-2017-09-12-1-Document-class-object-files")
    stopifnot(c("contractions.csv", "en_US.news.txt", "quadgrams.Rdata", "testgrams.Rdata")
              %in% of$files)
    stopifnot((of$completed - of$requested) > 1)
    stopifnot(dir.exists(of$path))
    stopifnot(file.exists(file.path(of$path, of$fileName)))


    # Logit
    logTests(class = "StateServer", mthd = "visitDocumentCollection", note = "Successfully compressed and saved files for the DocumentCollection objects.")

    cat(paste("\n", test, " Completed: Success!\n"))
  }

testInit()
test0()
test1()
test2()
test3()

}

testState()
