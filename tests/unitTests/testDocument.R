testDocument <- function() {

  init <- function() {
    objs <- ls(pos = ".GlobalEnv")
    if (exists("blue", envir = .GlobalEnv))  rm(list = objs[grep("blue", objs)], pos = ".GlobalEnv")
    if (exists("green", envir = .GlobalEnv))  rm(list = objs[grep("green", objs)], pos = ".GlobalEnv")
    if (exists("caroline", envir = .GlobalEnv))  rm(list = objs[grep("caroline", objs)], pos = ".GlobalEnv")
    if (exists("cassie", envir = .GlobalEnv))  rm(list = objs[grep("cassie", objs)], pos = ".GlobalEnv")
  }

  # Test 0: Confirm instantiation of Document
  test0 <- function() {
    test <- "test0"
    cat(paste("\n",test, " Commencing\r"))
    cls <- "Document"

    # Error handling: Success 8/22/17
    #Document$new()   # Fail, no name, Success
    #Document$new(logTests): # Fail func as name
    #Document$new(9)
    #Document$new(name = 'blue')
    #Document$new(name = 'blue', fileName = 'blue.txt')

    # Success w/o description
    Document$new(name = 'blue', fileName = 'blue.txt', path = 'ocean')
    doc <- blue$getDocument()
    stopifnot(doc$name == "blue")
    stopifnot(doc$fileName == "blue.txt")
    stopifnot(doc$path == "ocean")
    stopifnot(doc$desc == "blue document")
    stopifnot((Sys.time()- doc$modified) <1 )
    stopifnot((Sys.time()- doc$created) <1 )

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)
    stopifnot(checkCache("blue") == TRUE)

    # Success w/o description
    #Document$new(name = 'blue', fileName = 'blue.txt', path = 'ocean', desc = 'blue ocean') # fail as duplicate
    Document$new(name = 'green', fileName = 'green.txt', path = 'pastures', desc = 'green pastures')
    #doc <- green$getDocument(verbose = 5)
    doc <- green$getDocument(verbose = TRUE)
    stopifnot(doc$name == "green")
    stopifnot(doc$fileName == "green.txt")
    stopifnot(doc$path == "pastures")
    stopifnot(doc$desc == "green pastures")
    stopifnot((Sys.time()- doc$modified) <1 )
    stopifnot((Sys.time()- doc$created) <1 )

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)
    stopifnot(checkCache("green") == TRUE)

    # Logit
    logTests(cls = cls, mthd = "initiate", note = "Successfully created document ww/o Desc, validation ok")
    logTests(cls = cls, mthd = "getDocument", note = "Success. Validation ok ")

    cat(paste("\n", test, " Completed: Success!\r"))
  }

  # Test 1: Document Collection Instantiation
  test1 <- function() {
    test <- "test1"
    cat(paste("\r",test, " Commencing\r"))
    cls <- "DocumentCollection"

    # Error handling: Success 8/22/17
    #DocumentCollection$new()
    #DocumentCollection$new(name = logTests)
    #DocumentCollection$new(name = "cassie")

    # Test successful instantiation
    DocumentCollection$new(name = "caroline", path = "voice")
    doc <- caroline$getDocument()
    stopifnot(doc$name == "caroline")
    stopifnot(doc$path == "voice")
    stopifnot(doc$desc == "caroline collection")
    stopifnot((Sys.time()- doc$modified) <1 )
    stopifnot((Sys.time()- doc$created) <1 )

    DocumentCollection$new(name = "cassie", path = "cassie", desc = "blue and red amore")
    doc <- cassie$getDocument()
    stopifnot(doc$name == "cassie")
    stopifnot(doc$path == "cassie")
    stopifnot(doc$desc == "blue and red amore")
    stopifnot((Sys.time()- doc$modified) <1 )
    stopifnot((Sys.time()- doc$created) <1 )

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)
    stopifnot(checkCache("green") == TRUE)
    stopifnot(checkCache("blue") == TRUE)
    stopifnot(checkCache("cassie") == TRUE)
    stopifnot(checkCache("caroline") == TRUE)


    # Logit
    logTests(cls = cls, mthd = "initiate", note = "Successfully created document ww/o Desc, validation ok")
    logTests(cls = cls, mthd = "getDocument", note = "Success. Validation ok ")

    cat(paste("\n", test, " Completed: Success!\r"))
  }

  # Test 1: Test add document
  test2 <- function() {
    test <- "test2"
    cat(paste("\n",test, " Commencing\r"))
    cls <- "DocumentCollection"

    # Error handling
    #caroline$addDocument("cls")
    #caroline$addDocument(Lab)

    # Successful addition
    caroline$addDocument(green)
    doc <- caroline$getDocument(verbose = FALSE)
    stopifnot(doc$documents[[1]]$name == "green")
    stopifnot(doc$documents[[1]]$fileName == "green.txt")
    stopifnot(doc$documents[[1]]$path == "pastures")
    stopifnot(doc$documents[[1]]$desc == "green pastures")
    stopifnot((Sys.time()- doc$modified) <1 )
    stopifnot((Sys.time()- doc$created) <1 ) # Should fail

    # Check cache
    stopifnot(checkCache("nlpStudio") == TRUE)
    stopifnot(checkCache("green") == TRUE)
    stopifnot(checkCache("blue") == TRUE)
    stopifnot(checkCache("cassie") == TRUE)
    stopifnot(checkCache("caroline") == TRUE)


    # Logit
    logTests(cls = cls, mthd = "addDocument", note = "Successfully added document to collection")
    logTests(cls = cls, mthd = "getDocument", note = "Success. Validation ok ")

    cat(paste("\n", test, " Completed: Success!\r"))
  }

init()
test0()
test1()
test2()

}

devtools::load_all()
testDocument()
