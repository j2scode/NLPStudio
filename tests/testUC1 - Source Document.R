swipe <- function() {
  rm(list=ls(), envir = .GlobalEnv)
  detach(name = ".R_Cache", pos = -2)
}


testthat::context("Use Case 1: Source Document")

testthat::test_that("Test 1: nlpStudio has been created on load", {

  #swipe()
  SOAR::Remove(SOAR::Objects())
  studio <<- nlpStudio$getStudio(verbose = TRUE)
  testthat::expect_equal(studio$name, "nlpStudio")
  testthat::expect_equal(studio$desc, "NLPStudio: Natural Language Processing Studio")
  testthat::expect_equal(studio$currentLab, "None")
  testthat::expect_match("POSIXct", class(studio$created)[1])
  testthat::expect_match("POSIXct", class(studio$modified)[1])
  rm(list = ls(), envir = .GlobalEnv)
})

testthat::test_that("Test 2: nlpStudio has been recovered from cache", {

  SOAR::Remove(SOAR::Objects())
  SOAR::Store("nlpStudio", remove = FALSE)
  studio2 <<- nlpStudio$getStudio(verbose = TRUE)
  testthat::expect_equal(studio2$name, "nlpStudio")
  testthat::expect_equal(studio2$desc, "NLPStudio: Natural Language Processing Studio")
  testthat::expect_equal(studio2$currentLab, "None")
  testthat::expect_match("POSIXct", class(studio2$created)[1])
  testthat::expect_match("POSIXct", class(studio2$modified)[1])
})

# testthat::test_that("Test 3: Test alpha lab created and added to nlpStudio", {
#
#   SOAR::Remove(SOAR::Objects())
#   alpha <<- Lab$new(name = "alpha", desc = "Alpha Lab", TRUE)
#   #nlpStudio$addLab(alpha, current = TRUE)
#   #lab <<- alpha$getLab(verbose = TRUE)
#
#   testthat::expect_equal(lab$name, "alpha")
#   testthat::expect_equal(lab$desc, "Alpha Lab")
#   testthat::expect_match("POSIXct", class(lab$created)[1])
#   testthat::expect_match("POSIXct", class(lab$modified)[1])
# })
#
