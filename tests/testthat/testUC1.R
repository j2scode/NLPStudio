testthat::context("Use Case 1: Source Document")

testthat::test_that("Test 1: nlpStudio has been created on load", {

  # rm(list=ls(), envir = .GlobalEnv)
  # detach(name = ".R_Cache")
  # SOAR::Remove(SOAR::Objects())


  studio <<- nlpStudio$getStudio(verbose = TRUE)
  expect_equal(studio$name, "nlpStudio")
  expect_equal(studio$desc, "NLPStudio: Natural Language Processing Studio")
  expect_equal(studio$currentLab, "None")
  expect_match("POSIXct", class(studio$created)[1])
  expect_match("POSIXct", class(studio$modified)[1])
  rm(list = ls(), envir = .GlobalEnv)
})

testthat::test_that("Test 2: nlpStudio has been recovered from cache", {

  SOAR::Store("nlpStudio", remove = FALSE)
  studio2 <<- nlpStudio$getStudio(verbose = TRUE)
  expect_equal(studio2$name, "nlpStudio")
  expect_equal(studio2$desc, "NLPStudio: Natural Language Processing Studio")
  expect_equal(studio2$currentLab, "None")
  expect_match("POSIXct", class(studio2$created)[1])
  expect_match("POSIXct", class(studio2$modified)[1])
})

