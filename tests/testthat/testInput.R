library(eobsR)
context("Sanitize input")

expect_error(importEOBS("baf"), "Variable baf not known.")
expect_error(importEOBS('tg', '2014', "blah"), "Area should be spatial polygon object.")