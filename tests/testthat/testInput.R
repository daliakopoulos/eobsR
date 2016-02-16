library(doParallel)
registerDoParallel(1)
library(eobsR)
adm0 <- raster::getData('GADM', country='NL', level=0)
context("Input testing:")

expect_error(importEOBS("foo"), "Variable foo not known.")
expect_error(importEOBS('tg', 'A', "foo"), "Period should be either Numeric, timeBased or ISO-8601 style.")
expect_error(importEOBS('tg', '2014', "foo"), "Area should be of class SpatialPolygons or SpatialPolygonsDataFrame.")
expect_error(importEOBS('tg', '2014', adm0, "foo"), "Grid should be specified correctly.")

context("Output testing:")
expect_equal_to_reference(importEOBS('tg', '2014', adm0, '0.50reg'), file="output.rds")
expect_equal_to_reference(importEOBS('rr', '2014', adm0, '0.50reg'), file="output_rr.rds")

stopImplicitCluster()
