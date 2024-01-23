
test_that("standardProtein works", {
  expect_equal(nchar(standardProtein()), 386)
  expect_equal(nchar(standardProtein("bsa")), 607)
})

test_that("OVATable works",{
  expect_equal(nrow(OVATable()), 1)
  expect_equal(nrow(OVATable("Protein")), 1)
  expect_equal(nchar(OVATable()$Sequence), 385)
  expect_equal(nrow(OVATable("Peptide")), 36)
  expect_equal(OVATable("peptide")$Sequence[1], "ISQAVHAAHAEINEAGR")
  expect_equal(sum(OVATable("peptide")$Abundances_1), 46950687975)

})

test_that("rowsAndColumns works",{
  expect_equal(rowsAndcolumns(385, 5), c(columns = 45, rows = 9))
  expect_equal(rowsAndcolumns(400, 10), c(columns = 60, rows = 7))
})

test_that("boxDimensions works",{
  expect_equal(boxDimensions(385, 5), c(ncol = 45, nrow = 9))
  expect_equal(boxDimensions(400, ncol = 60), c(ncol = 60, nrow = 7))
})

test_that("boxData works",{
  testValue <- boxData(createProteinData(standardProtein(), start = 2, nterm = 1), ncols = 50)
  expect_equal(nrow(testValue), 400)
  expect_equal(levels(testValue$row), as.character(1:50))
  expect_equal(levels(testValue$col), as.character(1:8))
  expect_equal(paste(testValue$sequence[1:10], collapse =""), " GSIGAASME")
  expect_equal(paste(testValue$sequence[381:400], collapse =""), "GRCVSP")
})
