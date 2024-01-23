
test_that("getPositions works", {
  expect_equal(getPositions("P00000 [352-360]; P01012 [351-359]", Accession = "P01012"), "351-359")
  expect_equal(getPositions("P00000 [352-360]; P01012 [351-359]", Accession = "P0", exact = FALSE),
               c("352-360", "351-359"))
})

test_that("startPosition & endPosition work", {
  expect_equal(startPosition("901-902"), 901)
  expect_equal(endPosition("901-902"), 902)
  expect_equal(startPosition("1-102"), 1)
  expect_equal(endPosition("1-102"), 102)
})

test_that("getPeptideStart works", {
  expect_equal(getPeptideStart(proteinSequence = standardProtein(), peptideSequence = "DILNQITK"),
               86)
  expect_equal(sum(getPeptideStart(proteinSequence = standardProtein("BSA"),
                                   peptideSequence = "(?<!P)R|(?<!P)K")), 23730)
})

test_that("prepare & createProteinData work", {
  testValue <- createProteinData(sequence = standardProtein("bsa"),
                                 start = 25, nterm = 1, cterm = 610)
  expect_equal(nrow(testValue), 610)
  expect_equal(nchar(stringr::str_trim(paste(testValue$sequence, collapse = ""), side = "both")),
               583)
  expect_equal(sum(testValue$position), 186355)
})

test_that("proteinCoverage works", {
  testValue <- proteinCoverage(OVATable(), prepareColumns = 50)
  expect_equal(nrow(testValue), 400)
  expect_equal(levels(testValue$row), as.character(1:50))
  expect_equal(levels(testValue$col), as.character(1:8))
  expect_equal(nchar(paste(testValue$sequence, collapse = "")), 385)
  expect_equal(sum(as.integer(testValue$coverage)), 381)
  testValue <- proteinCoverage(sequence = substr(standardProtein(), 2, 386),
                               peptideTable = OVATable("peptide"),
                               Accession = "P01012", positionColumn = "PositionsinProteins")
  expect_equal(nrow(testValue), 385)
  expect_equal(nchar(paste(testValue$sequence, collapse = "")), 385)
  expect_equal(sum(as.integer(testValue$coverage)), 241)
})

test_that("addDataToProtein works", {
  testValue <- createProteinData(sequence = standardProtein("bsa"), nterm = 1,
                                 start = 25)
  trypticSites <- data.frame(position = testValue$position[
   testValue$sequence == "K" | testValue$sequence == "R"],
   data = "tryptic")
  testValue <- addDataToProtein(proteinDF = testValue, dataframe = trypticSites,
                                dataColumn = "data", dataName = "trypticSite", NAValue = "")
  expect_equal(nrow(testValue), 607)
  expect_equal(sum(testValue$trypticSite == "tryptic"), 82)
  expect_equal(sum(which(testValue$trypticSite == "tryptic")), 25933)
})

test_that("mapPeptidesToProtein works", {
  testValue <- createProteinData(sequence = standardProtein("OVA"), start = 2, nterm = 1, )
  testValue <- mapPeptidesToProtein(proteinDF = testValue, peptideTable = OVATable("peptide"),
                                    Accession = "P01012", positionColumn = "PositionsinProteins", variable = "Abundances_1",
                                    dataName = "SummedAbundances")
  testValue <- mapPeptidesToProtein(proteinDF = testValue, peptideTable = OVATable("peptide"),
                                    Accession = "P01012", positionColumn = "PositionsinProteins", variable = "Abundances_1",
                                    dataName = "Abundances", combineFunction = NA)
  expect_equal(sum(which(testValue$SummedAbundances > 1)), 50495)
  expect_equal(sum(testValue$SummedAbundances), 608472879676)
  expect_equal(sum(which(testValue$Abundances > 1)), 50495)
  expect_equal(format(sum(testValue$Abundances), digits = 5), "3.1479e+11")
})
