
test_that("getPositionLetters works", {
  expect_equal(getPositionLetters("C8"), "C")
  expect_equal(getPositionLetters("C8;H9"), "C;H")
})

test_that("getPositionNumbers works", {
  expect_equal(getPositionNumbers("C8"), "8")
  expect_equal(getPositionNumbers("C8;H9"), "8;9")
  expect_equal(getPositionNumbers("C;H9"), "0;9")
})

test_that("modPositions works", {
  expect_equal(modPositions(OVATable("peptide")$Modifications[26]),
               data.frame(modifications = c('Carbamidomethyl','Phospho'),
                          positions = c('C15','S;T'),
                          letters = c('C','S;T'),
                          numbers = c('15','0;0')))
  expect_equal(modPositions(OVATable("peptide")$Modifications[1], returnEmptyRow = FALSE),
               data.frame(modifications = c(''),
                          positions = c(''),
                          letters = c(''),
                          numbers = c('')))
  expect_equal(modPositions(OVATable("peptide")$Modifications[1]),
               data.frame(modifications = as.character(),
                          positions = as.character(),
                          letters = as.character(),
                          numbers = as.character()))
})

test_that("modPositionsToDF works", {
  expect_equal(modPositions(OVATable("peptide")$Modifications[32]) |> modPositionsToDF(),
               data.frame(modifications = c('Carbamidomethyl,Phospho'),
                          positions = c('C12,S7'),
                          letters = c('C,S'),
                          numbers = c('12,7')))
})

test_that("dfToModPositions works",{
  expect_equal(modPositions(OVATable("peptide")$Modifications[32]) |>
                 modPositionsToDF() |>
                 dfToModPositions(),
               modPositions(OVATable("peptide")$Modifications[32]))
})

test_that("getModPositionInProtein works", {
  theModifications <- purrr::map_df(OVATable("peptide")$Modifications,
                                    ~modPositions(.x, returnEmptyRow = FALSE) |>
                                      modPositionsToDF(returnEmptyRow = FALSE))
  newTable <- dplyr::bind_cols(OVATable("peptide"), theModifications)
  expect_equal(getModPositionInProtein(peptideTableRow = newTable[15,], Accession = "P01012",
                                       whichModification = "Oxidation", positionColumn = "PositionsinProteins"),
               data.frame(position = c(196),
                          clear = c(1)))
  expect_equal(getModPositionInProtein(peptideTableRow = newTable[26,], Accession = "P01012",
                                       whichModification = "Phospho", positionColumn = "PositionsinProteins"),
               data.frame(position = c(68,75,76,81,82),
                          clear = c(2,2,2,2,2)))
  expect_equal(purrr::map_df(1:nrow(newTable),
                             ~getModPositionInProtein(newTable[.x,], Accession = "P01012",
                                                      whichModification = "Phospho", positionColumn = "PositionsinProteins")) |>
                 dplyr::distinct(position, clear) |> dplyr::arrange(position),
               data.frame(position = c( 68, 68, 75, 76, 81, 82,344),
                          clear = c(2,1,2,2,2,2,1)))
})

test_that("getAllModPositionInProtein works", {
  testValue <- getAllModPositionInProtein(peptideTable = OVATable("peptide"), Accession = "P01012",
                                          whichModification = c("Carbamidomethyl","Phospho"),
                                          positionColumn = "PositionsinProteins")
  expect_equal(names(testValue), c("Carbamidomethyl","Phospho" ))
  expect_equal(testValue[[1]], data.frame(position = c( 73,120,367),
                                          clear = c(1,1,1)))
  expect_equal(testValue[[2]], data.frame(position = c( 68, 68, 75, 76, 81, 82,344),
                                          clear = c(2,1,2,2,2,2,1)))
})

test_that("whichModifications works", {
  expect_equal(whichModifications('Oxidation , Phospho, Carbamidomethyl'),
               c("Oxidation", "Phospho", "Carbamidomethyl"))
  expect_equal(whichModifications(c('Oxidation , Phospho','Carbamidomethyl, Phospho')),
               c("Oxidation", "Phospho", "Carbamidomethyl"))
})

test_that("containsModification works", {
  expect_equal(containsModification(c('Oxidation , Phospho','Carbamidomethyl, Phospho'),
                                    whichModification = "Phospho", trim = TRUE), c(TRUE, TRUE))
  expect_equal(containsModification(c('Oxidation , Phospho','Carbamidomethyl, Phospho'),
                                    whichModification = "Phosph"), c(FALSE, FALSE))
  expect_equal(containsModification(c('Oxidation , Phospho','Carbamidomethyl, Phospho'),
                                    whichModification = "Phosph", strict = FALSE), c(TRUE, TRUE))
})
