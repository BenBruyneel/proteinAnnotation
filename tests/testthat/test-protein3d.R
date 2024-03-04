
test_that("log10add & log10p works", {
  expect_equal(sum(log10add(1)(0:10)) |> format(digits = 10),
               "7.601155718")
  expect_equal(sum(log10p(0:10)) |> format(digits = 10),
               "7.601155718")
})

test_that("translateToFixedColors works",{
  ovaProtein <- proteinCoverage(sequence = standardProtein(),
   peptideTable = OVATable("peptide"), Accession = "P01012",
   positionColumn = "PositionsinProteins", shiftPosition = 1)
  ovaProtein <- addDataToProtein(proteinDF = ovaProtein,
   dataframe = data.frame(position = getPeptideStart(proteinSequence = standardProtein(),
   peptideSequence = "(?<!P)R|(?<!P)K"), data = "tryptic"), dataColumn = "data",
   dataName = "realTrypticSite", NAValue = "non tryptic")
  testValue <- translateToFixedColors(translateDF = data.frame(position = c("tryptic"),
                                                               color = "red"),
                                      notPresentColor = "white")(ovaProtein$realTrypticSite)
  expect_equal(table(testValue)[[1]], 34)
  expect_equal(table(testValue)[[2]], 352)
})

test_that("translateRangeColors works", {
  expect_equal(translateRangeToColors(rangeValues = c(1, 10), insideColor = "white")(1:20),
               c(rep("white", 10), rep(NA, 10)))
})

test_that("translateRangesColors works", {
  ovaProtein <- proteinCoverage(sequence = standardProtein(),
   peptideTable = OVATable("peptide"), Accession = "P01012",
   positionColumn = "PositionsinProteins", shiftPosition = 1)
  dfColors <- data.frame(rangeValues = NA,
  insideColors = c("#0053D6", "#65CBF3", "#FFDB13", "#FF7D45"),
  columnName = "position", inbetween = FALSE)
  dfColors$rangeValues <- list(c(1:10), c(11:20), c(21:30), c(31:40))
  expect_equal(table(translateRangesToColors(ranges = dfColors, proteinData = ovaProtein,
                                             outsideColor = "white"))[[1]], 10)
  expect_equal(table(translateRangesToColors(ranges = dfColors, proteinData = ovaProtein,
                                             outsideColor = "white"))[[2]], 10)
  expect_equal(table(translateRangesToColors(ranges = dfColors, proteinData = ovaProtein,
                                             outsideColor = "white"))[[3]], 10)
  expect_equal(table(translateRangesToColors(ranges = dfColors, proteinData = ovaProtein,
                                             outsideColor = "white"))[[4]], 10)
  expect_equal(table(translateRangesToColors(ranges = dfColors, proteinData = ovaProtein,
                                             outsideColor = "white"))[[5]], 346)
  expect_equal(names(table(translateRangesToColors(ranges = dfColors, proteinData = ovaProtein,
                                                   outsideColor = "white")))[1], "#0053D6")
  expect_equal(names(table(translateRangesToColors(ranges = dfColors, proteinData = ovaProtein,
                                                   outsideColor = "white")))[2], "#65CBF3")
  expect_equal(names(table(translateRangesToColors(ranges = dfColors, proteinData = ovaProtein,
                                                   outsideColor = "white")))[3], "#FF7D45")
  expect_equal(names(table(translateRangesToColors(ranges = dfColors, proteinData = ovaProtein,
                                                   outsideColor = "white")))[4], "#FFDB13")
  expect_equal(names(table(translateRangesToColors(ranges = dfColors, proteinData = ovaProtein,
                                                   outsideColor = "white")))[5], "white")
})

test_that("translateToGradientColors works", {
  linearColors <- translateToGradientColors(minValue = 1,
   maxValue = 1E9,
   colorSteps = 1000, colors = c("white", "blue"))
  expect_equal(linearColors(c(1, 1E5, 1E9)), c("#FFFFFF","#FFFFFF","#0000FF"))
  logColors <- translateToGradientColors(minValue = 1,
                                         maxValue = 1E9,
                                         colorSteps = 1000, colors = c("white", "blue"),
                                         transformFunction = log10)
  expect_equal(logColors(c(0, 1E5, 1E9)), c("#FFFFFF", "#7171FF", "#0000FF"))

})
