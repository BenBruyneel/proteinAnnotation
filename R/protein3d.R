
# ---- Helper functions ----

#' @title log10add
#'
#' @description function factoru that generates a function that takes a numeric value, adds a
#'  specified value and then gives the logarithmic value with base 10. This is similair tp
#'  \code{\link{log1p}}. It prevents problems with zero. It does add a small value to
#'  the original value, but as this is here meant for log10 transformation of values over a
#'  large range (like Abundances of peptides in a digest), it should not be a problem.
#'
#' @param valueToAdd numeric value to add to each value before taking the log10 in the resulting
#'  function
#'
#' @return function
#' @export
#'
#' @examples
#' log10(0:10)
#' log10add(1)(0:10)
#' log10add(0)(0:10)
#' log10(c(0,1E5, 1E7, 1E8))
#' log10add(1)(c(0,1E5, 1E7, 1E8))
#' log10add(1E3)(c(0,1E5, 1E7, 1E8))
log10add <- function(valueToAdd){
  force(valueToAdd)
  function(value){
    return(log10(value+valueToAdd))
  }
}

#' @title log10p
#'
#' @description takes a numeric value, adds 1 and then gives the logarithmic value with base 10.
#'  This is essentially the log10 version of \code{\link{log1p}}. This prevents problems with
#'  zero. It does add a small value to the original value, but as this is here meant for log10
#'  transformation of values over a large range (like Abundances of peptides in a digest),
#'  so it should not be a problem.
#'
#' @param value numeric value
#'
#' @return numeric value
#' @export
#'
#' @examples
#' log10(0:10)
#' log10p(0:10)
#' log10(c(0,1E5, 1E7, 1E8))
#' log10p(c(0,1E5, 1E7, 1E8))
log10p <- function(value){
  return(log10add(1)(value))
}

# ---- Translation functions ----

#' @title translateToFixedColors
#'
#' @description function factory that generates a function that 'tramslates' (maps) protein data
#'  to colors. Meant for non-continous data. If the colors are then used as a columnn for the
#'  protein data data.frame, it can be used modify colors on a 3d protein object. See
#'  \code{\link{addStyleElement}} for more info'
#'
#' @param translateDF data.frame with two columns: one has the different data elements that need to
#'  be translated to a certain color. The second column should be named 'color' and should contain
#'  the colors to which the values in the first column are translated.
#'  Examples: data.frame(position = 1:10, color = c("blue","red")) or
#'  data.frame(residue = c("C", "A", "P"), color = c("yellow", "red", "blue"))
#' @param notPresentColor color(code) for when a value is not in the translation data.frame
#'
#' @return function that takes the argument 'value', which it then translates the argument to
#'  its corresponding color(code)s. The function is vectorized so the argument can be multi-element
#' @export
#'
#' @examples
#' # create protein data
#' ovaProtein <- proteinCoverage(sequence = standardProtein(),
#'  peptideTable = OVATable("peptide"), Accession = "P01012",
#'  positionColumn = "PositionsinProteins", shiftPosition = 1)
#' # add tryptic sites
#' ovaProtein <- addDataToProtein(proteinDF = ovaProtein,
#'  dataframe = data.frame(position = getPeptideStart(proteinSequence = standardProtein(),
#'  peptideSequence = "(?<!P)R|(?<!P)K"), data = "tryptic"), dataColumn = "data",
#'  dataName = "realTrypticSite", NAValue = "non tryptic")
#' # add colors for the tryptic sites
#' ovaProtein$trypticColor <- translateToFixedColors(translateDF =
#'  data.frame(position = c("tryptic"), color = "red"),
#'  notPresentColor = "white")(ovaProtein$realTrypticSite)
#' # add colors for proline positions
#' ovaProtein$proline <- translateToFixedColors(translateDF = data.frame(sequence = "P",
#'  color = "red"), notPresentColor = "white")(ovaProtein$sequence)
#' ovaProtein |> dplyr::slice(15:20)
translateToFixedColors <- function(translateDF, notPresentColor){
  force(translateDF)
  force(notPresentColor)
  colnames(translateDF)[colnames(translateDF) != "color"] <- "value"
  function(value){
    purrr::map_chr(value, ~if (.x %in% translateDF[,"value"]) {translateDF$color[translateDF[,"value"] == .x]} else { notPresentColor})
  }
}

#' @title translateRangeColors
#'
#' @description function factory that generates a color translation function which assigns a color
#'  based on whether the argument value is between two values or if the argument value is part of a
#'  range values
#'
#' @param rangeValues numeric vector, range of values: if 'inbetween' is TRUE, then the resulting
#'  function will determine if a value falls in between the first two values. If 'inbetween' is
#'  FALSE, then the resulting function will check if a value is part of the rangeValues argument.
#'  Note that in that case one can work with non-numeric values, see example#'
#' @param insideColor color to assign to values that fall within or are part of a range
#' @param outsideColor color to assign to values that fall outside or are not part of a range
#' @param inbetween logical value that determines the type of range checking
#'
#' @return a function that takes the argument 'value', which it then translates to the correct color
#'  (vectorized)
#' @export
#'
#' @examples
#' translateRangeToColors(rangeValues = c(1, 10), insideColor = "white")(1:20)
#' translateRangeToColors(rangeValues = c(1, 10), insideColor = "white", inbetween = FALSE)(1:20)
#' translateRangeToColors(rangeValues = 1:10, insideColor = "white", inbetween = FALSE)(1:20)
#' translateRangeToColors(rangeValues = c(1,2,3, 11, 12, 13), insideColor = "white",
#'  outsideColor = "red", inbetween = FALSE)(1:20)
#' translateRangeToColors(rangeValues = c("A","B","C"), insideColor = "white",
#'  outsideColor = "red", inbetween = FALSE)(LETTERS)
translateRangeToColors <- function(rangeValues, insideColor, outsideColor = NA, inbetween = TRUE){
  force(rangeValues)
  force(insideColor)
  force(outsideColor)
  force(inbetween)
  if (inbetween){
    function(value){
      return(ifelse(dplyr::between(value, rangeValues[1], rangeValues[2]),
                    insideColor,
                    outsideColor))
    }
  } else {
    function(value){
      return(ifelse(value %in% rangeValues,
                    insideColor,
                    outsideColor))
    }
  }
}

# ranges = data.frame(rangeValues, insideColor, columnName, inbetween (logical))


#' @title translateRangesToColors
#'
#' @description more advanced function that takes a data.frame of information and a protein data
#'  data.frame and generates colors matching the instructions in the information data.frame applied
#'  to the protein data.frame. This allows for more complicated coloring schemes
#'
#' @param ranges data.frame with 4 columns: 'rangeValues', 'insideColor', 'inbetween'
#'  (similair in use to in \code{\link{translateRangeToColors}}) & 'columnName' which determines
#'  from which column, in the protein data data.frame, values should be taken to determine the
#'  colors. Note: that there can be colors in the earlier rows of this data.frame will be
#'  overwritten if needed by colors in later rows if needed
#' @param proteinData a proteinData data.frame with at least the columns specified in the ranges
#'  data.frame
#' @param outsideColor color to apply to any row in the proteinData that falls outside any of the
#'  ranges
#'
#' @return a vector of colors with the same length as the number of rows in the protein data
#' @export
#'
#' @examples
#' # create protein data
#' ovaProtein <- proteinCoverage(sequence = standardProtein(),
#'  peptideTable = OVATable("peptide"), Accession = "P01012",
#'  positionColumn = "PositionsinProteins", shiftPosition = 1)
#' # create ranges data.frame
#' dfColors <- data.frame(rangeValues = NA,
#' insideColors = c("#0053D6", "#65CBF3", "#FFDB13", "#FF7D45"),
#' columnName = "position", inbetween = FALSE)
#' dfColors$rangeValues <- list(c(1:10), c(11:20), c(21:30), c(31:40))
#' dfColors
#' translateRangesToColors(ranges = dfColors, proteinData = ovaProtein,
#'  outsideColor = "white")[1:45]
#' # create ranges data.frame
#' dfColors <- data.frame(rangeValues = NA,
#' insideColors = c("#0053D6", "#65CBF3", "#FFDB13", "#FF7D45"),
#' columnName = "sequence", inbetween = FALSE)
#' dfColors$rangeValues <- list(c("A","C","D"), c("E","F"), c("G","H"), c("K","L"))
#' dfColors
#' translateRangesToColors(ranges = dfColors, proteinData = ovaProtein,
#'  outsideColor = "white")[1:45]
translateRangesToColors <- function(ranges,
                                    proteinData, outsideColor){
  if ((nrow(proteinData) == 1) | (nrow(ranges) == 1)){
    stop("Error in proteinData, ranges or positionColumn does not exist in proteinData")
  }
  newColor<- rep(NA, nrow(proteinData))
  for (rowCounter in 1:nrow(proteinData)){
    for (rangesCounter in 1:nrow(ranges)){
      if (ranges$inbetween[rangesCounter]){
        if (dplyr::between(proteinData[rowCounter, ranges$columnName[rangesCounter]],
                           ranges$rangeValues[[rangesCounter]][1], ranges$rangeValues[[rangesCounter]][2])){
          newColor[rowCounter] <- ranges$insideColor[rangesCounter]
        }
      } else {
        if (proteinData[rowCounter,
                        ranges$columnName[rangesCounter]] %in% ranges$rangeValues[[rangesCounter]]){
          newColor[rowCounter] <- ranges$insideColor[rangesCounter]
        }
      }
    }
  }
  newColor[is.na(newColor)] <- outsideColor
  return(newColor)
}

#' @title translateToGradientColors
#'
#' @description function factory that generates a function that 'tramslates' (maps) protein data
#'  to colors. Meant for continous data. If the colors are then used as a columnn for the
#'  protein data data.frame, it can be used modify colors on a 3d protein object. See
#'  \code{\link{addStyleElement}} for more info'
#'
#' @param minValue minimum value to use when translating to colors. If a value is lower than this,
#'  it will be set to this
#' @param maxValue maximum value to use when translating to colors. If a value is higher than thism
#'  it will be set to this
#' @param colorSteps essentially the resolution of the gradient. If set to 1000 (default), then
#'  a gradient of 1000 individual colors will be created to translate to. Do not set excessively high
#'  or low
#' @param colors two element character vector. Specifies the colors for the minimumValue amd the
#'  maximumValue. The translation will have 'colorSteps' gradations between these two colors
#' @param transformFunction allows for passing a transformation function to the values to be
#'  translated. The minValue and maxValue arguments will also be transformed. This can be used where
#'  the value-range is (very) large
#'
#' @return function that takes the argument 'value', which is then used to translate the argument to
#'  the corresponding color(code)s. The function is vectorized so the argument can be multi-element.
#'  Note: the 'transformFunction' specified is applied to the argument!
#' @export
#'
#' @examples
#' # create protein data
#' ovaProtein <- proteinCoverage(sequence = standardProtein(),
#'  peptideTable = OVATable("peptide"), Accession = "P01012",
#'  positionColumn = "PositionsinProteins", shiftPosition = 1)
#' # mapping Abundance information
#' ovaProtein <- mapPeptidesToProtein(proteinDF = ovaProtein, peptideTable = OVATable("peptide"),
#'  Accession = "P01012", positionColumn = "PositionsinProteins",
#'  shiftPosition = 1L, variable = "Abundances_1",
#'  dataName = "Abundance", combineFunction = sum)
#' # create the color translation function
#' linearColors <- translateToGradientColors(minValue = min(ovaProtein$Abundance),
#'  maxValue = max(ovaProtein$Abundance),
#'  colorSteps = 1000, colors = c("white", "blue"))
#' linearColors(c(0, 1E5, 1E9))
#' # create a logarithmic translation function
#' logColors <- translateToGradientColors(minValue = 1E3,
#'  maxValue = 1E10,
#'  colorSteps = 1000, colors = c("white", "blue"), transformFunction = log10p)
#' logColors(c(0, 1E5, 1E9))
#' # this doesn't work, due to log10 not generating NA's
#' logColors <- translateToGradientColors(minValue = min(ovaProtein$Abundance),
#'  maxValue = max(ovaProtein$Abundance),
#'  colorSteps = 1000, colors = c("white", "blue"), transformFunction = log10)
#' logColors(c(0, 1E5, 1E9))
#' # this does work
#' logColors <- translateToGradientColors(minValue = min(ovaProtein$Abundance),
#'  maxValue = max(ovaProtein$Abundance),
#'  colorSteps = 1000, colors = c("white", "blue"), transformFunction = log10p)
#' logColors(c(0, 1E5, 1E9))
#' # limit the value range
#' logColors <- translateToGradientColors(minValue = 1E3,
#'  maxValue = max(ovaProtein$Abundance),
#'  colorSteps = 1000, colors = c("white", "blue"), transformFunction = log10)
#' logColors(c(0, 1E5, 1E9))
#' # same result in a different way
#' logColors <- translateToGradientColors(minValue = min(ovaProtein$Abundance),
#'  maxValue = max(ovaProtein$Abundance),
#'  colorSteps = 1000, colors = c("white", "blue"), transformFunction = log10add(1E3))
#' logColors(c(0, 1E5, 1E9))
#' # add the colors to the protein data
#' ovaProtein$AbundanceColors <- linearColors(ovaProtein$Abundance)
#' ovaProtein$AbundanceLogColors <- logColors(ovaProtein$Abundance)
#' ovaProtein |> dplyr::slice(35:45)
#' ovaProtein |> dplyr::slice(360:375)
translateToGradientColors <- function(minValue, maxValue,
                                      colorSteps = 1000, colors,
                                      transformFunction = NA){
  force(minValue)
  force(maxValue)
  force(colorSteps)
  force(colors)
  force(transformFunction)
  if (!identical(transformFunction, NA)){
    minValue <- transformFunction(minValue)
    maxValue <- transformFunction(maxValue)
  }
  ramp <- (maxValue - minValue)/(colorSteps)
  colors <- grDevices::colorRampPalette(colors)(colorSteps+1)
  if (!identical(transformFunction, NA)){
    function(value){
      value <- transformFunction(value)
      value <- unlist(lapply(value, function(x){max(c(x, minValue), na.rm = T)}))
      value <- unlist(lapply(value, function(x){min(c(x, maxValue), na.rm = T)}))
      return(colors[round(((value - minValue)/ramp)+1)])
    }
  } else {
    function(value){
      value <- unlist(lapply(value, function(x){max(c(x, minValue), na.rm = T)}))
      value <- unlist(lapply(value, function(x){min(c(x, maxValue), na.rm = T)}))
      return(colors[round(((value - minValue)/ramp)+1)])
    }
  }
}

#' @title reduceColors
#'
#' @description takes protein data with color information and creates a smaller data.frame contaning
#'  three columns: start, end (both positions) and color. This can be used for mapping the colors
#'  onto the 3D object. It is used inside the
#'
#' @note internal function
#'
#' @param proteinDF protein data data.frame
#' @param positionColumn name of the column containing the position information
#' @param colorColumn name of the column containing the color information
#'
#' @return data.frame with three columns: start, end (both positions) and color
reduceColors <- function(proteinDF, positionColumn = "position", colorColumn){
  proteinDF <- proteinDF %>% dplyr::arrange(!!dplyr::sym(positionColumn))
  result <- data.frame(start = proteinDF[1, positionColumn],
                       end = proteinDF[1, positionColumn],
                       color = proteinDF[1, colorColumn])
  for (counter in 2:nrow(proteinDF)){
    if (proteinDF[counter, colorColumn] != result$color[nrow(result)]){
      result <- rbind(result, data.frame(start = proteinDF[counter, positionColumn],
                                         end = proteinDF[counter, positionColumn],
                                         color = proteinDF[counter, colorColumn]))
    } else {
      result$end[nrow(result)] <- proteinDF[counter, positionColumn]
    }
  }
  return(result)
}

# ---- 3D style ----

#' @title addStyleElement
#'
#' @description takes a 3d object of the class 'r3dmol' as created via the 'r3dmol' library amd
#'  adds the colors as defined in the protein data. The function \code{\link{m_add_style}} is used
#'  for this.
#'
#' @param object3D a r3dmol object of a protein information file (pdf, cif)
#' @param proteinDF protein data data.frame with at least two columns: position & color
#' @param positionColumn name of the column containing the positions to be colored (default: 'position')
#' @param colorColumn name of the column containing the colors to be used for the positions
#' @param colorsReduce this allows for a smaller number of style objects to be added to the
#'  object3D
#' @param style function that defines the style to be 'added' to the object3D object (default:
#'  \code{\link{m_style_cartoon}})
#' @param ... for adding sdditional argunebts to the style-function. For 'm_style_cartoon', e.g.
#'  \code{arrows = FALSE} can be used.
#'
#' @return a r3dmol object
#' @export
#'
#' @examples
#' # create protein data
#' ovaProtein <- proteinCoverage(sequence = standardProtein(),
#'  peptideTable = OVATable("peptide"), Accession = "P01012",
#'  positionColumn = "PositionsinProteins", shiftPosition = 1)
#' # mapping Abundance information
#' ovaProtein <- mapPeptidesToProtein(proteinDF = ovaProtein, peptideTable = OVATable("peptide"),
#'  Accession = "P01012", positionColumn = "PositionsinProteins",
#'  shiftPosition = 1L, variable = "Abundances_1",
#'  dataName = "Abundance", combineFunction = sum)
#' # translate the colors
#' logColors <- translateToGradientColors(minValue = 1E3,
#'  maxValue = max(ovaProtein$Abundance),
#'  colorSteps = 1000, colors = c("white", "blue"), transformFunction = log10)
#' ovaProtein$AbundanceLogColors <- logColors(ovaProtein$Abundance)
#' library(r3dmol)
#' # get pdb file from alphafold
#' downloadedFile <- tempfile(fileext = ".pdb")
#' download.file("https://alphafold.ebi.ac.uk/files/AF-P01012-F1-model_v4.pdb",
#'  destfile = downloadedFile)
#' # create r3dmol object
#' \dontrun{
#' p3d <- r3dmol(viewer_spec = m_viewer_spec(
#'   cartoonQuality = 20,
#'   lowerZoomLimit = 50,
#'   upperZoomLimit = 350
#' )) %>%
#'   m_add_model(downloadedFile) %>%
#'   m_set_style(style = m_style_cartoon(arrows = F)) %>%
#'   m_zoom_to()
#' p3d
#' # add colors
#' addStyleElement(p3d, proteinDF = ovaProtein, colorColumn = "AbundanceColors", arrows = FALSE)
#' addStyleElement(p3d, proteinDF = ovaProtein, colorColumn = "AbundanceLogColors", arrows = FALSE)
#' }
addStyleElement <- function(object3D,
                            proteinDF, positionColumn = "position", colorColumn,
                            colorsReduce = T,
                            style = r3dmol::m_style_cartoon, ...){
  if (colorsReduce){
    proteinDF <- reduceColors(proteinDF, positionColumn = positionColumn, colorColumn = colorColumn)
    for (counter in 1:nrow(proteinDF)){
      object3D <- object3D %>%
        r3dmol::m_add_style(
          style = c(
            style(color = proteinDF$color[counter], ...)
          ),
          sel = r3dmol::m_sel(resi = proteinDF$start[counter]:proteinDF$end[counter])
        )
    }
  } else {
    for (counter in 1:nrow(proteinDF)){
      object3D <- object3D %>%
        r3dmol::m_add_style(
          style = c(
            style(color = proteinDF[counter, colorColumn], ...)
          ),
          sel = r3dmol::m_sel(resi = proteinDF[counter, positionColumn])
        )
    }
  }
  return(object3D)
}
