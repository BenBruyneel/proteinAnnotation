
# ---- helper functions for protein data ----

#' @title getPositions
#' @description Gets the part of the string containing position information. Works with
#'  data organized in the 'PositionsinProteins' and/or 'PositionsinMasterProteins'
#'  column of peptide tables as defined in Proteome Discoverer
#'
#' @param string character vector in the format: 'Accession' 'position'; 'Accession' 'position'; etc
#' @param Accession character vector that specifies from which protein Accession to get the position
#' @param exact logical vector which defines how the Accession argument is compared to the string argument.
#'  Default is TRUE which means that the Accession needs to be exactly the same letters/numbers.
#'  If FALSE then a 'grepl' statement is used with the Accession as a pattern
#' @param splitCharacter character vector which specifies the beginning/end of 'Accession' 'position' pair.
#'  Default is ";" which is used by Proteome Discoverer
#'
#' @return a character vector containing the position of the specified Accession
#' @export
#'
#' @note this function expects Accessions to be unique. If this is not true or the parameter exact is set to FALSE
#'  it may return character vectors with length > 1
#'
#' @examples
#' getPositions("P00000 [352-360]; P01012 [351-359]", Accession = "P01012")
#' getPositions("P00000 [352-360]; P01012 [351-359]", Accession = "P00000")
#' getPositions("P00000 [352-360]; P01012 [351-359]", Accession = "P0", exact = FALSE)
getPositions <- function(string, Accession, exact = TRUE, splitCharacter = ";"){
  string <- unlist(stringr::str_split(string, pattern = splitCharacter))
  if (exact){
    string <- strsplit(stringr::str_trim(string, side = "both"), split = " ")
    if (length(string)>0){
      whichString <- which(purrr::map_lgl(string, ~.x[1] == Accession))
    } else {
      whichString <- integer(0)
    }
    if (length(whichString) != 0){
      string <- purrr::map_chr(whichString, ~string[[.x]][2])
    } else {
      string <- character(0)
    }
  } else {
    string <- stringr::str_trim(string[grepl(string, pattern = Accession)], side = "both")
  }
  string <- ifelseProper(identical(string, character(0)), NA, string)
  if (!identical(string, NA)){
    string <- purrr::map_chr(string, ~strMultiReplaceAll(stringr::str_extract(.x, pattern = "\\[.*\\]"), patterns = c("\\[","\\]"), replacement = ""))
  }
  return(string)
}

#' @title startPosition
#'
#' @description Gives the start position of a character vector with format 'xxx-yyy'.
#'  Convenience function
#'
#' @param string character vector with format 'xxx-yyy', where xxx is the start
#'  position and yyy is the end position
#' @param splitCharacter default is "-", which is used by Proteome Discoverer
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' startPosition("901-902")
#' startPosition("1-102")
startPosition <- function(string, splitCharacter = "-"){
  as.numeric(stringr::str_replace(stringr::str_extract(string, pattern = "\\d*-"),
                                  pattern = "-", replacement = ""))
}

#' @title endPosition
#'
#' @description Gives the end position of a character vector with format 'xxx-yyy'.
#'  Convenience function
#'
#' @param string character vector with format 'xxx-yyy', where xxx is the start
#'  position and yyy is the end position
#' @param splitCharacter default is "-", which is used by Proteome Discoverer
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' endPosition("901-902")
#' endPosition("1-102")
endPosition <- function(string, splitCharacter = "-"){
  as.numeric(stringr::str_replace(stringr::str_extract(string, pattern = "-\\d*"),
                                  pattern = "-", replacement = ""))
}

#' @title getPeptideStart
#'
#' @description Helper function to get the start positions of amino acid sequences. The function
#'  works with either a protein sequence and the peptide sequence to be searched
#'  (regular expressions can be used) or a peptide table (data.frame) containing
#'  position information in the format used by Proteome Discoverer. See also
#'  \code{\link{getPositions}}
#'
#' @param peptideTable data.frame having a.position column in the Proteome
#'  Discoverer format (see \code{\link{getPositions}})
#' @param Accession character vector that specifies from which protein Accession to get the position
#' @param positionColumn character vector which specifies which column contains the peptide position information. Proteome Discoverer uses either
#'  'PositionsinProteins' or 'PositionsinMasterProteins' (depending on the settings used in the consensus method)
#' @param exact logical vector which defines how the Accession argument is compared to the string argument.
#'  Default is TRUE which means that the Accession needs to be exactly the same letters/numbers. If FALSE then
#'  a 'grepl' statement is used with the Accession as a pattern
#' @param proteinSequence character vector representing the protein sequence to be search. Ignored if peptideTable is not NA
#' @param peptideSequence character vector representing the peptide sequence to search for. Regular Expressions can be used
#'  (see examples). Ignored if peptideTable is not NA
#' @param noWarnings logical vector, default is TRUE: no warnings when a peptide sequence is present more than once in a protein sequence. If FALSE then
#'  a warning will be generated. Ignored if peptideTable is not NA
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' # find all start positions for all peptides in the peptide table
#' getPeptideStart(peptideTable = OVATable("peptide"), Accession = "P01012",
#'  positionColumn = "PositionsinProteins")
#' # find all locations of a peptide sequence
#' getPeptideStart(proteinSequence = standardProtein(), peptideSequence = "DILNQITK")
#' # find all locations of a single amino acid
#' getPeptideStart(proteinSequence = standardProtein(), peptideSequence = "P", noWarnings = FALSE)
#' # find all tryptic sites, not taking into account Proline (P)
#' getPeptideStart(proteinSequence = standardProtein(), peptideSequence = "K|R")
#' getPeptideStart(proteinSequence = standardProtein("BSA"), peptideSequence = "K|R")
#' # find all tryptic sites w/o a preceding Proline (P)
#' getPeptideStart(proteinSequence = standardProtein(), peptideSequence = "(?<!P)R|(?<!P)K")
#' getPeptideStart(proteinSequence = standardProtein("BSA"), peptideSequence = "(?<!P)R|(?<!P)K")
getPeptideStart <- function(peptideTable = NA,
                            Accession = "", positionColumn, exact = TRUE,
                            proteinSequence = NA, peptideSequence = NA,
                            noWarnings = TRUE){
  if (!identical(peptideTable, NA)){
    return(getPositions(peptideTable[,positionColumn], Accession = Accession, exact = exact) |> startPosition())
  } else {
    result <- as.data.frame(stringr::str_locate_all(proteinSequence, pattern = peptideSequence)[[1]])$start
    if (length(result) == 0){
      return(NA)
    } else {
      if ((length(result) > 1) & !noWarnings){
        warning("Peptide sequence is present more than once in the protein sequence")
      }
      return(as.data.frame(stringr::str_locate_all(proteinSequence, pattern = peptideSequence)[[1]])$start)
    }
  }
}

# ---- protein data functions ----

#' @title prepare
#'
#' @description Helper function to add (empty) rows to proteinData,
#'
#' @param data data.frame with two columns: position (integer vector) and sequence (amino acid letters: character vector)
#' @param start integer, should be less than the start position (in first row of data): adds positions with 'emptySequence' to the head of data
#' @param end integer, should be maximum positione: adds positions with 'emptySequence' to the tail of data
#' @param emptySequence default " ", character vector for the sequence column to use when adding rows. Note: other columns will also be
#'  extended and usually add NA's as a value
#'
#' @return data.frame with two columns: position (integer vector) and sequence (amino acid letters: character vector)
#' @export
#'
#' @examples
#' createProteinData(sequence = standardProtein("bsa"), start = 25) |> prepare(start = 1) |> head()
#' createProteinData(sequence = standardProtein("bsa"), start = 25) |> prepare(end = 610) |> tail()
prepare <- function(data,
                    start = min(data$position),
                    end = max(data$position),
                    emptySequence = " "){
  toAdd <- min(data$position) - start
  if (toAdd >= 1){
    data <- dplyr::bind_rows(data.frame(position = start:(start+toAdd-1), sequence = emptySequence), data)
  }
  toAdd <- end - max(data$position)
  if (toAdd >= 1){
    data <- dplyr::bind_rows(data, data.frame(position = (max(data$position)+1):end, sequence = emptySequence))
  }
  return(data)
}

#' @title createProteinData
#'
#' @description Create a protein data.frame with basic information: position & sequence. In the resulting data.frame, every row
#'  has an amino acid (letter, sequence column) and the position of that amino acid in the protein (position column)
#'
#' @param sequence character vector, protein sequence in the form of the usual letter codes
#' @param start first position in the sequence to be used
#' @param end last position in the sequence to be used
#' @param nterm start position 'adjustment'. Adds a number of 'empty' rows to the head of the resulting data.frame to have it start at position nterm
#' @param cterm end position 'adjustment'. Adds a number of 'empty' rows to the tail of the resulting data.frame to have it ends at position cterm
#' @param emptySequence default " ", character vector for the sequence column to use when adding rows. Note: other columns will also be
#'  extended and usually add NA's as a value
#'
#' @note nterm stands for n-terminus, cterm stands for c-terminus
#'
#' @return data.frame with two columns: position (integer vector) and sequence (amino acid letters: character vector)
#' @export
#'
#' @examples
#' createProteinData(sequence = standardProtein("bsa"), start = 25) |> head(25)
#' createProteinData(sequence = standardProtein("bsa"), nterm = 1, start = 25) |> head(25)
#' createProteinData(sequence = standardProtein("bsa"), start = 25) |> tail(10)
#' createProteinData(sequence = standardProtein("bsa"), start = 25, nterm = 1, cterm = 610) |>
#'  tail(10)
createProteinData <- function(sequence, start = 1, end = nchar(sequence),
                              nterm = NA, cterm = NA, emptySequence = " "){
  result <- data.frame(position = start:end,
                       sequence = stringr::str_split(stringr::str_sub(sequence, start, end)[[1]], pattern = "")[[1]])
  if (!is.na(nterm) | !is.na(cterm)){
    nterm <- ifelse(is.na(nterm),
                    min(result$position),
                    nterm)
    cterm <- ifelse(is.na(cterm),
                    max(result$position),
                    cterm)
    result <- prepare(data = result,
                      start = nterm, end = cterm,
                      emptySequence = emptySequence)
  }
  return(result)
}

# ---- Coverage -----

#' @title proteinCoverage
#'
#' @description Create a protein data.frame with basic information: position, sequence & coverage. In the resulting data.frame, every row
#'  has an amino acid (letter, sequence column), the position of that amino acid in the protein (position column) and a coverage
#'  column where 0 is no coverage and 1 is coverage.
#'
#' The function can work with a protein table row (as in Proteome Discoverer results), which should contain the Sequence and
#'  the SequenceCoverage columns. An alternative is using a sequence character vector containing a protein amino acid sequence
#'  and a peptide table (as in Proteome Discoverer results) of that same sequence (protein).
#'
#' @param proteinTableRow data.frame row that has at least a Sequence column with the whole protein amino acid sequence and a
#'  SequenceCoverage column specifying the coverage. The SequenceCoverage needs to be in format: 'full sequence length';'start-end 1st part';'start-end 2nd part'; etc etc
#'  The start-end parts together specify which parts of the protein have been sequenced (coverage). Note: the positions SequenceCoverage field in the proteinTableow are
#'  zero based: the first amino acid position is zero, not one! This behavior is consistent with protein tables as used by Proteome Discoverer and can be changed by
#'  the proteinTableRowZero argument.
#' @param checkSequenceLength logical vector: default is TRUE, defined for odd situations where there may be malformed SequenceCoverage fields. Might be removed in future versions
#' @param noWarnings logical vector: default is FALSE. If the check for the sequence length fails, it will generate a warning. If TRUE then no warning is generated. Might be removed in future versions
#' @param prepareColumns defines the number of columns for a 2D matrix (puts the amino acid letters in a rows*columns matrix)
#' @param prepareRows defines the number of rows for a 2D matrix (puts the amino acid letters in a rows*columns matrix)
#' @param emptySequence character vector: is used for the added space if the matrix is bigger than the amino acid sequence
#' @param noCoverage defines what to use when a position is not 'covered'. Can be a number, logical or character. It will be changed into
#'  a character vector at the end of the function. Default is 0 (integer)
#' @param coverage defines what to use when a position is 'covered'. Can be a number, logical or character. It will be changed into
#'  a character vector at the end of the function. Default is 1 (integer)
#' @param sequence character vector, protein sequence in the form of the usual letter codes.
#'  Ignored when proteinTableRow is not NA.
#' @param start first position in the sequence to be used. Ignored when proteinTableRow is not NA.
#' @param end last position in the sequence to be used. Ignored when proteinTableRow is not NA.
#' @param nterm start position 'adjustment'. Adds a number of 'empty' rows to the head of the resulting data.frame to have it start at position nterm.
#'  Ignored when proteinTableRow is not NA.
#' @param cterm end position 'adjustment'. Adds a number of 'empty' rows to the tail of the resulting data.frame to have it ends at position cterm
#' @param emptySequence default "-", character vector for the sequence column to use when adding rows. Note: other columns will also be
#'  extended and usually add NA's as a value. Ignored when proteinTableRow is not NA.
#' @param peptideTable data.frame containing the peptide information to be mapped to the sequence. Sholud contain either a column named 'PositionsinProteins'
#'  or 'PositionsinMasterProteins'. This column should be character vectors of one or more of following element: <Accession> \[start:end\];
#'  Ignored when proteinTableRow is not NA.
#' @param Accession character vector that specifies from which protein Accession to get the position. This is needed for when using the peptideTable as
#'  a peptide may be present in more than one protein. Ignored when proteinTableRow is not NA.
#' @param positionColumn character vector which specifies which column contains the peptide position information. Proteome Discoverer uses either
#'  'PositionsinProteins' or 'PositionsinMasterProteins' (depending on the settings used in the consensus method)
#'
#' @note it is possible to use both prepareColumns and prepareRows at the same time, but this may lead to errors and unexpected results.
#'  It is therefore not recommended to do so.
#'
#' @return data.frame with same format as output by \code{\link{createProteinData}} or \code{\link{boxData}} with the 'coverage' column added. The values
#'  in the coverage column are either 0 (no coverage) or 1 (coverage)
#' @export
#'
#' @examples
#' proteinCoverage(OVATable())
#' proteinCoverage(OVATable(), prepareColumns = 50)
#' proteinCoverage(OVATable()) |> boxData(ncols = 50)
#' proteinCoverage(sequence = substr(standardProtein(), 2, 386), peptideTable = OVATable("peptide"),
#'  Accession = "P01012", positionColumn = "PositionsinProteins")
proteinCoverage <- function(proteinTableRow = NA,
                            checkSequenceLength = TRUE, noWarnings = FALSE,
                            prepareColumns = NA, prepareRows = NA, emptySequence = "",
                            noCoverage = 0, coverage = 1,
                            sequence = NA, start = 1, end = ifelse(is.na(sequence), 1, nchar(sequence)),
                            nterm = NA, cterm = NA,
                            peptideTable, Accession, positionColumn){
  if (!identical(proteinTableRow, NA)){
    xtra <- 1 # because in protein table the protein sequence starts at zero
    proteinData <- createProteinData(sequence = proteinTableRow$Sequence)
    coverageParts <- strsplit(proteinTableRow$SequenceCoverage, split = ";")[[1]] # first one should be simply sequence length
    if (checkSequenceLength){
      if (nrow(proteinData) != as.integer(coverageParts[1])){
        warning("Error: protein sequence length does not match SequenceCoverage first element")
      }
    }
  } else {
    xtra <- 0
    proteinData <- createProteinData(sequence = sequence, start = start, end = end, nterm = nterm, cterm = cterm, emptySequence = emptySequence)
    coverageParts <- unique(purrr::map_chr(peptideTable[,positionColumn], ~getPositions(.x, Accession = Accession)))
  }
  if (!is.na(prepareColumns) | !(is.na(prepareRows))){
    boxDims <- boxDimensions(vectorLength = nrow(proteinData), ncol = prepareColumns, nrow = prepareRows)
    proteinData <- boxData(prepare(proteinData,
                                   start = 1, end = boxDims[1]*boxDims[2],
                                   emptySequence = emptySequence),
                           ncols = boxDims[1])
  }
  proteinData$coverage <- noCoverage
  if (!identical(proteinTableRow, NA)){
    coverageParts <- coverageParts[-1]
  }
  for (counter in 1:length(coverageParts)){
    startEnd <- as.integer(strsplit(coverageParts[counter], split = "-")[[1]])
    proteinData$coverage[(startEnd[1]+xtra):(startEnd[2]+xtra)] <- coverage
  }
  proteinData$coverage <- as.character(proteinData$coverage)
  return(proteinData)
}

# ---- protein data mainpulation ----

#' @title addDataToProtein
#'
#' @description Takes data.frame with position data and adds it as an extra column to a protein data data.frame
#' Positions not covered in the added data.frame are set to the argument NAValue (default is integer 0)
#'
#' @param proteinDF the protein data.frame to add the info to. Main requirements are that there is a column named
#' position, that there is no column named 'newData____' (internal use) and no column with the same name as defined
#' by the 'dataName' argument
#' @param dataframe data.frame with the data to be added. Should have two columns: one with the position info and
#' another with the data relating to these positions
#' @param dataColumn name of the column in dataframe with the data to be added
#' @param dataName name of the new data column in the protein data data.frame
#' @param positionColumn name of the position column in dataframe which has the position info (default is 'position')
#' @param NAValue value for the position in the protein data data.frame which are missing in the (new) dataframe data
#' @param strictPosition logical vector. Default is TRUE: if the positions in the (new) dataframe are outside the range
#' of the protein data, then an error will be generated. If FALSE, then only a warning is generated
#'
#' @return data.frame
#' @export
#'
#' @examples
#' bsaProtein <- createProteinData(sequence = standardProtein("bsa"), nterm = 1,
#'  start = 25)
#' toAdd <- data.frame(position = c(25:30, 100:110), data = c(rep(1,10),
#'  rep(3, 3),rep(2,4)))
#' bsaProtein <- addDataToProtein(proteinDF = bsaProtein, dataframe = toAdd,
#'  dataColumn = "data", dataName = "found")
#' bsaProtein$position[bsaProtein$sequence == "K" | bsaProtein$sequence == "R"]
#' trypticSites <- data.frame(position = bsaProtein$position[
#'  bsaProtein$sequence == "K" | bsaProtein$sequence == "R"],
#'  data = "tryptic")
#' bsaProtein <- addDataToProtein(proteinDF = bsaProtein, dataframe = trypticSites,
#'  dataColumn = "data", dataName = "trypticSite", NAValue = "")
#' bsaProtein[95:110,]
addDataToProtein <- function(proteinDF,
                             dataframe, dataColumn, dataName, positionColumn = "position", NAValue = 0L,
                             strictPosition = TRUE){
  if (dataName %in% colnames(proteinDF)){
    stop(paste(c("Protein data data.frame already has a column named '", dataName, "'!"), collapse = ""))
  }
  proteinDF$newData____ <- NAValue
  colnames(proteinDF)[which(colnames(proteinDF) == "newData____")] <- dataName
  minPosition <- min(proteinDF[, "position"], na.rm = T)
  maxPosition <- max(proteinDF[, "position"], na.rm = T)
  for (counter in 1:nrow(dataframe)){
    if ((dataframe[counter, positionColumn] >= minPosition) &
        (dataframe[counter, positionColumn] <= maxPosition)){
      proteinDF[dataframe[counter, positionColumn], dataName] <- dataframe[counter, dataColumn]
    } else {
      if (strictPosition){
        stop("New data contains positions which are outside the positions in the protein data data.frame")
      } else {
        warning("New data contains positions which are outside the positions in the protein data data.frame")
      }
    }
  }
  # proteinDF[is.na(proteinDF[,dataName]),dataName] <- NAValue
  return(proteinDF)
}

#' @title mapPeptidesToProtein
#'
#' @description takes a data.frame with peptide information (like a peptide table from Proteome Discoverer) and adds data from it to the proteinData
#'
#' @param proteinDF the protein data.frame to add the info to. Main requirements are that there is a column named
#'  position, that there is no column named 'newData____' (internal use) and no column with the same name as defined
#'  by the 'dataName' argument
#' @param peptideTable a data.frame with at least a position column (see 'positionColumn' argument) and a column with the same name as the 'variable' argument.
#'  This is what Proteome Discoverer produces
#' @param Accession character vector that specifies from which protein Accession to get the position
#' @param positionColumn character vector which specifies which column contains the peptide position information. Proteome Discoverer uses either
#'  'PositionsinProteins' or 'PositionsinMasterProteins' (depending on the settings used in the consensus method)
#' @param variable character vector: name of the column in the 'peptideTable' that needs to be mapped onto the protein data
#' @param dataName character vector: name of the new column in the protein data data.frame
#' @param NAValue vector to be used for positions where no peptide data maps to
#' @param combineFunction function (default is \code{\link{sum}} ) that is used for positions which have more than one peptide row mapping to it. In case of 'sum',
#'  the values are summed for that position, for the function 'max' the maximum value is taken etc etc. Note: only basic functions min, max, sum and mean have been
#'  tested
#' @param na.rm default is TRUE. If TRUE then 'na.rm=TRUE' is added to the 'combineFunction', If FALSE, then it is ignored
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' newTable <- createProteinData(sequence = standardProtein("OVA"), start = 2, nterm = 1, )
#' newTable |> head()
#' newTable <- mapPeptidesToProtein(proteinDF = newTable, peptideTable = OVATable("peptide"),
#'  Accession = "P01012", positionColumn = "PositionsinProteins", variable = "Abundances_1",
#'  dataName = "SummedAbundances")
#' newTable |> head()
#' newTable <- mapPeptidesToProtein(proteinDF = newTable, peptideTable = OVATable("peptide"),
#'  Accession = "P01012", positionColumn = "PositionsinProteins", variable = "Abundances_1",
#'  dataName = "Abundances", combineFunction = NA)
#' newTable[35:45,]
#' newTable[100:125,]
mapPeptidesToProtein <- function(proteinDF,
                                 peptideTable, Accession, positionColumn,
                                 variable, dataName = variable, NAValue = 0L,
                                 combineFunction = sum, na.rm = TRUE){
  proteinDF$newVariable____ <- NAValue
  for (rowCounter in 1:nrow(peptideTable)){
    startstop <- getPositions(peptideTable[rowCounter, positionColumn], Accession = Accession)
    for (positionCounter in startPosition(startstop):endPosition(startstop)){
      if (!identical(combineFunction, NA)){
        if (na.rm){
          proteinDF$newVariable____[positionCounter] <- combineFunction(c(proteinDF$newVariable____[positionCounter], peptideTable[rowCounter, variable]), na.rm = na.rm)
        } else {
          proteinDF$newVariable____[positionCounter] <- combineFunction(c(proteinDF$newVariable____[positionCounter], peptideTable[rowCounter, variable]))
        }
      } else {
        proteinDF$newVariable____[positionCounter] <- peptideTable[rowCounter, variable]
      }
    }
  }
  proteinDF <- proteinDF %>% dplyr::rename(!!dplyr::sym(dataName) := !!dplyr::sym("newVariable____"))
  return(proteinDF)
}
