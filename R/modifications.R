
#' @title getPositionLetters
#' @description helper function which extracts the letter codes (for amino acids) from the position info part of a modification
#'
#' @param position character vector with format <letter><number>;<letter><number>;... etc etc
#'
#' @return character vector with format <letter>;<letter>;... etc etc
#'
#' @note internal function
#'
# @examples
# getPositionLetters("C8")
# getPositionLetters("C8;H9")
getPositionLetters <- function(position){
  paste(purrr::map_chr(strsplit(position, split = ";")[[1]], ~stringr::str_extract(.x, pattern = "[:alpha:]*")), collapse = ";")
}

#' @title getPositionNumbers
#' @description helper function which extracts the positions from the position info part of a modification
#'
#' @param position character vector with format <letter><number>;<letter><number>;... etc etc
#'
#' @return character vector with format <letter>;<letter>;... etc etc
#'
#' @note internal function
#' @note position info which does not have a letter will return a zero as position
#'
# @examples
# getPositionNumbers("C8")
# getPositionNumbers("C8;H9")
# getPositionNumbers("C;H9")
getPositionNumbers <- function(position){
  paste(purrr::map_chr(strsplit(position, split = ";")[[1]],
                       ~stringr::str_extract(.x, pattern = "\\d+")) %>% tidyr::replace_na(replace = "0"), collapse = ";")

}

#' @title modPositions
#'
#' @description helper function which 'translates' a character vector representing a (peptide) modification to a data.frame
#'
#' @param modifications character vector representing one or more modifications. The format is 'number × modification name \[position<letter(+number)>\]'.
#'  If more than one modification is present, the character vector is 'combined' with separation character ';' (collapseChar)
#' @param collapseChar character vector separating the modifications in the argument 'modifications'
#' @param returnEmptyRow logical vector, default is TRUE. TRUE: if no modification is present in the 'modifications' argument, a data.frame will
#'  be returned without rows. FALSE: in that case a data.frame with a single row, with in every column an empty character vector, will be returned.
#'
#' @note if there are two or more modification present, then number of rows in the returned data.frame will also be two or more
#' @note the modification format is what Proteome Discoverer/Sequest works with in its peptide tables
#'
#' @return a data.frame with columns: modifications, positions, letters and numbers
#' @export
#'
#' @examples
#' OVATable("peptide")$Modifications[26]
#' modPositions(OVATable("peptide")$Modifications[26])
#' OVATable("peptide")$Modifications[2]
#' modPositions(OVATable("peptide")$Modifications[2])
#' OVATable("peptide")$Modifications[1]
#' modPositions(OVATable("peptide")$Modifications[1])
#' modPositions(OVATable("peptide")$Modifications[1], returnEmptyRow = FALSE)
#' purrr::map_df(OVATable("peptide")$Modifications, ~modPositions(.x))
modPositions <- function(modifications, collapseChar = ";", returnEmptyRow = TRUE){
  if ((modifications == "") | is.na(modifications)){
    return(data.frame(modifications = ifelseProper(returnEmptyRow,
                                                   as.character(),
                                                   ""),
                      positions = ifelseProper(returnEmptyRow,
                                               as.character(),
                                               ""),
                      letters = ifelseProper(returnEmptyRow,
                                             as.character(),
                                             ""),
                      numbers = ifelseProper(returnEmptyRow,
                                             as.character(),
                                             "")))
  }
  tempvar <- stringr::str_trim(strsplit(modifications, split = "]")[[1]], side = "both")
  tempvar <- strMultiReplaceAll(tempvar, patterns = c("\\[","; "))
  tempvar <- strsplit(tempvar, split = " ")
  moddf <- data.frame()
  for (counter in 1:length(tempvar)){
    moddf <- dplyr::bind_rows(moddf,
                              data.frame(
                                modifications = stringr::str_replace_all(tempvar[[counter]][1], pattern = paste0("\\d", DescTools::AscToChar(c(195, 151))), replacement = ""),
                                positions = paste(stringr::str_extract_all(tempvar[[counter]][2], pattern = "\\w{1}\\d*")[[1]], collapse = ";")))
  }
  moddf$letters <- NA
  moddf$numbers <- NA
  moddf
  for (counter in 1:nrow(moddf)){
    posits <- strsplit(moddf$positions[counter], split = ";")[[1]]
    moddf$letters[counter] <- paste(purrr::map_chr(posits, ~getPositionLetters(.x)), collapse = collapseChar)
    moddf$numbers[counter] <- paste(purrr::map_chr(posits, ~getPositionNumbers(.x)), collapse = collapseChar)
  }
  return(moddf)
}

#' @title modPositionsToDF
#'
#' @description helper function that 'collapses' two or more rows in a modification data.frane (coming from \code{\link{modPositions}})
#'  into a single data.frame row
#'
#' @param mods data.frame coming from \code{\link{modPositions}}
#' @param collapseChar character vector (default ',') which is used to collapse the columns of the rows (needed to undo
#'  the 'collapse' in \code{\link{dfToModPositions}})
#' @param returnEmptyRow logical vector, default is TRUE. TRUE: if no modification is present in the 'mods' argument, a data.frame will
#'  be returned without rows. FALSE: in that case a data.frame with a single row, with in every column an empty character vector, will be returned.
#'
#' @return a data.frame with columns: modifications, positions, letters and numbers
#' @export
#'
#' @examples
#' OVATable("peptide")$Modifications[32]
#' modPositions(OVATable("peptide")$Modifications[32])
#' modPositions(OVATable("peptide")$Modifications[32]) |> modPositionsToDF()
#' OVATable("peptide")$Modifications[2]
#' modPositions(OVATable("peptide")$Modifications[2])
#' modPositions(OVATable("peptide")$Modifications[2]) |> modPositionsToDF()
modPositionsToDF <- function(mods, collapseChar = ",", returnEmptyRow = TRUE){
  if ((nrow(mods) < 1) & returnEmptyRow){
    return(data.frame(modifications = as.character(),
                      positions = as.character(),
                      letters = as.character(),
                      numbers = as.character()))
  }
  return(data.frame(modifications = paste(mods$modifications, collapse = collapseChar),
                    positions = paste(mods$positions, collapse = collapseChar),
                    letters = paste(mods$letters, collapse = collapseChar),
                    numbers = paste(mods$numbers, collapse = collapseChar)))
}


#' @title dfToModPositions
#' @description Takes a single modification table row with one or more modification definitions (as produced by \code{\link{modPositionsToDF}})
#'  and makes it into a single-modification-per-row data.frame. A row with 'x' modifications becomes 'x' number of rows. Essentially
#'  the opposite of \code{\link{modPositionsToDF}}.
#'
#' @param df a single row data.frame having the 'collapsed' columns (modifications, positions, letters and numbers)
#'  Each modification(-type) is separated by the argument 'splitChar'
#' @param splitChar character vector specifying the separation 'character' that is placed between the modification definitions
#' @param returnEmptyRow  logical vector, default is TRUE. TRUE: if no modifications are present in the 'df' argument, a data.frame will
#'  be returned without rows. FALSE: in that case a data.frame with a single row, with in every column an empty character vector, will be returned.
#'
#' @return a data.frame with columns: modifications, positions, letters and numbers
#' @export
#'
#' @examples
#' modPositions(OVATable("peptide")$Modifications[32]) |> modPositionsToDF() |> dfToModPositions()
#' modPositions(OVATable("peptide")$Modifications[2]) |> modPositionsToDF() |> dfToModPositions()
#' modPositions(OVATable("peptide")$Modifications[1])
#' modPositions(OVATable("peptide")$Modifications[1]) |> modPositionsToDF()
#' modPositions(OVATable("peptide")$Modifications[1]) |> modPositionsToDF() |> dfToModPositions()
dfToModPositions <- function(df, splitChar = ",",  returnEmptyRow = TRUE){
  if ((nrow(df) < 1) & returnEmptyRow){
    return(data.frame(modifications = as.character(),
                      positions = as.character(),
                      letters = as.character(),
                      numbers = as.character()))
  }
  if ((nchar(df$modifications) == 0) & !returnEmptyRow){
    return(data.frame(modifications = "",
                      positions = "",
                      letters = "",
                      numbers = ""))
  }
  return(data.frame(modifications = strsplit(df$modifications, split = splitChar)[[1]],
                    positions = strsplit(df$positions, split = splitChar)[[1]],
                    letters = strsplit(df$letters, split = splitChar)[[1]],
                    numbers = strsplit(df$numbers, split = splitChar)[[1]]))
}

#' @title getModPositionInProtein
#'
#' @description takes a row of a data.frame with peptide information (like a peptide table from Proteome Discoverer) with added modification info
#'  (like coming from \code{\link{modPositionsToDF}}) and gives the position of a specified modification (if present)
#'
#' @param peptideTableRow single row of a data.frame with a 'position column' and 'translated' modification (see examples)
#' @param Accession character vector that specifies from which protein Accession to get the position
#' @param exact logical vector which defines how the Accession argument is compared to the string argument.
#'  Default is TRUE which means that the Accession needs to be exactly the same letters/numbers. If FALSE then
#'  a 'grepl' statement is used with the Accession as a pattern
#' @param whichModification character vector that specifies for which modification the protein position is calculated (if present, name needs to be exact)
#' @param positionColumn character vector which specifies which column contains the peptide position information. Proteome Discoverer uses either
#'  'PositionsinProteins' or 'PositionsinMasterProteins' (depending on the settings used in the consensus method)
#' @param clearPosition vector (default is 1L) that is used for the clear column when the position of the modification in the peptide is unambiguous (certain)
#' @param unclearPosition vector (default is 2L) that is used when a modification position is ambiguous (uncertain)
#'
#' @return data.frame with two columns: 'position' (position of modification in protein) and 'clear' which details if a modification position is ambiguous
#' @export
#'
#' @examples
#' OVATable("peptide")$Modifications
#' # generate data.frame of all these modifications
#' theModifications <- purrr::map_df(OVATable("peptide")$Modifications, ~modPositions(.x,
#'  returnEmptyRow = FALSE) |> modPositionsToDF(returnEmptyRow = FALSE))
#' theModifications |> head()
#' # bind together the modifications woth the original table
#' newTable <- dplyr::bind_cols(OVATable("peptide"), theModifications)
#' newTable |> head()
#'
#' newTable[15,]
#' getModPositionInProtein(peptideTableRow = newTable[15,], Accession = "P01012",
#'  whichModification = "Oxidation", positionColumn = "PositionsinProteins")
#' newTable[32,]
#' getModPositionInProtein(peptideTableRow = newTable[32,], Accession = "P01012",
#'  whichModification = "Phospho", positionColumn = "PositionsinProteins")
#' getModPositionInProtein(peptideTableRow = newTable[32,], Accession = "P01012",
#'  whichModification = "Carbamidomethyl", positionColumn = "PositionsinProteins")
#' getModPositionInProtein(peptideTableRow = newTable[32,], Accession = "P01012",
#'  whichModification = "Carbamido", positionColumn = "PositionsinProteins")
#' newTable[1,]
#' getModPositionInProtein(peptideTableRow = newTable[1,], Accession = "P01012",
#'  whichModification = "Carbamidomethyl", positionColumn = "PositionsinProteins")
#' newTable[26,]
#' # # of possible positions for the phospho modification
#' stringr::str_count(newTable$Sequence[26], pattern = "S|T")
#' getModPositionInProtein(peptideTableRow = newTable[26,], Accession = "P01012",
#'  whichModification = "Phospho", positionColumn = "PositionsinProteins")
#' getModPositionInProtein(peptideTableRow = newTable[26,], Accession = "P01012",
#'  whichModification = "Carbamidomethyl", positionColumn = "PositionsinProteins")
#' # to get all Phospho positions in the protein
#' purrr::map_df(1:nrow(newTable), ~getModPositionInProtein(newTable[.x,], Accession = "P01012",
#'  whichModification = "Phospho", positionColumn = "PositionsinProteins")) |>
#'  dplyr::distinct(position, clear) |> dplyr::arrange(position)
getModPositionInProtein <- function(peptideTableRow,
                                    Accession, exact = TRUE, whichModification, positionColumn,
                                    clearPosition = 1L, unclearPosition = 2L){ # unclearPosition = 2 for distinction, note: always make  bigger than clearPosition
  modsDF <- dfToModPositions(peptideTableRow)
  if (nrow(modsDF) == 0){
    return(data.frame(position = as.integer(), clear = as.integer()))
  } else {
    if (!whichModification %in% modsDF$modifications){
      return(data.frame(position = as.integer(), clear = as.integer()))
    }
  }
  peptideStart <- getPeptideStart(peptideTable = peptideTableRow,
                                  positionColumn = positionColumn,
                                  Accession = Accession, exact = exact)
  result <- as.integer()
  resultClear <- as.integer()
  for (counter in 1:nrow(modsDF)){
    if (modsDF$modifications[counter] == whichModification){
      theNumbers <- strsplit(modsDF$numbers[counter], split = ";")[[1]]
      theLetters <- strsplit(modsDF$letters[counter], split = ";")[[1]]
      for (counter2 in 1:length(theNumbers)){
        if (theNumbers[counter2] != 0){
          result <- append(result, peptideStart + as.integer(theNumbers[counter2]) - 1)
          resultClear <- append(resultClear, clearPosition)
        } else {
          result <- append(result, peptideStart + as.data.frame(stringr::str_locate_all(peptideTableRow$Sequence, pattern = theLetters[counter2])[[1]])$start - 1)
          resultClear <- append(resultClear, rep(unclearPosition, nrow(stringr::str_locate_all(peptideTableRow$Sequence, pattern = theLetters[counter2])[[1]])))
        }
      }
    }
  }
  return(data.frame(position = result, clear = resultClear) %>%
           dplyr::arrange(!!dplyr::sym("position")) %>%
           dplyr::group_by(!!dplyr::sym("position")) %>%
           dplyr::summarise(!!dplyr::sym("clear") := min(!!dplyr::sym("clear"))) %>%
           dplyr::ungroup() %>% as.data.frame()) # remove doubles: if position is both unclear and clear it should be clear
}

#' @title getAllModPositionInProtein
#'
#' @description takes a data.frame with peptide information (like a peptide table from Proteome Discoverer) and gives the position(S) of
#'  a specified modification(s) (if present)
#'
#' @param peptideTable a data.frame with columns 'Sequence', 'Modifications' and a position column (see 'positionColumn' argument). This is what
#'  Proteome Discoverer produces
#' @param Accession character vector that specifies from which protein Accession to get the position
#' @param exact logical vector which defines how the Accession argument is compared to the string argument.
#'  Default is TRUE which means that the Accession needs to be exactly the same letters/numbers. If FALSE then
#'  a 'grepl' statement is used with the Accession as a pattern
#' @param whichModification character vector that specifies for which modification the protein position is calculated (if present, name needs to be exact)
#' @param positionColumn character vector which specifies which column contains the peptide position information. Proteome Discoverer uses either
#'  'PositionsinProteins' or 'PositionsinMasterProteins' (depending on the settings used in the consensus method)
#' @param clearPosition vector (default is 1L) that is used for the clear column when the position of the modification in the peptide is unambiguous (certain)
#' @param unclearPosition vector (default is 2L) that is used when a modification position is ambiguous (uncertain)
#' @param sumPositions logical vector which (if TRUE) causes the position information to be somewhat simplified: the clear-fields of all identical positions
#'  are summed. As an example, this causes a position which is present twice (clear and unclear) to become a single row with the clear field (clearPosition + unclearPosition)
#'
#' @return a named list of data.frame's that contain the position information
#' @export
#'
#' @examples
#' getAllModPositionInProtein(peptideTable = OVATable("peptide"), Accession = "P01012",
#'  whichModification = c("Carbamidomethyl","Phospho"), positionColumn = "PositionsinProteins")
#' getAllModPositionInProtein(peptideTable = OVATable("peptide"), Accession = "P01012",
#'  whichModification = c("Carbamidomethyl","Phospho"), positionColumn = "PositionsinProteins",
#'  sumPositions = TRUE)
#' getAllModPositionInProtein(peptideTable = OVATable("peptide"), Accession = "P01012",
#'  whichModification = c("Oxidation"), positionColumn = "PositionsinProteins")
#' getAllModPositionInProtein(peptideTable = OVATable("peptide"), Accession = "P01012",
#'  whichModification = c("Oxidation"), positionColumn = "PositionsinProteins", sumPositions = TRUE)
#' getAllModPositionInProtein(peptideTable = OVATable("peptide"), Accession = "P01012",
#'  whichModification = c("Something"), positionColumn = "PositionsinProteins")
getAllModPositionInProtein <- function(peptideTable,
                                       Accession, exact = TRUE, whichModification, positionColumn,
                                       clearPosition = 1L, unclearPosition = 2L, sumPositions = FALSE){
  # get only potentially relevant rows
  peptideTable <- peptideTable %>% dplyr::select(dplyr::all_of(c("Sequence", "Modifications")), dplyr::all_of(positionColumn))
  newTable <- data.frame()
  for (counter in 1:length(whichModification)){
    newTable <- dplyr::bind_rows(newTable, peptideTable %>%
                                   dplyr::filter(grepl(!!dplyr::sym("Modifications"), pattern = whichModification[counter])))
  }
  result <- list()
  if (nrow(newTable) < 1){
    for (counter in 1:length(whichModification)){
      result[[counter]] <- data.frame(position = as.numeric(), clear = as.numeric())
    }
    names(result) <- whichModification
    return(result)
  }
  theModifications <- purrr::map_df(newTable$Modifications, ~modPositions(.x, returnEmptyRow = FALSE) |> modPositionsToDF(returnEmptyRow = FALSE))
  newTable <- dplyr::bind_cols(newTable, theModifications)
  for (counter in 1:length(whichModification)){
    result[[counter]] <- purrr::map_df(1:nrow(newTable),
                                       ~getModPositionInProtein(newTable[.x,],
                                                                Accession = Accession,
                                                                whichModification = whichModification[counter],
                                                                exact = exact, positionColumn = positionColumn,
                                                                clearPosition = clearPosition, unclearPosition = unclearPosition)) |>
      dplyr::distinct(!!dplyr::sym("position"), !!dplyr::sym("clear")) |>  dplyr::arrange(!!dplyr::sym("position"))
  }
  if (sumPositions){
    result <- purrr::map(result, ~.x |> dplyr::group_by(!!dplyr::sym("position")) |>
                           dplyr::summarise(!!dplyr::sym("clear") := sum(!!dplyr::sym("clear"), na.rm = TRUE)) |> dplyr::ungroup() |> as.data.frame())
  }
  names(result) <- whichModification
  return(result)
}

#' @title whichModifications
#'
#' @description Helper function to list modifications in a character vector of format modification,splitCahracter,modification,splitCahracter, etc etc
#'
#' @param modification character vector of format <modification> <splitCahracter> <modification> <splitCahracter> etc etc
#' @param splitCharacter character vector used to split the modification argument
#' @param trim logical vector. Default is TRUE: the result will be trimmed (spaces on both sides removed). When FALSE, this is not done
#'
#' @return character vector
#' @export
#'
#' @examples
#' whichModifications('Oxidation,Phospho')
#' whichModifications('Oxidation , Phospho')
#' whichModifications('Oxidation , Phospho', trim = FALSE)
#' whichModifications('Oxidation , Phospho, Carbamidomethyl')
#' whichModifications(c('Oxidation , Phospho','Carbamidomethyl, Phospho'))
whichModifications <- function(modification, splitCharacter = ",", trim = TRUE){
  if (length(modification)==1){
    if (nchar(modification)>0){
      return(
        ifelseProper(trim,
                     stringr::str_trim(strsplit(modification, split = splitCharacter)[[1]], side = "both"),
                     strsplit(modification, split = splitCharacter)[[1]])
      )
    } else {
      return(NA)
    }
  } else {
    if (length(modification>1)){
      return(unique(unlist(lapply(modification, function(x){whichModifications(x, splitCharacter = splitCharacter, trim = trim)}))))
    } else {
      return(NA)
    }
  }
}

#' @title containsModification
#'
#' @description Helper function to check whether a character vector of format modification,splitCahracter,modification,splitCahracter, (etc etc) contains
#'  a certain modification
#'
#' @param modifications character vector of format <modification> <splitCahracter> <modification> <splitCahracter> etc etc
#' @param whichModification the modification to search for (character vector)
#' @param strict logical vector, default is TRUE: the search for the modification is strict. If FALSE then the search goes via the 'grepl' function
#'  and regular expressions can be used
#' @param splitCharacter character vector used to split the modifications argument
#' @param trim logical vector. Default is TRUE: the modifications will be trimmed (spaces on both sides removed). When FALSE, this is not done
#'
#' @return logical vector
#' @export
#'
#' @examples
#' containsModification(c('Oxidation,Phospho','Carbamidomethyl,Phospho'),
#'  whichModification = "Phospho")
#' containsModification(c('Oxidation , Phospho','Carbamidomethyl, Phospho'),
#'  whichModification = "Phospho")
#' containsModification(c('Oxidation , Phospho','Carbamidomethyl, Phospho'),
#'  whichModification = "Phospho", trim = TRUE)
#' containsModification(c('Oxidation , Phospho','Carbamidomethyl, Phospho'),
#'  whichModification = "Phospho", strict = FALSE)
containsModification <- function(modifications, whichModification, strict = TRUE, splitCharacter = ",", trim = TRUE){
  if (!strict){
    return(grepl(modifications, pattern = whichModification))
  } else {
    result <- purrr::map(modifications, ~ifelseProper(trim,
                                                      stringr::str_trim(strsplit(.x, split = ",")[[1]], side = "both"),
                                                      strsplit(.x, split = ",")[[1]]))
    return(purrr::map_lgl(result, ~length(which(.x == whichModification)) > 0))
  }
}
