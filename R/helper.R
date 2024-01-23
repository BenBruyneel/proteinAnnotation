
# ---- data for examples/tests ----

#' @title standardProtein
#' @description Helper function for examples & testing. Generate a character vector of the amino acid sequence
#'  of one of the (2) standard proteins.
#'
#' @param short shortened name of the protein to retrieve. Only 'bsa' and 'ova' (or their upper case equivalents) are
#'  valid inputs. Other input will return NA
#'
#' @return an amino acid sequence in the form of a character vector or NA
#' @export
#'
#' @note The data is coming from \href{https://www.uniprot.org/uniprotkb/}{UniProt}
#'
#' OVA is short for \href{https://www.uniprot.org/uniprotkb/P01012/entry}{P01012 · OVAL_CHICK} Ovalbumin (Gallus gallus)
#'
#' BSA is short for \href{https://www.uniprot.org/uniprotkb/P01012/entry}{P02769 · ALBU_BOVIN} Bovine Serum Albumin (Bos taurus)
#'
#' @examples
#' standardProtein()
#' standardProtein("bsa")
#' standardProtein("Insulin")
standardProtein <- function(short = "OVA"){
  return(switch(toupper(short),
                "BSA" = "MKWVTFISLLLLFSSAYSRGVFRRDTHKSEIAHRFKDLGEEHFKGLVLIAFSQYLQQCPFDEHVKLVNELTEFAKTCVADESHAGCEKSLHTLFGDELCKVASLRETYGDMADCCEKQEPERNECFLSHKDDSPDLPKLKPDPNTLCDEFKADEKKFWGKYLYEIARRHPYFYAPELLYYANKYNGVFQECCQAEDKGACLLPKIETMREKVLASSARQRLRCASIQKFGERALKAWSVARLSQKFPKAEFVEVTKLVTDLTKVHKECCHGDLLECADDRADLAKYICDNQDTISSKLKECCDKPLLEKSHCIAEVEKDAIPENLPPLTADFAEDKDVCKNYQEAKDAFLGSFLYEYSRRHPEYAVSVLLRLAKEYEATLEECCAKDDPHACYSTVFDKLKHLVDEPQNLIKQNCDQFEKLGEYGFQNALIVRYTRKVPQVSTPTLVEVSRSLGKVGTRCCTKPESERMPCTEDYLSLILNRLCVLHEKTPVSEKVTKCCTESLVNRRPCFSALTPDETYVPKAFDEKLFTFHADICTLPDTEKQIKKQTALVELLKHKPKATEEQLKTVMENFVAFVDKCCAADDKEACFAVEGPKLVVSTQTALA",
                "OVA" = "MGSIGAASMEFCFDVFKELKVHHANENIFYCPIAIMSALAMVYLGAKDSTRTQINKVVRFDKLPGFGDSIEAQCGTSVNVHSSLRDILNQITKPNDVYSFSLASRLYAEERYPILPEYLQCVKELYRGGLEPINFQTAADQARELINSWVESQTNGIIRNVLQPSSVDSQTAMVLVNAIVFKGLWEKAFKDEDTQAMPFRVTEQESKPVQMMYQIGLFRVASMASEKMKILELPFASGTMSMLVLLPDEVSGLEQLESIINFEKLTEWTSSNVMEERKIKVYLPRMKMEEKYNLTSVLMAMGITDVFSSSANLSGISSAESLKISQAVHAAHAEINEAGREVVGSAEAGVDAASVSEEFRADHPFLFCIKHIATNAVLFFGRCVSP",
                NA))
}

#' @title OVATable
#' @description Helper function for examples & testing. Generates a data.frame, of protein or peptide information (both Ovalbumin), which is similar to
#'  what comes out of Proteome Discoverer
#'
#' @param whichone character vector which specifies what to get: 'protein' for a protein table, 'peptide' for a peptide table
#'
#' @return data.frame or NA
#' @export
#'
#' @examples
#' OVATable("protein")
#' OVATable("Peptide")
#' OVATable("Info")
OVATable <- function(whichone = "protein"){
  if (toupper(whichone) == "PROTEIN"){
    return(data.frame(Sequence = substr(standardProtein("OVA"), 2, nchar(standardProtein("OVA"))),
                      Accession = "P01012",
                      SequenceCoverage = "385;0-380"))
  } else {
    if (toupper(whichone) == "PEPTIDE"){
      return(
        data.frame(Sequence = c('ISQAVHAAHAEINEAGR','ADHPFLFCIK','AFKDEDTQAMPFR','LTEWTSSNVMEER','GGLEPINFQTAADQAR','YPILPEYLQCVK','DEDTQAMPFR','QIGLFR','ELINSWVESQTNGIIR','VTEQESKPVQMMY','VTEQESKPVQM','ADHPFLF','VTEQESKPVQMMYQIGLFR','MYQIGLFR','AFKDEDTQAMPFR','HIATNAVLFFGR','VLVNAIVFK','YQIGLFR','EVVGSAEAGVDAASVSEEFR','DILNQITKPNDVYSFSLASR','VTEQESKPVQM','VTEQESKPVQMMYQIGLFR','YPILPEYLQCVK','ADHPFLFCIK','YPILPEYLQC','FDKLPGFGDSIEAQCGTSVNVHSSLR','LYAEERYPILPEYLQCVK','SFSLASR','EVVGSAEAGVD','HIATNAVLF','AMGITDVFSSSANLSGISSAESLK','LPGFGDSIEAQCGTSVNVHSSLR','MYQIGLFR','VTEQESKPVQMMYQ','AMVYLGAK','EVVGSAEAGVD'),
                   Modifications = c('',paste(c('1',DescTools::AscToChar(c(195, 151)),'Carbamidomethyl [C8]'), collapse = ""),
                                     '','','',
                                     paste(c('1',DescTools::AscToChar(c(195, 151)),'Carbamidomethyl [C10]'), collapse = ""),
                                     '','','','','','','','',
                                     paste(c('1',DescTools::AscToChar(c(195, 151)),'Oxidation [M10]'), collapse = ""),
                                     '','','',
                                     paste(c('1',DescTools::AscToChar(c(195, 151)),'Phospho [S5]'), collapse =""),
                                     '',
                                     paste(c('1',DescTools::AscToChar(c(195, 151)),'Oxidation [M11]'), collapse = ""),
                                     paste(c('1',DescTools::AscToChar(c(195, 151)),'Oxidation [M]'), collapse = ""),
                                     '','',
                                     paste(c('1',DescTools::AscToChar(c(195, 151)),'Carbamidomethyl [C10]'), collapse = ""),
                                     paste(c('1',DescTools::AscToChar(c(195, 151)),'Carbamidomethyl [C15]; 1',DescTools::AscToChar(c(195, 151)),'Phospho [S/T]'), collapse = ""),
                                     paste(c('1',DescTools::AscToChar(c(195, 151)),'Carbamidomethyl [C16]'), collapse = ""),
                                     '',
                                     paste(c('1',DescTools::AscToChar(c(195, 151)),'Phospho [S5]'), collapse = ""),
                                     '','',
                                     paste(c('1',DescTools::AscToChar(c(195, 151)),'Carbamidomethyl [C12]; 1',DescTools::AscToChar(c(195, 151)),'Phospho [S7]'), collapse = ""),
                                     paste(c('1',DescTools::AscToChar(c(195, 151)),'Oxidation [M1]'), collapse = ""),
                                     '','',''),
                   PositionsinProteins = c('P00000 [323-339]; P01012 [323-339]','P00000 [360-369]; P01012 [360-369]','P00000 [187-199]; P01012 [187-199]','P00000 [264-276]; P01012 [264-276]','P00000 [127-142]; P01012 [127-142]','P00000 [111-122]; P01012 [111-122]','P00000 [190-199]; P01012 [190-199]','P00000 [213-218]; P01012 [213-218]','P00000 [143-158]; P01012 [143-158]','P00000 [200-212]; P01012 [200-212]','P00000 [200-210]; P01012 [200-210]','P00000 [360-366]; P01012 [360-366]','P00000 [200-218]; P01012 [200-218]','P00000 [211-218]; P01012 [211-218]','P00000 [187-199]; P01012 [187-199]','P00000 [370-381]; P01012 [370-381]','P00000 [173-181]; P01012 [173-181]','P00000 [212-218]; P01012 [212-218]','P00000 [340-359]; P01012 [340-359]','P00000 [85-104]; P01012 [85-104]  ','P00000 [200-210]; P01012 [200-210]','P00000 [200-218]; P01012 [200-218]','P00000 [111-122]; P01012 [111-122]','P00000 [360-369]; P01012 [360-369]','P00000 [111-120]; P01012 [111-120]','P00000 [59-84]; P01012 [59-84]    ','P00000 [105-122]; P01012 [105-122]','P00000 [98-104]; P01012 [98-104]  ','P00000 [340-350]; P01012 [340-350]','P00000 [370-378]; P01012 [370-378]','P00000 [299-322]; P01012 [299-322]','P00000 [62-84]; P01012 [62-84]    ','P00000 [211-218]; P01012 [211-218]','P00000 [200-213]; P01012 [200-213]','P00000 [39-46]; P01012 [39-46]    ','P00000 [340-350]; P01012 [340-350]'),
                   XCorrbySearchEngine_1 = c( 7.746809, 4.090933, 5.751740, 5.085020, 5.391547, 5.270951, 3.744389, 2.335548, 5.883391, 5.825895, 4.928473, 1.917278, 7.971527, 2.800339, 6.016358, 4.747321, 3.350193, 2.306348, 8.966028, 7.224921, 4.816049, 7.337126, 4.534242, 3.898762, 3.108466,10.714921, 6.474907, 1.964828, 3.640042, 2.952908, 7.617251,10.983087, 2.979085, 6.427321, 2.055961, 3.565870),
                   Abundances_1 = c(6922540038,6459800097,5561482511,4562266198,4475159720,2387118310,2089608792,1934193152,1693816042,1264935871,1240225416,1237818112,1026623301, 912975056, 762172032, 754950638, 703813174, 612413070, 599291661, 541217897, 139383770, 114833303, 109806210,  93821290,  86888424,  78966057,  72976459,  68826544,  67498509,  63774436,  62323403,  56105704,  52146989,  50184580,  45376732,  45354477),
                   Abundances_2 = c( 7817180355,11641298738, 8982076379, 5516495806, 5717628078, 5403322654, 2482494501, 2139503360, 1987764787, 1567878070, 2098952836, 1581101056, 1442462592, 1075726278, 1245223959, 1896183039, 1298861009,  562293592,  847669384, 2848052574,   63512062,  151375160,  172898708,   84245118,  100213140,  116759111,  112749437,    7627628,   98103685,  133451876,  109367824,  216828141,   61159512,   56786550,   60897808,   34962933)))
    } else {
      return(NA)
    }
  }
}

# ---- basic tools ----

#' @title strMultiReplaceAll
#' @description helper function that replaces multiple patterns in a character vector, essentially an extension
#'  of stringr::str_replace_all
#'
#' @param strings character vector (can be longer than 1) in which all the patterns (arguments) are to be replaced
#' @param patterns character vector of patterns to be replaced
#' @param replacements character vector of replacements for the patterns in the strings argument (if found). Should be
#'  either length = 1 or same length as the replacements. Default is "" (empty character vector), which means that
#'  all patterns in strings will be removed
#'
#' @return character vector
#' @note internal function
#'
# ---- examples internal function ----
# @examples
# strMultiReplaceAll("Hello World", patterns = c("e","o"))
# strMultiReplaceAll("Hello World", patterns = c("e","o"), replacements = c("ee","oo"))
# paste(strMultiReplaceAll(paste(LETTERS, collapse = ""),
#  patterns = LETTERS[5:10], replacements = letters[5:10]), collapse = "")
strMultiReplaceAll <- function(strings, patterns, replacements = ""){
  if (length(replacements) == 1){
    replacements <- rep(replacements, length(patterns))
  }
  if (length(patterns) != length(replacements)){
    stop("Error: length of patterns and replacements should the same (unless replacements length is 1)")
  }
  for (stringCounter in 1:length(strings)){
    for (counter in 1:length(patterns)){
      strings[stringCounter] <- stringr::str_replace_all(strings[stringCounter], pattern = patterns[counter], replacement = replacements[counter])
    }
  }
  return(strings)
}

#' @title is.Class
#' @description helper function to determine if object == whichClass or a descendant
#'  of whichClass.
#'
#' @param object a data object of some class
#' @param whichClass character string: class name to be tested
#' @return TRUE or FALSE
#' @note internal function
is.Class <- function(object, whichClass){
  return(whichClass %in% class(object))
}

#' ifelse replacement for properly returning all datatypes.
#'
#' @param logicValue variable or expression resulting in TRUE or FALSE,
#'  if missing or not logical then the function will return NULL.
#' @param ifTrue variable or expression to be returned when logicValue == TRUE
#' @param ifFalse variable or expression to be returned when logicValue == FALSE
#'
#' @returns depending on logicValue, ifTrue ir ifFalse.
#' @note not vectorized
#' @note internal function
ifelseProper <- function(logicValue = NULL, ifTrue = NULL, ifFalse = NULL){
  if (missing(logicValue)){
    return(NULL)
  } else {
    if (!is.logical(logicValue)){
      return(NULL)
    } else {
      if (logicValue){
        return(ifTrue)
      } else {
        return(ifFalse)
      }
    }
  }
}

# ---- matrix dimension functions ----

#' @title rowAndColumns
#' @description Calculates number of columns and rows of a matrix from number of cells and the ratio number of columns/ number of rows
#'  The number of cells is a defined as a minimum number of cells. Either the number of rows or the number of columns is
#'  increased to accommodate this. The column/row ratio and number of cells for the 'solution' might therefore not be exact.
#'  The reason for this is that the intended target for this function is to come up with a cell matrix big enough for a
#'  data of a certain size (vectorLength)
#'
#' @param vectorLength minimum number of cells for the resulting columns*rows matrix
#' @param ratio the intended ratio the resulting columns*rows matrix. Increase ratio to get more columns, decrease to get more rows.
#'  Note: ratio = number of columns divided by the number of rows.
#' @param increaseColumns if TRUE, then the number of columns will be increased to reach the number of matrix cells required.
#'  If FALSE then the number of rows will be increased to do this. Default is FALSE
#' @param minimumRatio due to the iterative nature of part of the function, this puts a minimum limit on the possible ratios
#'  to prevent the function from 'getting stuck'. Default is 0.01
#' @param maximumRatio due to the iterative nature of part of the function, this puts a maximum limit on the possible ratios
#'  to prevent the function from 'getting stuck'. Default is 100
#'
#' @return a named numeric vector (names: columns, rows)
#' @export
#'
#' @examples
#' rowsAndcolumns(385, 5)
#' rowsAndcolumns(385, 0.2)
#' rowsAndcolumns(400, 10)
#' rowsAndcolumns(400, 0.1)
rowsAndcolumns <- function(vectorLength, ratio, increaseColumns = FALSE, minimumRatio = 0.01, maximumRatio = 100) {
  # Check if the ratio is not zero, not negative
  if ((ratio < minimumRatio) | (ratio > maximumRatio)){
    stop(paste(c("Ratio must be between ", minimumRatio, " and ", maximumRatio), collapse = ""))
  }
  # Calculate b using the formula
  rows <- round(sqrt(vectorLength / abs(ratio)))
  # Calculate a using the ratio
  columns <- round(ratio * rows)
  # increase either rows or columns to get to get to minimum vectorLength
  # do minimum increase to get to or above vectorLength
  while ((rows*columns) < vectorLength){
    columns <- columns + (increaseColumns)
    rows <- rows + (!increaseColumns)
  }
  # Return the result as a numeric vector
  result <- c(columns = columns, rows = rows)
  return(result)
}



#' @title boxDimensions
#' @description Calculates/defines the dimensions of the 'matrix' to use to display data of a certain length (e.g. a protein sequence)
#'
#' @param vectorLength minimum number of cells for the resulting columns*rows matrix
#' @param ratio the intended ratio the resulting columns*rows matrix. Increase ratio to get more columns, decrease to get more rows.
#' @param increaseColumns if TRUE, then the number of columns will be increased to reach the number of matrix cells required.
#'  If FALSE then the number of rows will be increased to do this. Default is FALSE
#' @param ncol defines the number of columns to use. If not also defined, the number of rows is adapted to fit all of the required number of cells for the matrix
#' @param nrow defines the number of rows to use.  If not also defined, the number of columns is adapted to fit all of the required number of cells for the matrix
#' @param noWarning logical vector, if product of the number of rows and the number of columns is too small for the number of matrix cells (vector length) required
#'  then a warning will displayed (default noWarning is FALSE), unless noWarning is TRUE
#'
#' @note Order of importance for the ratio, ncol and nrow arguments: if the ratio is NA (default), then the ncol argument
#'  is used, if possible with the nrow argument. If ncol is NA, then the nrow argument is used.
#'
#' @return a named numeric vector (names: ncol, nrow)
#' @export
#'
#' @examples
#' boxDimensions(385, 5)
#' boxDimensions(385, nrow = 9)
#' boxDimensions(400, 10)
#' boxDimensions(400, ncol = 60)
boxDimensions <- function(vectorLength = 100,
                          ratio = NA, increaseColumns = FALSE,
                          ncol = NA, nrow = NA, noWarning = FALSE){
  if (!is.na(ratio)){
    result <- rowsAndcolumns(vectorLength = vectorLength, ratio = ratio,
                             increaseColumns = increaseColumns)
    names(result) <- c("ncol", "nrow")
    return(result)
  } else {
    if (is.na(ncol)){
      if (is.na(nrow)){
        return(c(ncol = vectorLength, nrow = 1))
      } else {
        return(c(ncol = ceiling(vectorLength/nrow), nrow = nrow))
      }
    } else {
      if (is.na(nrow)){
        return(c(ncol = ncol, nrow = ceiling(vectorLength/ncol)))
      } else {
        if (as.integer(ncol*nrow) < vectorLength){
          if (!noWarning){
            warning("Dimensions lead to smaller box then is needed for the defined length (number of matrix cells)")
          }
          return(c(ncol = ncol, nrow = nrow))
        } else {
          return(c(ncol = ncol, nrow = nrow))
        }
      }
    }
  }
}

#' @title boxData
#'
#' @description Takes a data.frame and adds a 'row' and 'col' column which together define which cell in the rows*columns-matrix a data.frame-row maps to
#'
#' @param data data.frame, should not already have a 'row' and/or 'col' column
#' @param ncols number of required columns for the matrix
#' @param adjustRows logical vector, default is TRUE. Defines whether the number of rows for the data.frame is allowed to be adjusted.
#'  Setting this to FALSE may lead to 'stop'-error if data doesn't fit the requested matrix.
#' @param emptySequence if data.frame is extended, this character vector is used to sequence in the new rows. The position column is simply
#'  filled with increasing integers.
#' @param replaceNA default is NA, if defined as something else, then ALL NA values in the data.frame will be replace with this argument.
#'  This is an admittedly crude way to replace NA values. If more finesse is needed, leave at default and handle NA values in followup functions
#'  such as 'replace_na' from the tidyr package
#'
#' @note If the data.frame extended: the position column and the sequence are filled with increasing integers (starting from the max position)
#'  and the 'emptySequence' argument respectively. Other columns in the extended using NA
#'
#' @return a data.frame with extra columns (row and col) and possible extra rows
#' @export
#'
#' @examples
#' boxData(createProteinData(standardProtein(), start = 2, nterm = 1), ncols = 50) |> head(20)
#' boxData(createProteinData(standardProtein(), start = 2, nterm = 1), ncols = 50) |> tail(20)
#' boxData(createProteinData(standardProtein(), start = 2, nterm = 1), ncols = 50)[40:60,]
boxData <- function(data, ncols = NA, adjustRows = TRUE, emptySequence = "", replaceNA = NA){
  if (!is.na(ncols)){
    if (nrow(data) %% ncols != 0){
      if (!adjustRows){
        stop("Requested number of rows does not fit data")
      } else {
        dimsNeeded <- boxDimensions(vectorLength = nrow(data), ncol = ncols)
        rowstoAdd <- (dimsNeeded["ncol"] * dimsNeeded["nrow"]) - nrow(data)
        rowsToAdd <- data.frame(position = max(data$position) + 1:rowstoAdd, sequence = rep(emptySequence, rowstoAdd))
        data <- dplyr::bind_rows(data, rowsToAdd)
        if (!is.na(replaceNA)){
          data[is.na(data)] <- replaceNA
        }
      }
    }
    data$row <- as.factor(1:ncols)
    data$col <- as.factor(rep((nrow(data) %/% ncols):1, each = ncols))
  }
  return(data)
}
