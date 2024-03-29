% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modifications.R
\name{getAllModPositionInProtein}
\alias{getAllModPositionInProtein}
\title{getAllModPositionInProtein}
\usage{
getAllModPositionInProtein(
  peptideTable,
  Accession,
  exact = TRUE,
  whichModification,
  positionColumn,
  clearPosition = 1L,
  unclearPosition = 2L,
  sumPositions = FALSE
)
}
\arguments{
\item{peptideTable}{a data.frame with columns 'Sequence', 'Modifications' and a position column (see 'positionColumn' argument). This is what
Proteome Discoverer produces}

\item{Accession}{character vector that specifies from which protein Accession to get the position}

\item{exact}{logical vector which defines how the Accession argument is compared to the string argument.
Default is TRUE which means that the Accession needs to be exactly the same letters/numbers. If FALSE then
a 'grepl' statement is used with the Accession as a pattern}

\item{whichModification}{character vector that specifies for which modification the protein position is calculated (if present, name needs to be exact)}

\item{positionColumn}{character vector which specifies which column contains the peptide position information. Proteome Discoverer uses either
'PositionsinProteins' or 'PositionsinMasterProteins' (depending on the settings used in the consensus method)}

\item{clearPosition}{vector (default is 1L) that is used for the clear column when the position of the modification in the peptide is unambiguous (certain)}

\item{unclearPosition}{vector (default is 2L) that is used when a modification position is ambiguous (uncertain)}

\item{sumPositions}{logical vector which (if TRUE) causes the position information to be somewhat simplified: the clear-fields of all identical positions
are summed. As an example, this causes a position which is present twice (clear and unclear) to become a single row with the clear field (clearPosition + unclearPosition)}
}
\value{
a named list of data.frame's that contain the position information
}
\description{
takes a data.frame with peptide information (like a peptide table from Proteome Discoverer) and gives the position(S) of
a specified modification(s) (if present)
}
\examples{
getAllModPositionInProtein(peptideTable = OVATable("peptide"), Accession = "P01012",
 whichModification = c("Carbamidomethyl","Phospho"), positionColumn = "PositionsinProteins")
getAllModPositionInProtein(peptideTable = OVATable("peptide"), Accession = "P01012",
 whichModification = c("Carbamidomethyl","Phospho"), positionColumn = "PositionsinProteins",
 sumPositions = TRUE)
getAllModPositionInProtein(peptideTable = OVATable("peptide"), Accession = "P01012",
 whichModification = c("Oxidation"), positionColumn = "PositionsinProteins")
getAllModPositionInProtein(peptideTable = OVATable("peptide"), Accession = "P01012",
 whichModification = c("Oxidation"), positionColumn = "PositionsinProteins", sumPositions = TRUE)
getAllModPositionInProtein(peptideTable = OVATable("peptide"), Accession = "P01012",
 whichModification = c("Something"), positionColumn = "PositionsinProteins")
}
