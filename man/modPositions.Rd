% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modifications.R
\name{modPositions}
\alias{modPositions}
\title{modPositions}
\usage{
modPositions(modifications, collapseChar = ";", returnEmptyRow = TRUE)
}
\arguments{
\item{modifications}{character vector representing one or more modifications. The format is 'number × modification name [position<letter(+number)>]'.
If more than one modification is present, the character vector is 'combined' with separation character ';' (collapseChar)}

\item{collapseChar}{character vector separating the modifications in the argument 'modifications'}

\item{returnEmptyRow}{logical vector, default is TRUE. TRUE: if no modification is present in the 'modifications' argument, a data.frame will
be returned without rows. FALSE: in that case a data.frame with a single row, with in every column an empty character vector, will be returned.}
}
\value{
a data.frame with columns: modifications, positions, letters and numbers
}
\description{
helper function which 'translates' a character vector representing a (peptide) modification to a data.frame
}
\note{
if there are two or more modification present, then number of rows in the returned data.frame will also be two or more

the modification format is what Proteome Discoverer/Sequest works with in its peptide tables
}
\examples{
OVATable("peptide")$Modifications[26]
modPositions(OVATable("peptide")$Modifications[26])
OVATable("peptide")$Modifications[2]
modPositions(OVATable("peptide")$Modifications[2])
OVATable("peptide")$Modifications[1]
modPositions(OVATable("peptide")$Modifications[1])
modPositions(OVATable("peptide")$Modifications[1], returnEmptyRow = FALSE)
purrr::map_df(OVATable("peptide")$Modifications, ~modPositions(.x))
}
