% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cellosaurus_annotations.R
\name{annotateCellAccession}
\alias{annotateCellAccession}
\title{Annotate Cell Accession}
\usage{
annotateCellAccession(
  accessions,
  to = c("id", "ac", "hi", "sy", "ca", "sx", "ag", "di", "derived-from-site",
    "misspelling", "dt")
)
}
\arguments{
\item{accessions}{The Cellosaurus accession to annotate.}

\item{to}{A character vector specifying the types of annotations to retrieve. Possible values include "id", "ac", "hi", "sy", "ca", "sx", "ag", "di", "derived-from-site", "misspelling", and "dt".}
}
\value{
A data frame containing the annotations for the cell line.
}
\description{
This function takes a Cellosaurus accession and returns annotations for the cell line.
}
\examples{
annotateCellAccession("CVCL_0031")
annotateCellAccession("CVCL_0031", to = c("id", "ac", "hi", "sy"))

}
