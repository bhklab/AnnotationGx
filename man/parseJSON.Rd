% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getPubChem.R
\name{parseJSON}
\alias{parseJSON}
\title{Parse a JSON into a list}
\usage{
parseJSON(response, ..., encoding = "UTF-8", query_only = FALSE)
}
\arguments{
\item{response}{A \code{response} object as returned by \code{httr::GET}}

\item{...}{Additional arguments to the \code{httr::content} function.}

\item{as}{A \code{character} vector indicating the return type. Options are 'raw',}
}
\description{
Parse a JSON into a list
}
