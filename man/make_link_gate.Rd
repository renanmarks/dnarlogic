% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GATE_LIB.R
\name{make_link_gate}
\alias{make_link_gate}
\title{Creates a "link gate", i.e., an object to hold the necessary metadata of linked species.}
\usage{
make_link_gate(sourceSignal, destinySignal, negated = FALSE)
}
\arguments{
\item{sourceSignal}{The source species to be linked.}

\item{destinySignal}{The destiny species that will be linked to source species.}

\item{negated}{If the link have a NOT gate between the source and destiny}
}
\value{
An object holding the necessary metadata of linked species.
}
\description{
Creates a "link gate", i.e., an object to hold the necessary metadata of linked species.
}
