% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GATE_LIB.R
\name{make_or_gate}
\alias{make_or_gate}
\title{Create an OR gate with initial input values.}
\usage{
make_or_gate(name, input1value, input2value)
}
\arguments{
\item{name}{The logic gate name}

\item{input1value}{The initial value of input1.}

\item{input2value}{The initial value of input2.}
}
\value{
An OR gate with its name, species, specific CRN reactions, ki and ci constants.
Z_0 + Z1 -> Z0'
}
\description{
Create an OR gate with initial input values.
}
