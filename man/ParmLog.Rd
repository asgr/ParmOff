\name{ParmLog}
\alias{ParmLog}
\alias{ParmUnLog}
\title{
Log or unlog selected elements of a list
}
\description{
Helper functions to apply a log or inverse-log transformation to selected
elements of a list. Elements are selected either by a logical vector or by a
character vector of names (the same interface used throughout the
\code{\link{ParmOff}} family). The structure of each element (matrix, array,
vector, etc.) is preserved in the output.
}
\usage{
ParmLog(x, logged, log_type = 'log10', verbose = FALSE)
ParmUnLog(x, logged, log_type = 'log10', verbose = FALSE)
}
\arguments{
  \item{x}{
List whose elements are to be (un)logged.
}
  \item{logged}{
Logical vector (same length as \code{x}) or character vector of element
names. For a logical vector, elements corresponding to \code{TRUE} are
transformed. For a character vector, elements whose names appear in
\code{logged} are transformed.
}
  \item{log_type}{
Character scalar. Use \code{'log10'} (default) for base-10 transformations
(\code{\link{log10}} / \code{10^x}), \code{'ln'} for natural-log transformations (\code{\link{log}} / \code{\link{exp}}) or \code{'log2'} for base-2 transformations (\code{\link{log2}} / \code{2^x}).
}
  \item{verbose}{
Logical; if TRUE, prints a message for each element that is transformed,
showing its name and before/after values. Output is only printed for elements
that are simple scalars, vectors of length \eqn{\le 20}, or matrices of size
\eqn{\le 10 \times 10}. Default is FALSE.
}
}
\details{
\code{ParmLog} applies the forward log transformation (\code{log10} or
\code{log}) to the selected elements. \code{ParmUnLog} applies the inverse
(\code{10^x}, \code{exp}, \code{2^x}).

Internally, \code{\link{lapply}} is used so that the list structure is never
simplified. R's vectorised arithmetic (\code{log10}, \code{exp}, \code{log2})
preserves the dimensions of matrices and arrays, so those structures are
returned unchanged in shape.

\code{ParmUnLog} is used internally by \code{\link{ParmOff}} to back-transform
arguments listed in \code{.logged}. When \code{\link{ParmOff}} is called with
\code{.verbose = TRUE}, that flag is forwarded to \code{ParmUnLog} so that
de-logging transformations are reported automatically.
}
\value{
A copy of \code{x} with the selected elements transformed.
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{ParmOff}}, \code{\link{ParmLimLo}}, \code{\link{log10}},
\code{\link{log}}, \code{\link{exp}}
}
\examples{
# --- character-vector selection ---
params <- list(a = 100, b = 10, c = 5)

# Log two elements (base 10)
ParmLog(params, logged = c("a", "b"))
# a = log10(100) = 2, b = log10(10) = 1, c unchanged

# Unlog them back
ParmUnLog(ParmLog(params, logged = c("a", "b")), logged = c("a", "b"))

# --- logical-vector selection ---
flags <- c(TRUE, TRUE, FALSE)
ParmLog(params, logged = flags)
ParmUnLog(params, logged = flags, log_type = 'ln')  # natural-log inverse

# --- verbose output for debugging ---
# Print the name and before/after values of each transformed element
ParmLog(params, logged = c("a", "b"), verbose = TRUE)
ParmUnLog(list(a = 2, b = 1, c = 5), logged = c("a", "b"), verbose = TRUE)

# --- matrix elements keep their shape ---
mat_params <- list(mu = 5, cov = matrix(c(100, 0, 0, 100), 2, 2))
ParmLog(mat_params, logged = "cov")   # cov becomes log10 of each entry, still 2x2
}
