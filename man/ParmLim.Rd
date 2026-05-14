\name{ParmLim}
\alias{ParmLimLo}
\alias{ParmLimHi}
\alias{ParmLimBoth}
\title{
Flexible lower/upper limiting for vectors and lists
}
\description{
Helper functions to apply lower and upper limits to vectors or lists (including nested lists). These are used internally by \code{\link{ParmOff}}, but can also be used directly for generic list-processing workflows.
}
\usage{
ParmLimLo(x, lower)
ParmLimHi(x, upper)
ParmLimBoth(x, lower, upper)
}
\arguments{
  \item{x}{
Object to be limited. Usually a numeric vector/scalar, or a list containing numeric leaves. Nested lists are supported.
}
  \item{lower}{
Lower limit specification. Can be scalar/vector-like, or list-like to mirror all or part of \option{x}. Named list elements are matched by name where available. If \option{x} is a list and \option{lower} is vector then \option{lower} is applied to all list elements. Almost always if \option{x} is named list then \option{lower} should also be a named list (potentially just a subset).
}
  \item{upper}{
Upper limit specification. Can be scalar/vector-like, or list-like to mirror all or part of \option{x}. Named list elements are matched by name where available. If \option{x} is a list and \option{upper} is vector then \option{upper} is applied to all list elements. Almost always if \option{x} is named list then \option{upper} should also be a named list (potentially just a subset).
}
}
\details{
\code{ParmLimLo} applies \code{\link{pmax}} recursively; \code{ParmLimHi} applies \code{\link{pmin}} recursively. \code{ParmLimBoth} first applies lower limits, then upper limits.

For list inputs, bounds can be:
\itemize{
  \item Named lists (matched by element name),
  \item Unnamed lists (matched by position),
  \item Single-element unnamed lists (broadcast to all children),
  \item Scalars/vectors (applied to all leaves).
}

If a list child has no matching bound, that child is left unchanged.

For vector inputs, bounds can be:
\itemize{
  \item Named vector (matched by element names if \option{x} is also a named vector),
  \item Unnamed vector (matched by position, potentially with recycling).
}
}
\value{
An object with the same overall structure as \code{x}, with limits applied where possible.
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{ParmOff}}, \code{\link{pmax}}, \code{\link{pmin}}
}
\examples{
# Simple vector limits
ParmLimLo(c(a = -1, b = 2), lower = 0)
ParmLimHi(c(a = -1, b = 2), upper = 1)
ParmLimBoth(c(a = -1, b = 2), lower = 0, upper = 1)

# List and nested list limits
x <- list(a = -1, b = c(2, 5), c = list(d = -3, e = 8))

ParmLimLo(x, lower = list(a = 0, b = 3, c = list(d = -2)))
ParmLimHi(x, upper = list(a = 1, b = 4, c = list(e = 6)))
ParmLimBoth(x,
  lower = list(a = 0, b = 3, c = list(d = -2)),
  upper = list(a = 1, b = 4, c = list(e = 6))
)
}
