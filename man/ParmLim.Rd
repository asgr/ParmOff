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
\code{ParmLimLo} applies \code{\link{pmax}} recursively; \code{ParmLimHi} applies \code{\link{pmin}} recursively. \code{ParmLimBoth} first applies lower limits (via \code{ParmLimLo}), then upper limits (via \code{ParmLimHi}).

\strong{Behaviour when \code{x} is a list}

The bound is applied recursively to each child element. The child bound is
determined as follows, in order:

\enumerate{
  \item \strong{Bound is a named list}: each child of \code{x} is matched by
        name. Children whose names appear in the bound list receive the
        corresponding sub-bound; children with no matching name in the bound
        are left unchanged. This means you only need to specify bounds for the
        children you care about.

  \item \strong{Bound is an unnamed list with one element}: that single element
        is broadcast to every child of \code{x}.

  \item \strong{Bound is an unnamed list with multiple elements}: children are
        matched by position (index). Children beyond the length of the bound
        list are left unchanged.

  \item \strong{Bound is a scalar or atomic vector (not a list)}: the bound
        value is broadcast to every leaf in the entire tree, no matter how
        deeply nested.
}

If the bound is a list but \code{x} is an \emph{atomic} vector (a leaf node),
the bound is silently ignored and \code{x} is returned unchanged, because a
list bound has no meaning at a scalar leaf.

\strong{Behaviour when \code{x} is an atomic vector}

\enumerate{
  \item \strong{Both \code{x} and bound are named}: elements are aligned by
        name. Elements of \code{x} whose names have no corresponding entry in
        the bound (after name alignment) are left unchanged.

  \item \strong{Bound is unnamed (scalar or vector)}: applied directly via
        \code{\link{pmax}} / \code{\link{pmin}}, which recycles \code{bound}
        across the elements of \code{x} in the standard R manner.

  \item \strong{Bound is a list}: ignored; \code{x} is returned unchanged.
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
# -----------------------------------------------------------------------
# 1.  Pure vectors
# -----------------------------------------------------------------------

# Unnamed vector: scalar bound applied element-wise
ParmLimLo(c(-3, 0, 5), lower = 0)      # 0  0  5
ParmLimHi(c(-3, 0, 5), upper = 2)      # -3 0  2
ParmLimBoth(c(-3, 0, 5), lower = 0, upper = 2)  # 0  0  2

# Named vector: named bound aligned by name; unmatched elements unchanged
x <- c(a = -5, b = 10, c = 3)
ParmLimLo(x, lower = c(a = 0))          # a=0, b and c unchanged
ParmLimHi(x, upper = c(b = 7))          # b=7, a and c unchanged
ParmLimBoth(x, lower = c(a = 0), upper = c(b = 7))  # a=0, b=7, c unchanged

# Named vector: unnamed bound recycled element-wise (pmax/pmin semantics)
ParmLimLo(c(a = -1, b = 2, c = -3), lower = c(0, 0, 0))  # 0 2 0

# -----------------------------------------------------------------------
# 2.  Simple named list
# -----------------------------------------------------------------------

x <- list(a = -1, b = 5)

# Named list bound: only named children are affected
ParmLimLo(x, lower = list(a = 0, b = 3))   # a=0,  b=5 (already >= 3)
ParmLimHi(x, upper = list(a = 1, b = 4))   # a=-1, b=4
ParmLimBoth(x,
  lower = list(a = 0, b = 3),
  upper = list(a = 1, b = 4)
)

# Partial bound: only 'a' has a lower bound; 'b' is untouched
ParmLimLo(x, lower = list(a = 0))   # a=0, b=5

# Scalar broadcast: applies the same bound to every list element
ParmLimLo(x, lower = 0)    # a=0,  b=5

# -----------------------------------------------------------------------
# 3.  Nested list (2 levels)
# -----------------------------------------------------------------------

x <- list(a = -1, b = list(c = 5, d = -3), e = 9)

# Named nested bound — only the named children at each level are bounded
ParmLimLo(x, lower = list(a = 0, b = list(d = -2)))
# a=0, b$c unchanged (no bound), b$d=-2, e unchanged

ParmLimHi(x, upper = list(b = list(c = 4), e = 8))
# a unchanged, b$c=4, b$d unchanged, e=8

# Scalar broadcast to all leaves, regardless of nesting depth
ParmLimLo(x, lower = 0)   # a=0, b$c=5 (already >=0), b$d=0, e=9

# -----------------------------------------------------------------------
# 4.  Deeply nested list (3 levels)
# -----------------------------------------------------------------------

deep <- list(p = list(q = list(r = -5, s = 20), t = 3), u = -1)

lower <- list(p = list(q = list(r = 0), t = 0), u = 0)
upper <- list(p = list(q = list(s = 10), t = 5))

ParmLimBoth(deep, lower = lower, upper = upper)
# p$q$r: clamped to 0 (was -5)
# p$q$s: clamped to 10 (was 20)
# p$t:   3  (above lower 0, below upper 5 — unchanged)
# u:     0  (clamped to lower 0)

# Scalar bounds broadcast all the way to the deepest leaves
ParmLimBoth(deep, lower = 0, upper = 10)

# -----------------------------------------------------------------------
# 5.  ParmOff integration
# -----------------------------------------------------------------------

model <- function(x, y, z) x + y + z

# Clamp individual arguments before calling the model
ParmOff(model, list(x = -1, y = 20, z = 3),
        .lower = list(x = 0, y = 0),
        .upper = list(y = 10))
# x: 0, y: 10, z: 3  → sum = 13
}
