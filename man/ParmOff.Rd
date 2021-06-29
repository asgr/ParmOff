\name{ParmOff}
\alias{ParmOff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Powerful Paramter Passing
}
\description{
Simple interface to allow the user to pass complex lists of arguments into functions, with matching and mis-matching rules.
}
\usage{
ParmOff(.func, .args = NULL, .use_args = NULL, .rem_args = NULL, .quote = TRUE,
  .envir = parent.frame(), .pass_dots = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{.func}{
Function; the function to be executed with provided arguments.
}
  \item{.args}{
List; arguments to potential be passed into \option{.func}.
}
  \item{.use_args}{
Character vector; arguments in \option{.args} and \dots will be restricted to this set. This intervention happens first.
}
  \item{.rem_args}{
Character vector; arguments in \option{.args} and \dots will have these arguments removed. This intervention happens second.
}
  \item{.quote}{
Logical; to be passed to \option{quote} argument of \code{\link{do.call}}. Leaving as TRUE is usually a good idea because debugging of large inputs is much easier.
}
  \item{.envir}{
Environment; to be passed to \option{envir} argument of \code{\link{do.call}}.
}
  \item{.pass_dots}{
Logical; if TRUE (default) and \option{.func} has a \dots argument input then unmatched arguments will be passed on (on the implicit assumption these will be matched by a function within \option{.func}). If FALSE or \option{.func} has no \dots argument input then only remaining matching arguments will be passed into \option{.func}. This intervention happens last.
}
  \item{\dots}{
Other arguments to be merged with \option{.args} and processed as discussed above.
}
}
\details{
These events happen in this exact order:

\enumerate{
  \item If \option{.args} and \dots are both present then any arguments in \dots that match by name are removed (\option{.args} takes precendence).
  \item Merge \option{.args} and \dots into one list (current_args).
  \item If \option{.use_args} is present then restrict current_args to these arguments.
  \item If \option{.rem_args} is present then remove these arguments from current_args.
  \item If \dots are not present in the arguments of \option{.func} or \option{.pass_dots}=FALSE then restrict the current_args list to only those arguments that appear in the \code{\link{formals}} of \option{.func}.
  \item Run \code{\link{do.call}} on the target \option{.func} with the remaining current_args, returning the output only.
}
}
\value{
Return of \option{.func} with whatever the remaining current_args are after the various layers of passing.
}
\author{
Aaron Robotham
}
\seealso{
\code{\link{do.call}}, \code{\link{formals}}
}
\examples{
#Pass a mixture of an argument list and dots, ignoring conflicting arguments from latter:

example_args = list(col='red', xlab='Test x', ylab='Test y')
ParmOff(plot, example_args, x=sin, xlab='Ignore This')

#Ignore the col argument (if present, which it is):
ParmOff(plot, example_args, .rem_args='col', x=sin, xlab='Ignore This')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
