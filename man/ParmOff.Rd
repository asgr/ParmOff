\name{ParmOff}
\alias{ParmOff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Powerful Parameter Passing
}
\description{
Simple interface to allow the user to pass complex lists of arguments into functions, with matching and mis-matching rules.
}
\usage{
ParmOff(.func, .args = NULL, .use_args = NULL, .rem_args = NULL, .logged = NULL,
  .strip = NULL, .quote = TRUE, .envir = parent.frame(), .pass_dots = TRUE,
  .return = 'function', ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{.func}{
Function; the function to be executed with provided arguments.
}
  \item{.args}{
List (or named vector); arguments to potential be passed into \option{.func}. If not a list, then it will be coerced to one with one element per entry (user needs to make sure this is appropriate and fully named, but usually useful if passing in something like a vector of scalar parameters from a fit etc).
}
  \item{.use_args}{
Character vector; arguments in \option{.args} and \dots will be restricted to this set. This intervention happens first. The name/s provided should be as per post-stripped (if \option{.strip} is provided).
}
  \item{.rem_args}{
Character vector; arguments in \option{.args} and \dots will have these arguments removed. The name/s provided should be as per post-stripped (if \option{.strip} is provided).
}
  \item{.logged}{
Character vector; logged arguments in \option{.args} and \dots. These will be unlogged (using base 10), so only specify if this action is desired. The name/s provided should be as per post-stripped (if \option{.strip} is provided).
}
  \item{.strip}{
Character vector; a string element to strip out of all \option{.args} names. This intervention happens first.
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
  \item{.return}{
Character scalar; if 'function' then the output of \code{\link{do.call}} on the target \option{.func} with the remaining current_args, returning the output only. If 'args' then a list with the current_args (that meet all the matching critera) and ignore_args (provided but not to be used) are returned.
}
  \item{\dots}{
Other arguments to be merged with \option{.args} and processed as discussed above.
}
}
\details{
These events happen in this exact order:

\enumerate{
  \item \option{.args} has string elements of names removed if \option{.strip} is provided.
  \item If \option{.args} and \dots are both present then any arguments in \dots that match by name are removed (\option{.args} takes precedence).
  \item Merge \option{.args} and \dots into one list (current_args).
  \item current_args has names elements specided by \option{.logged} unlogged (using base 10).
  \item If \option{.use_args} is present then restrict current_args to these arguments.
  \item If \option{.rem_args} is present then remove these arguments from current_args.
  \item If \dots are not present in the arguments of \option{.func} or \option{.pass_dots}=FALSE then restrict the current_args list to only those arguments that appear in the \code{\link{formals}} of \option{.func}.
  \item Return the output of \code{\link{do.call}} or the current_args (depending on the setting of \option{.return}.
}

Whilst \option{.args} can be a vector input (see Examples), users need to be careful it is fully named and all of a common type (e.g. numeric in most cases). If in doubt, use a named list since this gives full control over the typing of each list element.

It should be noted that for trivially fast functions, \code{ParmOff} add a lot of overhead (see Examples). It is generally designed for convenience working with complex and CPU intensive functions (ones that take seconds rather than microseconds to run).
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

#Get the current_args and the ignore_args lists:
ParmOff(plot, example_args, .rem_args='col', x=sin, xlab='Ignore This', .return='args')

#An example of a complex model (note for non complex .args you can use a named vector):
model_ex = function(x,y,z){x * y + z}
input = c(x=1, y=2, z=3, t=4) #the input to pass into .args (note 't' will be ignored)
ParmOff(model_ex, input)

#And now tagging argument 'y' to be unlogged:
ParmOff(model_ex, input, .logged='y')

#Truth in advertising, because of the extra checking ParmOff slows down fast functions:

system.time(for(i in 1:1e4){model_ex(input[1], input[2], input[3])})
system.time(for(i in 1:1e4){do.call(model_ex, as.list(input)[1:3])})
system.time(for(i in 1:1e4){ParmOff(model_ex, input)})
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
