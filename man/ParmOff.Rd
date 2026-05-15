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
ParmOff(.func, .args = NULL, .use_args = NULL, .rem_args = NULL,
  .lower = NULL, .upper = NULL, .logged = NULL, .strip = NULL, .quote = TRUE,
  .envir = parent.frame(), .pass_dots = TRUE, .return = 'func', .check = TRUE,
  .bound_raw = TRUE, .log_type = 'log10', .clash = 'first', .verbose = FALSE, ...)
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
  \item{.lower}{
Lower limit specification. Can be scalar/vector-like, or list-like to mirror all or part of \option{.args}. Named list elements are matched by name where available. If \option{.args} is a list and \option{lower} is vector then \option{lower} is applied to all list elements. Almost always if \option{.args} is named list then \option{lower} should also be a named list (potentially just a subset).
}
  \item{.upper}{
Upper limit specification. Can be scalar/vector-like, or list-like to mirror all or part of \option{.args}. Named list elements are matched by name where available. If \option{.args} is a list and \option{upper} is vector then \option{upper} is applied to all list elements. Almost always if \option{.args} is named list then \option{upper} should also be a named list (potentially just a subset).
}
  \item{.logged}{
Character/logical vector; logged arguments in \option{.args} and \dots. These will be unlogged (using base 10), so only specify if this action is desired. The name/s provided should be as per post-stripped (if \option{.strip} is provided). If providing a logical vector it must also be exactly the same length as \option{.args}. We deliberately do not allow just a positional (which-like) integer vector because it is very easy to create subtle bugs.
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
Character scalar; if 'func' (or 'function') then the output of \code{\link{do.call}} on the target \option{.func} with the remaining 'current_args', returning the output only. If 'args' (or 'arg') then a list with the 'current_args' (that meet all the matching critera) and 'ignore_args' (provided but not to be used) are returned. If 'func_args' (or 'func_arg') then both are returned in a list with items 'func_out' and 'args' respectively.
}
  \item{.check}{
Logical; should input argument checking be carried out? Obviously safer to say TRUE (default), but in speed critical cases you might want to set to FALSE.
}
  \item{.bound_raw}{
Logical; should \option{.lower} and \option{.upper} bounds be applied pre de-logging (via \option{.logged}) i.e. TRUE (default), or after de-logging i.e. FALSE.
}
  \item{.log_type}{
Character scalar. Use \code{'log10'} (default) for base-10 transformations
(\code{\link{log10}} / \code{10^x}), \code{'ln'} for natural-log transformations (\code{\link{log}} / \code{\link{exp}}) or \code{'log2'} for base-2 transformations (\code{\link{log2}} / \code{2^x}).
}
  \item{.clash}{
Character scalar; what to do if multiple \option{.args} have the same name. Since this is generally not allowed when passing arguments into a function, you generally either want to take the 'first' (default) or 'last' option. There is also the option to do 'nothing', which is mainly for de-bugging purposes. Note \dots are only used if their names do not clash with the initial provided \option{.args}, i.e. the latter always takes priority if there is a clash (no matter the setting of \option{.clash}.
}
  \item{.verbose}{
Logical; if TRUE it will print to screen the names of the used and ignored arguments, and also forwards the flag to \code{\link{ParmLimLo}}, \code{\link{ParmLimHi}}, and \code{\link{ParmUnLog}} so that lower/upper limit clamping and de-logging operations report the parameter name and its before/after values. Output is only printed for elements that are simple scalars, vectors of length \eqn{\le 20}, or matrices of size \eqn{\le 10 \times 10}. If FALSE then no verbose output is printed (unless by the called function itself).
}
  \item{\dots}{
Other arguments to be merged with \option{.args} and processed as discussed above. Note \dots are only used if their names do not clash with the initial provided \option{.args}, i.e. the latter always takes priority if there is a clash (no matter the setting of \option{.clash}.
}
}
\details{
These events happen in this exact order:

\enumerate{
  \item \option{.args} has string elements of names removed if \option{.strip} is provided.
  \item If \option{.args} and \dots are both present then any arguments in \dots that match by name are removed (\option{.args} takes precedence).
  \item Merge \option{.args} and \dots into one list (current_args).
  \item If \option{.bound_raw} is TRUE (default): if \option{.lower} is provided, clamp named current_args to the specified minimum values; if \option{.upper} is provided, clamp to the specified maximum values.
  \item current_args has named elements specified by \option{.logged} unlogged (using base 10).
  \item If \option{.bound_raw} is FALSE: if \option{.lower} is provided, clamp named current_args to the specified minimum values; if \option{.upper} is provided, clamp to the specified maximum values.
  \item If \option{.use_args} is present then restrict current_args to these arguments.
  \item If \option{.rem_args} is present then remove these arguments from current_args.
  \item If \dots are not present in the arguments of \option{.func} or \option{.pass_dots}=FALSE then restrict the current_args list to only those arguments that appear in the \code{\link{formals}} of \option{.func}.
  \item If there are any name clashes in the remaining current_args then disambiguate using the setting of \option{.clash}.
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

#Bound argument values before passing:
ParmOff(model_ex, input, .lower=list(x=0, y=0.5), .upper=list(y=1.5, z=2))

#Bounding happens before de-logging, so bounds are in log10 space:
ParmOff(model_ex, input, .logged='y', .lower=list(y=0), .upper=list(y=1))

#Truth in advertising, because of the extra checking ParmOff slows down fast functions:

system.time(for(i in 1:1e4){model_ex(input[1], input[2], input[3])})
arg_list = as.list(input)[1:3]
system.time(for(i in 1:1e4){do.call(model_ex, arg_list)})
system.time(for(i in 1:1e4){ParmOff(model_ex, arg_list)})
system.time(for(i in 1:1e4){ParmOff(model_ex, arg_list, .check=FALSE)})
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
