# ParmOff (R package)

<!-- badges: start -->
![R-CMD-check](https://github.com/asgr/ParmOff/workflows/R-CMD-check/badge.svg)
<!-- badges: end -->

## Synopsis

Core package providing a simple interface to allow highly flexible and generic function argument matching. Allows users to pass complex lists of arguments into functions, with matching and mis-matching rules.

## Installation

### Getting R

First things first, you will probably want to install a recent version of **R** that lets you build packages from source. The advantage of choosing this route is you can then update bleeding edge versions directly from GitHub. If you rely on the pre-built binaries on CRAN you might be waiting much longer.

#### Mac

For Mac just get the latest binaries from the **R** project pages:

<https://cloud.r-project.org/bin/macosx/>

#### Windows

For Windows just get the latest binaries from the **R** project pages:

<https://cloud.r-project.org/bin/windows/>

#### Linux

Debian:	`sudo apt-get install r-base r-base-dev`

Fedora:	`sudo yum install R`

Suse:	More of a pain, see here <https://cloud.r-project.org/bin/linux/suse/README.html>

Ubuntu:	`sudo apt-get install r-base-dev`

All the info on binaries is here: <https://cloud.r-project.org/bin/linux/>

### Getting ParmOff

Source installation from GitHub should be easy:

```R
install.packages('remotes')
remotes::install_github("asgr/ParmOff")
library(ParmOff)
```

## Code Example

```R
# Pass a mixture of an argument list and dots, ignoring conflicting arguments from latter:
example_args = list(col='red', xlab='Test x', ylab='Test y')
ParmOff(plot, example_args, x=sin, xlab='Ignore This')

# Ignore the col argument (if present, which it is):
ParmOff(plot, example_args, .rem_args='col', x=sin, xlab='Ignore This')

# An example of a complex model (note for non complex .args you can use a named vector):
model_ex = function(x, y, z){x * y + z}
input = c(x=1, y=2, z=3, t=4) # the input to pass into .args (note 't' will be ignored)
ParmOff(model_ex, input)
```

To find more examples, please check the documentation provided. You can browse these with:

```R
?ParmOff
```

## Contributors

Aaron Robotham

## License

LGPL-3+
