% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fork.R
\name{eval_safe}
\alias{eval_safe}
\alias{eval_fork}
\title{Safe Evaluation}
\usage{
eval_safe(expr, tmp = tempfile("fork"), std_out = stdout(),
  std_err = stderr(), timeout = 0, priority = NULL, uid = NULL,
  gid = NULL, rlimits = NULL, profile = NULL, device = pdf)

eval_fork(expr, tmp = tempfile("fork"), std_out = stdout(),
  std_err = stderr(), timeout = 0)
}
\arguments{
\item{expr}{expression to evaluate}

\item{tmp}{the value of \code{\link[=tempdir]{tempdir()}} inside the forked process}

\item{std_out}{if and where to direct child process \code{STDOUT}. Must be one of
\code{TRUE}, \code{FALSE}, filename, connection object or callback function. See section
on \emph{Output Streams} below for details.}

\item{std_err}{if and where to direct child process \code{STDERR}. Must be one of
\code{TRUE}, \code{FALSE}, filename, connection object or callback function. See section
on \emph{Output Streams} below for details.}

\item{timeout}{maximum time in seconds to allow for call to return}

\item{priority}{(integer) priority of the child process. High value is low priority.
Non root user may only raise this value (decrease priority)}

\item{uid}{evaluate as given user (uid or name). See \code{\link[unix:setuid]{unix::setuid()}}, only for root.}

\item{gid}{evaluate as given group (gid or name). See \code{\link[unix:setgid]{unix::setgid()}} only for root.}

\item{rlimits}{named vector/list with rlimit values, for example: \code{c(cpu = 60, fsize = 1e6)}.}

\item{profile}{AppArmor profile, see \code{RAppArmor::aa_change_profile()}.
Requires the \code{RAppArmor} package (Debian/Ubuntu only)}

\item{device}{graphics device to use in the fork, see \code{\link[=dev.new]{dev.new()}}}
}
\description{
Evaluates an expression in a temporary fork and returns the value without any
side effects on the main R session. For \code{\link[=eval_safe]{eval_safe()}} the expression is wrapped
in additional R code to handle errors and graphics.
}
\details{
Some programs such as \code{Java} are not fork-safe and cannot be called from within a
forked process if they have already been loaded in the main process. On MacOS any
software calling \code{CoreFoundation} functionality might crash within the fork. This
includes \code{libcurl} which has been built on OSX against native SecureTransport rather
than OpenSSL for https connections. The same limitations hold for e.g. \code{parallel::mcparallel()}.
}
\examples{
#Only works on Unix
if(.Platform$OS.type == "unix"){

# works like regular eval:
eval_safe(rnorm(5))

# Exceptions get propagated
test <- function() { doesnotexit() }
tryCatch(eval_safe(test()), error = function(e){
  cat("oh no!", e$message, "\\n")
})

# Honor interrupt and timeout, even inside C evaluations
try(eval_safe(svd(matrix(rnorm(1e8), 1e4)), timeout = 2))

# Capture output
outcon <- rawConnection(raw(0), "r+")
eval_safe(print(sessionInfo()), std_out = outcon)
cat(rawToChar(rawConnectionValue(outcon)))
}
}
