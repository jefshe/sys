#' @export
eval_safe_background<- function(expr, tmp = tempfile("fork"), std_out = stdout(), std_err = stderr(),
                      timeout = 0, priority = NULL, uid = NULL, gid = NULL, rlimits = NULL,
                      profile = NULL, device = pdf){
  orig_expr <- substitute(expr)
  out <- eval_fork_background(expr = tryCatch({
    if(length(priority))
      set_priority(priority)
    if(length(rlimits))
      set_rlimits(rlimits)
    if(length(gid))
      setgid(gid)
    if(length(uid))
      setuid(uid)
    if(length(profile))
      aa_change_profile(profile)
    if(length(device))
      options(device = device)
    graphics.off()
    options(menu.graphics = FALSE)
    withVisible(eval(orig_expr, parent.frame()))
  }, error = function(e){
    old_class <- attr(e, "class")
    structure(e, class = c(old_class, "eval_fork_error"))
  }, finally = substitute(graphics.off())),
  tmp = tmp, timeout = timeout, std_out = std_out, std_err = std_err)
  if(inherits(out, "eval_fork_error"))
    base::stop(out)
  if(out$visible)
    out$value
  else
    invisible(out$value)
}


#' @export
#' @useDynLib sys R_fork
eval_fork_background<- function(expr, tmp = tempfile("fork")) {
  # Convert TRUE or filepath into connection objects
  clenv <- force(parent.frame())
  clexpr <- substitute(expr)
  if(!file.exists(tmp))
    dir.create(tmp)
  tmp <- normalizePath(tmp)
  .Call(R_fork, clexpr, clenv, tmp)
}

#' @export
#' @useDynLib sys R_fork_collect
fork_collect <- function(pid, std_out = stdout(), std_err = stderr(), timeout = 0)
{
  if(length(timeout)){
    stopifnot(is.numeric(timeout))
    timeout <- as.double(timeout)
  } else {
    timeout <- as.numeric(0)
  }
  std_out <- if(isTRUE(std_out) || identical(std_out, "")){
    stdout()
  } else if(is.character(std_out)){
    file(normalizePath(std_out, mustWork = FALSE))
  } else std_out

  std_err <- if(isTRUE(std_err) || identical(std_err, "")){
    stderr()
  } else if(is.character(std_err)){
    std_err <- file(normalizePath(std_err, mustWork = FALSE))
  } else std_err

  # Define the callbacks
  outfun <- if(inherits(std_out, "connection")){
    if(!isOpen(std_out)){
      open(std_out, "wb")
      on.exit(close(std_out), add = TRUE)
    }
    if(identical(summary(std_out)$text, "text")){
      function(x){
        cat(rawToChar(x), file = std_out)
        flush(std_out)
      }
    } else {
      function(x){
        writeBin(x, con = std_out)
        flush(std_out)
      }
    }
  }
  errfun <- if(inherits(std_err, "connection")){
    if(!isOpen(std_err)){
      open(std_err, "wb")
      on.exit(close(std_err), add = TRUE)
    }
    if(identical(summary(std_err)$text, "text")){
      function(x){
        cat(rawToChar(x), file = std_err)
        flush(std_err)
      }
    } else {
      function(x){
        writeBin(x, con = std_err)
        flush(std_err)
      }
    }
  }
  .Call(R_fork_collect, pid, timeout, outfun, errfun)
}
