#' Install Python packages
#'
#' Install Python dependencies into a virtual environment or Conda environment.
#'
#' @param envname The name, or full path, of the environment in which Python
#'   packages are to be installed. When `NULL` (the default), the active
#'   environment as set by the `RETICULATE_PYTHON_ENV` variable will be used;
#'   if that is unset, then the `r-reticulate` environment will be used.
#'
#' @param method Installation method. By default, "auto" automatically finds a
#'   method that will work in the local environment. Change the default to force
#'   a specific installation method. Note that the "virtualenv" method is not
#'   available on Windows.
#'
#' @param conda Path to conda executable (or "auto" to find conda using the
#'   PATH and other conventional install locations).
#'
#' @param python_version The requested Python version. Ignored when attempting
#'   to install with a Python virtual environment.
#'
#' @param pip Boolean; use `pip` for package installation? This is only relevant
#'   when Conda environments are used, as otherwise packages will be installed
#'   from the Conda repositories.
#'
#' @param ... Additional arguments passed to [conda_install()]
#'   or [virtualenv_install()].
#'
#' @details On Linux and OS X the "virtualenv" method will be used by default
#'   ("conda" will be used if virtualenv isn't available). On Windows, the
#'   "conda" method is always used.
#'
#' @import reticulate
#' @export
install_mlr3tabnet <- function(gpu=TRUE,
                               envname = NULL,
                               method = "auto",
                               conda = "auto",
                               python_version = NULL,
                               ...) {
  pytorch <- ifelse(gpu, "pytorch", "pytorch-cpu")
  reticulate::py_install(
    c("pytorch-tabnet", "fastai", pytorch),
    ...
  )
}
