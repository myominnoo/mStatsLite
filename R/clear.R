#' @title Clean Global Environment, Plots and Console
#'
#' @description
#'
#' \code{clear()} cleans three R workspaces: Global Environment, Plots and
#' R Console.
#'
#' @details
#'
#' \code{clear()} removes all objects from Global Environment, clear plots
#' and R Console (the same as pressing \code{Ctrl + L}). It does not require
#' any arguments.
#'
#' This can be used as an initial command line to clear the workspace as
#' follows:
#'
#' \preformatted{
#' ## Clear the workspace
#' clear()
#' }
#'
#' \code{clear} does not interrupt logging process of \code{ilog}.
#'
#' @keywords remove all, clean, clear, global environment, plots, console
#'
#' @import grDevices
#'
#' @author
#'
#' For any feedback, please contact \code{Myo Minn Oo} via:
#'
#' Email: \email{dr.myominnoo@@gmail.com}
#'
#' Website: \url{https://myominnoo.github.io/}
#'
#' @examples
#' # create x by generating 100 random values
#' x <- rnorm(100)
#'
#' # plotting x
#' plot(x)
#'
#' # calling clear to clean workspace
#' clear()

#' @export
clear <- function() {
    while (!is.null(dev.list()))  dev.off()
    rm(list = ls(envir = .GlobalEnv), pos = 1)
    cat("\014")
}
