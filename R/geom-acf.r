######################################### GEOM ACF AND PACF ########################################

# GEOMS --------------------------------------------------------------------------------------------

#' ACF And PACF For \code{ggplot2}
#' 
#' \code{geoms} and \code{stats} for plotting ACF and PACF directly from data
#' 
#' \code{geom_acf} and \code{geom_pacf} take as \code{y} aesthetic the name of the column containing
#' the sereis whose ACF or PACF is to be plotted. This means that scaling functions such as 
#' \code{\link[ggplot2:scale_y_continuous]{scale_y_continuous}} apply 
#' \strong{DIRECTLY TO THE SERIES} and not to the autocorrelation values computed. To trim the plot, 
#' use \code{\link[ggplot2:coord_cartesian]{coord_cartesian()}}
#' 
#' @section Aesthetics:
#' 
#' \code{geom_acf()} and \code{geom_pacf()} are essentially wrappers for \code{geom_line()}, so that
#' all aesthetics understood by it work with these new functions. The only required aesthetic is 
#' \strong{y}
#' 
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}} or 
#'     \code{\link[ggplot2:aes_]{aes_()}}. If specified and \code{inherit.aes = TRUE} (the default),
#'     it is combined with the default mapping at the top level of the plot. You must supply mapping
#'     if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#' 
#' If \code{NULL}, the default, the data is inherited from the plot data as specified in the 
#' call to \code{\link[ggplot2:ggplot]{ggplot()}}.
#' 
#' A \code{data.frame}, or other object, will override the plot data. All objects will be 
#' fortified to produce a data frame. See \code{\link[ggplot2:fortify]{fortify()}} for which 
#' variables will be created.
#' 
#' A function will be called with a single argument, the plot data. The return value must be a 
#' \code{data.frame}, and will be used as the layer data. A function can be created from a 
#' formula (e.g. ~ head(.x, 10)).
#' @param na.rm If \code{FALSE}, the dagult, missing values are removed with a warning. If 
#'     \code{TRUE}, missing values are silentry removed
#' @param show.legend logical. Shoud this layer be include in the legends? \code{NA}, de default, 
#'     includes if any aesthetics are mapped. \code{FALSE} never inlcudes and \code{TRUE} always 
#'     includes. It can also be a named logical vector to finely select the aesthetics to display
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather than combining with 
#'     them. This is most useful for helper functions that define both data and aesthetics and 
#'     shouldn't inherit behaviour from the default plot specification, e.g. 
#'     \code{\link[ggplot2:borders]{borders()}}.
#' @param ... Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are often 
#'     aesthetics, used to set an aesthetic to a fixed value, like \code{colour = "red"} or 
#'     \code{size = 3}. They may also be parameters to the paired geom/stat.
#' 
#' @examples 
#' 
#' # its still necessary to attach ggplot2 first
#' library(ggplot2)
#' 
#' dplot <- data.frame(series = arima.sim(1000, model = list(ar = .7)))
#' 
#' ggplot(dplot, aes(y = series)) + geom_acf()
#' ggplot(dplot, aes(y = series)) + geom_pacf()
#' 
#' ggplot(dplot, aes(y = series)) + geom_acf(lag.max = 150)
#' 
#' # exp option is more useful when checking for heterokedasticity
#' ggplot(dplot, aes(y = series)) + geom_acf(exp = 2)
#' 
#' # There is a problem when using scale_y_continuous that distorts the autocorrelation values. 
#' # Should the user want to limit the y axis, coord_cartesian should be used
#' ggplot(dplot, aes(y = series)) + geom_acf() + coord_cartesian(ylim = c(0, .3))
#' 
#' @export
#' 
#' @rdname gg_acf_pacf

geom_acf <- function(mapping = NULL, data = NULL, na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
    list(
        stat_ACF(mapping = NULL, data = NULL, na.rm = FALSE, show.legend = NA,
                 inherit.aes = TRUE, ...),
        stat_CONF(mapping = NULL, data = NULL, na.rm = FALSE, show.legend = NA,
                  inherit.aes = TRUE)
    )
}

#' @export
#' 
#' @rdname gg_acf_pacf

geom_pacf <- function(mapping = NULL, data = NULL, na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
    list(
        stat_PACF(mapping = NULL, data = NULL, na.rm = FALSE, show.legend = NA,
                 inherit.aes = TRUE, ...),
        stat_CONF(mapping = NULL, data = NULL, na.rm = FALSE, show.legend = NA,
                  inherit.aes = TRUE)
    )
}

# STATS --------------------------------------------------------------------------------------------

#' @param lag.max number of lags to plot
#' @param exp power to which the series will be exponentiated before computing its ACF of PACF. This
#'     is meant as a facilitor when checking residuals for heterokedasticity
#' 
#' @export
#' 
#' @rdname gg_acf_pacf

stat_ACF <- function(mapping = NULL, data = NULL, na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, lag.max = 20, exp = 1, ...) {

    layer(
        stat = statACF, data = data, mapping = aes(x = ..lag.., group = ..lag..), geom = "line",
        position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, lag.max = lag.max, exp = exp, ...)
    )
}

#' @export
#' 
#' @rdname gg_acf_pacf

stat_PACF <- function(mapping = NULL, data = NULL, na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, lag.max = 20, exp = 1, ...) {

    layer(
        stat = statPACF, data = data, mapping = aes(x = ..lag.., group = ..lag..), geom = "line",
        position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, lag.max = lag.max, exp = exp, ...)
    )
}

#' @export
#' 
#' @rdname gg_acf_pacf

stat_CONF <- function(mapping = NULL, data = NULL, na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {

    layer(
        stat = statCONF, data = data, mapping = aes(yintercept = ..conf..), geom = "hline",
        position = "identity", show.legend = show.legend, inherit.aes = FALSE,
        params = list(na.rm = na.rm, color = "blue4", linetype = 2, ...)
    )
}

# GGPROTO ------------------------------------------------------------------------------------------

statACF <- ggproto("statACF", Stat,
    compute_group = function(data, scales, lag.max = 20, exp = 1) {
        acf <- acf(data$y^exp, lag.max = lag.max, plot = FALSE)[[1]]
        d <- data.frame(lag = 0:lag.max, y = acf)
        d <- rbind(d, data.frame(lag = 0:lag.max, y = rep(0, (lag.max + 1))))
        d
    },
    required_aes = c("y")
)

statPACF <- ggproto("statPACF", Stat,
    compute_group = function(data, scales, lag.max = 20, exp = 1) {
        pacf <- pacf(data$y^exp, lag.max = lag.max, plot = FALSE)[[1]]
        d <- data.frame(lag = 1:lag.max, y = pacf)
        d <- rbind(d, data.frame(lag = 1:(lag.max + 1), y = rep(0, (lag.max + 1))))
        d
    },
    required_aes = c("y")
)

statCONF <- ggproto("statCONF", Stat,
    compute_group = function(data, scales) {
        conf <- 2 / sqrt(nrow(data))
        data.frame(conf = c(conf, -conf))
    }
)
