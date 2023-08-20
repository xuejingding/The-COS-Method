#' CosPlot
#'
#'Plot the p.d.f function for the univariate distribution with x and y
#'
#' @param data a tibble， which contains x and y
#' @param x x
#' @param y y, which is the pdf returned from CosPdf
#'
#' @import ggplot2
#' @import tibble
#' @return A ggplot figure of the probability density function
#'
#' @export
#'
#' @examples
#' N <- 64
#' y <- seq(-5, 5, by = 0.1)
#' a <- -6.0
#' b <- 6.0
#' NormChf <- function(u) {mu = 0;sigma = 1;return(exp(1i * mu * u - 0.5 * sigma ^ 2 * u ^ 2))}
#' cosnorm <- CosPdf(y, a, b, N, NormChf)
#' require("tibble")
#' cosnorm <- tibble(x=y,y=cosnorm)
#' nor <- CosPlot(data=cosnorm,x=x,y=y)
#' nor



CosPlot <- function(data,x,y){
  gg <- ggplot(data= data)+
    geom_point(mapping = aes(x=x,y=y),col="blue")+
    xlab("x")+
    ylab("fx")+
    theme_bw()
  return(gg)
}
