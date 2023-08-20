#' CosPdf
#'
#' Using the cos method to restore the probability density of a function
#'
#' @param y vector of observations
#' @param a the lower limit of the truncation interval
#' @param b the upper limit of the truncation interval
#' @param N the number of cos term for summation
#' @param Chf the characteristic function
#'
#' @return The approximated probability density of x
#'
#' @references Fang F. and Oosterlee C.W. 2008. "A Novel Pricing Method for European Options Based on Fourier-Cosine Series Expansions", Siam Journal on Scientific Computing. 31(2): 826-848. doi: 10.1137/080718061.
#'
#' @export
#'



CosPdf <- function(y,a,b,N,Chf){
  Pdfone <- function(y,a,b,N,Chf){
    k <- 0 : (N - 1)
    u <- k * pi / (b-a)
    F_k <- 2.0 / (b-a) * Re(Chf(u) * exp(-1i * u * a))
    F_k[1] <- 0.5 * F_k[1]
    f_y <- sum(F_k * cos(u * (y-a)))
    return(f_y)
  }
  return(sapply(y,Pdfone, a,b,N,Chf))
}
