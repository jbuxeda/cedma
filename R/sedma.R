#' Selects a subset of points with or without factors
#'
#'
#'
#' @param x a matrix prepared with the iedma function.
#' @param p the points that will not be selected for use.
#' @param k logical value to select or otherwise the existing factors.
#'
#' @return
#' @export
#'
#' @examples
#' new_file_A <- sedma(original_file_A, k = FALSE) # all points will be used but not
#' the qualitative factors.
#' new_file_B <- iedma(original_file_B, p = c(2, 10)) # all points and factors will be
#' selected but points 2 and 10
#' new_file_C <- iedma(original_file_C, p = c(2, 3, 5), k = FALSE) # factors and points
#' 2, 3 and 5 will not be selected
"sedma" <-  function(x, p = c(0), k = TRUE) {
# x és el nom de la matriu; p els punts que es volen no utilitzar (el 0 el que fa és
# deixar-los tots); k és un operador lògic de si es preserven els factors amb
#  informació qualitativa o no
# -----identify how many factors exist in x and if they are selected-----
  j <- sum(sapply (x, is.factor) == TRUE)
  jj <- logical(length = dim(x)[[2]])
  jj <- !jj
  if (k != TRUE) {
    jj[c(1:j)] <- FALSE
  }
# -----identify how many landmarks exist and if they are selected-----
  dimen <- ((-1 + sqrt(1 + 8 * (length(jj) - j))) / 2) + 1
  compte2 <- 1 + j
  for (ii in 1:(dimen - 1)) {
    for (kk in (ii + 1):dimen) {
      for (ll in 1:length(p)) {
        if (p[ll] == ii | p[ll] == kk) {jj[compte2] <- FALSE}
      }
      compte2 <- compte2 + 1
    }
  }
  x[, jj]
}

