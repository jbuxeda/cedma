#' Prepare the matrix of euclidean distances from the original landmarks
#' coordinates.
#'
#' This function assists in the process of importing a file with original
#' landmarks coordinates for each studied individuals and transforming these
#' data into the lower triangular matrix of euclidean distance between all
#' ladmarks coordinates for each individual placed in a single row.
#'
#'
#' @param x a matrix containing the initial data of landmarks coordinates.
#' @param p number of landmarks.
#' @param k number of dimensions of the coordinates.
#'
#' @return A matrix with the lower triangular matrix of euclidean distance
#'   between all ladmarks coordinates for each individual placed in a single row.
#'
#' @examples
#' new_file <- iedma(original_file, 10)
#'
"iedma" <-  function(x, p, k = 2) {
# x és el nom de la matriu; p el número de punts; k les dimensions, 2 per defecte
# és important recordar que en importar no s’ha d’agafar la primera columna com
# a etiquetes, ja que els noms dels punts estan repetits
# -----make unique the first column and use it for row.names-----
  x[, 1] <- as.character(x[, 1])
  x[, 1] <- make.unique(x[, 1])
  row.names(x) <- x[, 1]
  x <- x[, -c(1)]
# -----create the new data.frame with n individuals, m columns and corresponding dimnames (z and zz)-----
  n <- dim(x)[1] / (p + 1)
  m <- (p * (p - 1)) / 2
  z <- vector(length = n)
  zz <- vector(length = m)
  varmat <- matrix(0, n, m)
  varmat2 <- matrix(0, p, k)
  compte <- 1
  compte2 <- 1
  for (i in seq(1, n * (p + 1), by = (p + 1))) {
    z[compte] <- dimnames(x)[[1]][i]
    varmat2 <- x[c((i + 1):(i + p)), ]
    varmat[compte, ] <- c(dist(varmat2))
    if (compte == 1) {
      for (ii in 1:(p - 1)) {
        for (jj in (ii + 1):p) {
          zz[compte2] <- as.character(paste(dimnames(varmat2)[[1]][ii], dimnames(varmat2)[[1]][jj], sep = "-"))
          compte2 <- compte2 + 1
          }
        }
      }
    compte <- compte + 1
  }
# -----output the new data.frame-----
  dimnames(varmat) <- list(c(z), c(zz))
  as.data.frame(varmat)
}
