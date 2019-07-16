#' Prepares the matrix of euclidean distances.
#'
#' This function assists in the process of importing a file with original
#' landmarks coordinates for each studied individuals and transforming these
#' data into the lower off-diagonal matrix of euclidean distance between all
#' ladmarks coordinates for each individual placed in a single row. It also allows
#' to import qualitative information for each individual resulting in factors in
#' the new data frame.
#'
#'
#' @param x A data frame containing the initial data of landmarks coordinates.
#' @param p Number of landmarks.
#' @param k Number of factors with qualitative information.
#' @param d Number of dimensions of the lanmdmarks coordinates.
#'
#' @return A data frame with the lower off-diagonal matrix of euclidean distance
#'   between all ladmarks coordinates for each individual placed in a single row. If
#'   k factors with qualitative information are also imported, factors are place in the
#'   k first columns of the new data frame.
#'
#' @examples
#' new_file_A <- iedma(original_file_A, p = 10)
#' new_file_B <- iedma(original_file_B, p = 10, k = 3)
#'
"iedma" <-  function(x, p, k = 0, d = 2) {
# x és el nom de la matriu; p el número de punts; k el nombre de factors, cap per defecte,
# i d el nombre de dimensions en les que es prenen els punts, 2 per defecte.
# És important recordar que en importar no s’ha d’agafar la primera columna com
# a etiquetes, ja que els noms dels punts i els possibles factors estan repetits
# -----make unique the first column and use it for row.names-----
  x[, 1] <- as.character(x[, 1])
  x[, 1] <- make.unique(x[, 1])
  row.names(x) <- x[, 1]
  x <- x[, -c(1)]
# -----create the new data.frame with n individuals, m columns and corresponding dimnames (z and zz)-----
  n <- dim(x)[1] / (p + k + 1)
  m <- k + ((p * (p - 1)) / 2)
  z <- vector(length = n)
  zz <- vector(length = m)
  varmat <- as.data.frame(matrix(0, n, m))
  varmat2 <- as.data.frame(matrix(0, p, d))
  compte <- 1
  compte2 <- 1 + k
  for (i in seq(1, n * (p + k + 1), by = (p + k + 1))) {
    z[compte] <- dimnames(x)[[1]][i]
    varmat2[, ] <- x[c((i + 1):(i + p)), c(1:d)]
    varmat[compte, c((k + 1):m)] <- c(dist(varmat2))
    if (k > 0) {
      for (kk in 1:k) {
        varmat[compte, kk] <- x[i + p + kk, d + 1]
      }
    }
    if (compte == 1) {
      if (k > 0) {
        for (kk in 1:k) {
          zz[kk] <- dimnames(x)[[1]][kk + p + 1]
        }
      }
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
  if (k > 0) {
    originallevels <- levels(x[, d + 1])
    for (i in 1:k) {
      varmat[, i] <- as.factor(varmat[, i])
      thislevels <- as.numeric(levels(varmat[, i]))
      varmat[, i] <- factor(varmat[, i], labels = originallevels[thislevels])
    }
  }
  varmat
}
