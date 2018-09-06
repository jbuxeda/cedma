"invedma" <-  function(x, united = F, labeled = F, net = F)  {
    # a partir de la matriu de distàncies d’EDMA calcula on van els punts i en dibuixa les línies
    # x és la matriu EDMA indicant sempre els individus a treballar
    # el càlcul dels punts es fa amb un multidimensional scaling i la identificació del contorn amb un
    # convex hull dels punts dibuixats. L’ordre de les línies a dibuixar s’estableix ordenant el convex hull
    # units (=F) són les línies entre els punts en el seu ordre natural
    # l’anterior és la inversa de (n*(n-1))/2 i troba les dimensions (dimen)
    # per defecte les línies no es dibuixen (linies=F)
    # etiquetes (que d’entrada no es posen =F) és per posar el nom dels punts
    # rotar permet, per proba i error, rotar la proposta de configuració de punts. L’angle s’entra en graus sexagesimals
    # i es transforma en radiants. La imatge és reflexada en l’eix de les x (reflexio=1) o de les y (reflexio=2)
    # xarxa (=F) permet tirar línies entre tots els punts
# -----calculate the number of landmarks and prepare distance matrix-----
  dimen <- ((-1 + sqrt(1 + 8 * length(x))) / 2) + 1
  aa <- matrix(0, dimen, dimen)
  aa[lower.tri(aa, diag = F)] <- c(as.matrix(x[1, ]))
  aa <- aa + t(aa)
# -----coordinates after metric multidimensional scaling-----
  fit <- cmdscale(aa, eig = T, k = 2)
  punts <- as.matrix(fit$points)
# -----rotate results with points according to positions of points 1 and 2-----
  if (punts[1, 2] < punts[2, 2]) {
    reflex <- matrix(c(1, 0, 0, -1), 2, 2)
    punts <- punts %*% reflex
  }
  if (punts[1, 1] < punts[10, 1]) {
    reflex <- matrix(c(-1, 0, 0, 1), 2, 2)
    punts <- punts %*% reflex
  }
  rotar <- atan((punts[2, 1] - punts[1, 1]) / (punts[2, 2] - punts[1, 2]))
  if (rotar < 0) {rotar = rotar * (-1)}
  rotacio <- matrix(c(cos(rotar), sin(rotar), -sin(rotar), cos(rotar)), 2, 2)
  punts <- punts %*% rotacio
# -----plot according options-----
  minim <- min(punts)
  maxim <- max(punts)
  plot(punts[, 1], punts[, 2], pch = 16, xlab = "Abscisses", ylab = "Ordenades", xlim = c(minim, maxim), ylim = c(minim, maxim))
  if (united == T) {
    if (net == T) {
      lines(punts[, 1], punts[, 2], col = gray(0.5), lwd=4)
      } else {
      lines(punts[, 1], punts[, 2], col = gray(0.4))}
    }
  if (net == T) {
    for (ii in 1:(dimen - 1)) {
      for (jj in (ii + 1):dimen) {
        lines(c(punts[ii, 1], punts[jj, 1]), c(punts[ii, 2], punts[jj, 2]), col="chocolate1")
        }
      }
    }
  if (labeled == T) {
      text(punts[, 1], punts[, 2], labels = dimnames(as.data.frame(aa))[[1]], pos = 4, cex = 0.7, col = gray(0.4))
  }
# -----output of plot coordinates-----
    as.data.frame(punts)
}
