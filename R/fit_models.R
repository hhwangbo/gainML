#' @importFrom fields rdist
#' @importFrom stats dnorm
fit.akern <- function(y, x, x.new, id.circ, k, gcv, adj = (1/3)) {
    pred <- rep(NA, nrow(x.new))
    trace <- 0
    for (j in 1:nrow(x.new)) {
        dist.j <- fields::rdist(x, matrix(x.new[j, ], nrow = 1))
        knn.j <- dist.j[order(dist.j)[k], ]
        tmp <- dist.j/(knn.j * adj)
        wgt <- stats::dnorm(tmp)
        proj <- wgt/sum(wgt)
        pred[j] <- sum(proj * y)
        trace <- trace + proj[j]
    }

    if (gcv == TRUE) {
        list(pred = pred, trace = trace)
    } else return(pred)
}
