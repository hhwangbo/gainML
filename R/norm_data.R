norm.data <- function(x, x.new, id.circ) {
    min.cov <- apply(x, 2, min)
    max.cov <- apply(x, 2, max)
    norm.x <- sapply(1:ncol(x), function(col) (x[, col] - min.cov[col])/(max.cov[col] - 
        min.cov[col]))
    norm.xnew <- sapply(1:ncol(x), function(col) (x.new[, col] - min.cov[col])/(max.cov[col] - 
        min.cov[col]))
    list(x = norm.x, x.new = norm.xnew)
}
