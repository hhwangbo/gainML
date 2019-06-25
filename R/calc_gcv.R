calc.gcv <- function(y, x, id.circ, kernel) {
    n <- nrow(x)
    q <- ncol(x)
    opt.ord <- n^(4/(4 + q))
    
    if (opt.ord <= 30) 
        k.cand <- c(3, 5, 7, 10, 15, 25) else if (opt.ord <= 50) 
        k.cand <- c(5, 7, 10, 15, 25, 40) else if (opt.ord <= 75) 
        k.cand <- c(5, 10, 20, 30, 50, 75) else if (opt.ord <= 100) 
        k.cand <- c(10, 20, 30, 50, 75, 100) else k.cand <- c(10, 25, 50, 100, 150, 200)
    
    if (kernel == "unif") {
        rss <- sapply(k.cand, function(k) FNN::knn.reg(train = x, y = y, k = k)$PRESS)
        gcv <- rss/(n * (1 - 1/k.cand)^2)
    } else {
        res <- lapply(k.cand, function(k) fit.akern(y = y, x = x, x.new = x, id.circ = id.circ, 
            k = k, gcv = TRUE))
        gcv <- sapply(1:length(k.cand), function(k) mean((y - res[[k]]$pred)^2)/((1 - 
            res[[k]]$trace/length(y))^2))
    }
    
    list(cand = k.cand, gcv = gcv)
}
