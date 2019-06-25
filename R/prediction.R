#' @importFrom FNN knn.reg
pred.akern <- function(y, x, x.new = x, id.circ, k, kernel = "unif") {
    df.norm <- norm.data(x, x.new, id.circ)
    norm.x <- df.norm$x
    norm.xnew <- df.norm$x.new
    
    if (is.numeric(k)) 
        k.eval <- k else if (k == "gcv") 
        k.eval <- est.bw(y, norm.x, id.circ, kernel)
    
    
    if (kernel == "unif") 
        pred <- FNN::knn.reg(train = norm.x, test = norm.xnew, y = y, k = k.eval)$pred else pred <- fit.akern(y, norm.x, norm.xnew, id.circ = id.circ, k = k.eval, gcv = FALSE)
    
    return(pred)
}
