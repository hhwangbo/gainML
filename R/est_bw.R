est.bw <- function(y, norm.x, id.circ, kernel) {
    gcvs <- calc.gcv(y, norm.x, id.circ, kernel)
    id.cand <- which(gcvs$gcv == min(gcvs$gcv, na.rm = T))
    k.eval <- gcvs$cand[id.cand]
    
    return(k.eval)
}
