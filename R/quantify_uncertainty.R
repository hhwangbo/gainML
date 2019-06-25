#' Construct a Confidence Interval of the Gain Estimate
#'
#' Estimates gain and its confidence interval at a given level of confidence by
#' using bootstrap.
#'
#' @inheritParams analyze.gain
#' @inheritParams analyze.p2
#' @param n.rep An integer describing the total number of replications when
#'   applying bootstrap. This number determines the confidence level; for
#'   example, if \code{n.rep} is set to 10, this function will provide an 80\%
#'   confidence interval.
#' @param pred.return A logical value whether to return the full prediction
#'   results; see \bold{Details} below. The default value is \code{FALSE}.
#' @return The function returns a list of \code{n.rep} replication objects
#'   (lists) each of which includes the following. \describe{
#'   \item{\code{gain.res}}{A list containing gain quantification results; see
#'   \code{\link{quantify.gain}} for the details.} \item{\code{p1.pred}}{A list
#'   containing period 1 prediction results. \itemize{ \item{\code{pred.REF}: A
#'   list of \eqn{k} datasets each representing the \eqn{k}th fold's period 1
#'   prediction for the REF turbine.} \item{\code{pred.CTR}: A list of \eqn{k}
#'   datasets each representing the \eqn{k}th fold's period 1 prediction for the
#'   CTR-b turbine.}}} \item{\code{p2.pred}}{A list containing period 2
#'   prediction results; see \code{\link{analyze.p2}} for the details.} }
#' @details For each replication, this function will make a \eqn{k} of period 1
#'   predictions for each of REF and CTR-b turbine models and an additional
#'   period 2 prediction for each model. This results in \eqn{2 \times (k + 1)}
#'   predictions for each replication. With \code{n.rep} replications, there
#'   will be \eqn{n.rep \times 2 \times (k + 1)} predictions in total.
#'
#'   One can avoid storing such many datasets in the memory by setting
#'   \code{pred.return} to \code{FALSE}; which is the default setting.
#' @references H. Hwangbo, Y. Ding, and D. Cabezon, 'Machine Learning Based
#'   Analysis and Quantification of Potential Power Gain from Passive Device
#'   Installation,' arXiv:1906.05776 [stat.AP], Jun. 2019.
#'   \url{https://arxiv.org/abs/1906.05776}.
#' @examples
#' df.ref <- with(wtg, data.frame(time = time, turb.id = 1, wind.dir = D,
#'  power = y, air.dens = rho))
#' df.ctrb <- with(wtg, data.frame(time = time, turb.id = 2, wind.spd = V,
#'  power = y))
#' df.ctrn <- df.ctrb
#' df.ctrn$turb.id <- 3
#'
#' opt.cov = c('D','density','Vn','hour')
#' n.rep = 2 # just for illustration; a user may use at leat 10 for this.
#'
#' res <- bootstrap.gain(df.ref, df.ctrb, df.ctrn, opt.cov = opt.cov, n.rep = n.rep,
#'  p1.beg = '2014-10-24', p1.end = '2014-10-25', p2.beg = '2014-10-25',
#'  p2.end = '2014-10-26', ratedPW = 1000, AEP = 300000, pw.freq = pw.freq,
#'  k.fold = 2)
#'
#' length(res) #2
#' sapply(res, function(ls) ls$gain.res$gainCurve) #This provides 2 gain curves.
#' sapply(res, function(ls) ls$gain.res$gain) #This provides 2 gain values.
#'
#' @export
#'
bootstrap.gain <- function(df1, df2, df3, opt.cov, n.rep, p1.beg, p1.end, p2.beg, 
    p2.end, ratedPW, AEP, pw.freq, freq.id = 3, time.format = "%Y-%m-%d %H:%M:%S", 
    k.fold = 5, col.time = 1, col.turb = 2, free.sec = NULL, neg.power = FALSE, pred.return = FALSE) {
    res <- rep(list(c()), n.rep)
    
    for (i in 1:n.rep) {
        message("Bootstrap Replication: ", i)
        # Prepare data
        data <- arrange.data(df1, df2, df3, p1.beg, p1.end, p2.beg, p2.end, time.format, 
            k.fold, col.time, col.turb, bootstrap = i, free.sec, neg.power)
        
        if (is.na(data$train)[1]) {
            gain.res <- list(gainCurve = NA, gain = NA)
            res[[i]] <- gain.res
        } else {
            id.circ <- which(opt.cov %in% c("D", "hour"))
            
            # Period 1 Analysis
            message("Period 1 Prediction")
            message(" Period 1 Prediction - REF Model")
            n.dots <- floor(10/k.fold)
            
            # message('0')
            pred.REF <- lapply(1:k.fold, function(x) {
                pred <- pred.akern(data$train[[x]]$yr, as.matrix(data$train[[x]][, 
                  opt.cov]), as.matrix(data$test[[x]][, opt.cov]), id.circ, k = "gcv", 
                  kernel = "gauss")
                
                # message(rep('.', n.dots), sep = '') if (x == k.fold & (n.dots * k.fold < 10))
                # message(rep('.', (10 - n.dots * k.fold)), sep = '') if (x == k.fold)
                # message('100%\n')
                
                return(cbind(data$test[[x]][, c(opt.cov, "yr")], pred = pred))
            })
            
            message(" Period 1 Prediction - CTR-b Model")
            # message('0')
            
            pred.CTR <- lapply(1:k.fold, function(x) {
                pred <- pred.akern(data$train[[x]]$yb, as.matrix(data$train[[x]][, 
                  opt.cov]), as.matrix(data$test[[x]][, opt.cov]), id.circ, k = "gcv", 
                  kernel = "gauss")
                
                # message(rep('.', n.dots), sep = '') if (x == k.fold & (n.dots * k.fold < 10))
                # message(rep('.', (10 - n.dots * k.fold)), sep = '') if (x == k.fold)
                # message('100%\n')
                
                return(cbind(data$test[[x]][, c(opt.cov, "yb")], pred = pred))
            })
            
            bin.ref <- c(seq(0, ratedPW - 100, 100), ratedPW + 100)
            bins <- lapply(pred.CTR, function(df) .bincode(df$yb, bin.ref))
            
            bin.biasREF <- t(sapply(1:k.fold, function(x) get.biasCurve(pred.REF[[x]], 
                bins[[x]], bin.ref, "yr")))
            bin.biasCTR <- t(sapply(1:k.fold, function(x) get.biasCurve(pred.CTR[[x]], 
                bins[[x]], bin.ref, "yb")))
            
            p1.res <- list(opt.cov = opt.cov, pred.REF = pred.REF, pred.CTR = pred.CTR, 
                biasCurve.REF = bin.biasREF, biasCurve.CTR = bin.biasCTR)
            
            p2.res <- analyze.p2(data$per1, data$per2, p1.res$opt.cov)
            
            gain.res <- quantify.gain(p1.res, p2.res, ratedPW = ratedPW, AEP = AEP, 
                pw.freq = pw.freq)
            
            p1.res <- p1.res[2:3]
            
            if (pred.return) 
                res[[i]] <- list(gain.res = gain.res, p1.pred = p1.res, p2.pred = p2.res) else res[[i]] <- list(gain.res = gain.res)
        }
    }
    
    return(res)
}
