#' Quantify Gain Based on Period 1 and Period 2 Prediction
#'
#' Calculates effect curve, offset curve, and gain curve, and quantifies gain by
#' using both period 1 and period 2 prediction results.
#'
#' @importFrom stats median
#'
#' @param p1.res A list containing the period 1 analysis results.
#' @param p2.res A list containing the period 2 prediction results.
#' @param ratedPW A kW value that describes the (common) rated power of the
#'   selected turbines (REF and CTR-b).
#' @param AEP A kWh value describing the annual energy production from a single
#'   turbine.
#' @param pw.freq A matrix or a dataframe that includes power output bins and
#'   corresponding frequency in terms of the accumulated hours during an annual
#'   period.
#' @param freq.id An integer indicating the column number of \code{pw.freq} that
#'   describes the frequency of power bins in terms of the accumulated hours
#'   during an annual period. By default, this parameter is set to 3.
#' @return The function returns a list containing the following. \describe{
#'   \item{\code{effectCurve}}{A vector of length \eqn{m} illustrating REF
#'   turbine's power output difference between period 1 and 2, where \eqn{m} is
#'   the number of power bins.} \item{\code{offsetCurve}}{A vector of length
#'   \eqn{m} illustrating CTR-b turbine's power output difference between period
#'   1 and 2.} \item{\code{gainCurve}}{A vector of length \eqn{m} illustrating
#'   the bin-wise gain. Equivalent to \code{effCurve - offCurve}.}
#'   \item{\code{gain}}{A scalar representing the final gain after offset
#'   adjustment (derived from \code{gainCurve}).} \item{\code{effect}}{A scalar
#'   representing the initial effect without offset correction (derived from
#'   \code{effCurve}).} \item{\code{offset}}{A scalar representing the offset
#'   value for the final gain quantification (derived from \code{offCurve}).}}
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
#' data <- arrange.data(df.ref, df.ctrb, df.ctrn, p1.beg = '2014-10-24',
#'  p1.end = '2014-10-25', p2.beg = '2014-10-25', p2.end = '2014-10-26',
#'  k.fold = 2)
#'
#' p1.res <- analyze.p1(data$train, data$test, ratedPW = 1000)
#' p2.res <- analyze.p2(data$per1, data$per2, p1.res$opt.cov)
#'
#' res <- quantify.gain(p1.res, p2.res, ratedPW = 1000, AEP = 300000, pw.freq = pw.freq)
#'
#' res$effect - res$offset #This should be equivalent to the final gain below.
#' res$gain
#'
#' res$gainCurve #This shows the bin-wise gain (after offset adjustment).
#'
#' @export
#'
quantify.gain <- function(p1.res, p2.res, ratedPW, AEP, pw.freq, freq.id = 3) {
    if (is.na(p1.res$opt.cov)[1]) {
        list(biasCurve.REF = NA, biasCurve.CTR = NA, effCurve = NA, offCurve = NA, 
            gainCurve = NA, gain = NA, gain.init = NA, offset = NA)
    } else {
        bin.ref <- c(seq(0, ratedPW - 100, 100), ratedPW + 100)
        
        bins.p2 <- .bincode(p2.res$pred.CTR$yb, bin.ref)
        
        del.REF <- apply(p1.res$biasCurve.REF, 2, mean)
        del.CTR <- apply(p1.res$biasCurve.CTR, 2, mean)
        
        del2.REF <- sapply(1:(length(bin.ref) - 1), function(x) {
            id.bin <- which(bins.p2 == x)
            return(c(with(p2.res$pred.REF[id.bin, ], stats::median(yr - pred))))
        })
        
        del2.CTR <- sapply(1:(length(bin.ref) - 1), function(x) {
            id.bin <- which(bins.p2 == x)
            return(c(with(p2.res$pred.CTR[id.bin, ], stats::median(yb - pred))))
        })
        
        alpha.SoW <- del2.REF - del.REF
        alpha.offset <- del2.CTR - del.CTR
        alpha <- alpha.SoW - alpha.offset
        
        gain.SoW <- sum(alpha.SoW * pw.freq[, freq.id])/AEP
        gain.offset <- sum(alpha.offset * pw.freq[, freq.id])/AEP
        gain <- sum(alpha * pw.freq[, freq.id])/AEP
        
        list(effectCurve = alpha.SoW, offsetCurve = alpha.offset, gainCurve = alpha, 
            gain = gain, effect = gain.SoW, offset = gain.offset)
    }
}

