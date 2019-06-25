#' Apply Period 1 Analysis
#'
#' Conducts period 1 analysis; selects the optimal set of variables that
#' minimizes a \emph{k}-fold CV error measure and establishes a machine learning
#' model that predicts power output of REF and CTR-b turbines by using period 1
#' data.
#'
#' @inheritParams quantify.gain
#' @param train A list containing \emph{k} datasets that will be used to train
#'   the machine learning model.
#' @param test A list containing \emph{k} datasets that will be used to test the
#'   machine learning model and calculate CV error measures.
#' @return The function returns a list containing period 1 analysis results as
#'   follows. \describe{ \item{\code{opt.cov}}{A character vector presenting the
#'   names of predictor variables chosen for the optimal set.}
#'   \item{\code{pred.REF}}{A list of \eqn{k} datasets each representing the
#'   \eqn{k}th fold's period 1 prediction for the REF turbine.}
#'   \item{\code{pred.CTR}}{A list of \eqn{k} datasets each representing the
#'   \eqn{k}th fold's period 1 prediction for the CTR-b turbine.}
#'   \item{\code{err.REF}}{A data frame containing \eqn{k}-fold CV based RMSE
#'   values and BIAS values for the REF turbine model (so \eqn{k} of them for
#'   both). The first column includes the RMSE values and the second column
#'   includes the BIAS values.} \item{\code{err.CTR}}{A data frame containing
#'   \eqn{k}-fold CV based RMSE values and BIAS values for the CTR-b turbine
#'   model. Similarly structured with \code{err.REF}.}
#'   \item{\code{biasCurve.REF}}{A \eqn{k} by \eqn{m} matrix describing the
#'   binned BIAS (technically speacking, `residuals' which are the negative
#'   BIAS) curve for the REF turbine model, where \eqn{m} is the number of power
#'   bins.} \item{\code{biasCurve.CTR}}{A \eqn{k} by \eqn{m} matrix describing
#'   the binned BIAS curve for the CTR-b turbine model.}}
#' @note \bold{VERY IMPORTANT!} \itemize{ \item Selecting the optimal set of
#'   variables will take a significant amount of time. For example, with a
#'   typical size of an annual dataset, the evaluation of one set of variables
#'   for a single fold testing may take about 20-40 minutes (from the authors'
#'   experience). \item To help understand the progress of the selection, some
#'   informative messages will be displayed while this function runs.}
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
#' p1.res$opt.cov #This provides the optimal set of variables.
#'
#' @export
#'
analyze.p1 <- function(train, test, ratedPW) {
    if (is.na(train)[1]) {
        list(opt.cov = NA, pred.REF = NA, pred.CTR = NA, rmse = NA, bias = NA)
    } else {
        cov.name <- names(train[[1]])
        id.elim <- sort(unique(c(grep("Time", cov.name), grep("yr", cov.name), grep("b$",
            cov.name), grep("fold", cov.name))))

        id.cov <- list(cov.name[-id.elim])

        thres <- Inf
        rmse <- c()
        bias <- c()
        pred <- c()
        best.cov <- c()

        message("Period 1 Analysis")
        message(" Finding the optimal set of variables...")

        repeat {
            rmse.tmp <- rep(list(c()), length(id.cov))
            bias.tmp <- rep(list(c()), length(id.cov))
            pred.tmp <- rep(list(c()), length(id.cov))

            # cat('Number of covariates:', length(id.cov[[1]]), '\n')

            for (j in 1:length(id.cov)) {
                rmse.tmp[[j]] <- rep(NA, length(train))
                bias.tmp[[j]] <- rep(NA, length(train))
                pred.tmp[[j]] <- rep(list(c()), length(train))

                # cat('Testing covariates:', id.cov[[j]], '\n') cat('0', sep = '')

                id.circ <- which(id.cov[[j]] %in% c("D", "hour"))

                for (i in 1:length(train)) {
                  dtr <- train[[i]]
                  dts <- test[[i]]

                  pred.val <- pred.akern(dtr$yr, as.matrix(dtr[, id.cov[[j]]]), as.matrix(dts[,
                    id.cov[[j]]]), id.circ = id.circ, k = "gcv", kernel = "gauss")

                  pred.tmp[[j]][[i]] <- dts[, c(id.cov[[j]], "yr")]
                  pred.tmp[[j]][[i]]$pred <- pred.val

                  rmse.tmp[[j]][i] <- sqrt(mean((dts$yr - pred.val)^2))
                  bias.tmp[[j]][i] <- (sum(pred.val) - sum(dts$yr))/sum(pred.val)

                  n.dots <- floor(10/length(train))
                  # cat(rep('.', n.dots), sep = '') if (i == length(train) & (n.dots *
                  # length(train) < 10)) cat(rep('.', (10 - n.dots * length(train))), sep = '') if
                  # (i == length(train)) cat('100%\n')
                }
                # cat('RMSE:', mean(rmse.tmp[[j]]), '\tBias:', mean(bias.tmp[[j]]), '\n')
            }

            score <- sapply(rmse.tmp, mean)
            id.min <- which(score == min(score))
            # cat('Best covariates for', length(id.cov[[1]]), 'variables:', id.cov[[id.min]],
            # '\n\n')

            if (score[id.min] < thres) {
                rmse <- cbind(rmse, rmse.tmp[[id.min]])
                bias <- cbind(bias, bias.tmp[[id.min]])
                thres <- score[id.min]
                pred <- pred.tmp[[id.min]]
                best.cov <- id.cov[[id.min]]

                if (length(best.cov) == 1) {
                  # cat('---------------------------------------\n')
                  message("  Optimal set of variables: ", best.cov)
                  # cat('---------------------------------------\n\n')
                  break
                }

                id.cov <- lapply(1:length(best.cov), function(p) {
                  return(best.cov[-p])
                })
            } else {
                # cat('---------------------------------------\n')
                message("  Optimal set of variables: ", best.cov)
                # cat('---------------------------------------\n\n')
                break
            }
        }

        message(" Estimating the baseline model (CTR-b)...")
        pred.CTR <- lapply(1:length(test), function(x) {
            id.circ <- which(best.cov %in% c("D", "hour"))
            pred <- pred.akern(train[[x]]$yb, as.matrix(train[[x]][, best.cov]),
                as.matrix(test[[x]][, best.cov]), id.circ = id.circ, k = "gcv", kernel = "gauss")
            return(cbind(test[[x]][, c(best.cov, "yb")], pred = pred))
        })

        err.REF <- data.frame(RMSE = rmse[, ncol(rmse)], BIAS = bias[, ncol(bias)])

        rmse.CTR <- sapply(pred.CTR, function(df) with(df, sqrt(mean((pred - yb)^2))))
        bias.CTR <- sapply(pred.CTR, function(df) with(df, (sum(pred) - sum(yb))/sum(pred)))
        err.CTR <- data.frame(RMSE = rmse.CTR, BIAS = bias.CTR)

        bin.ref <- c(seq(0, ratedPW - 100, 100), ratedPW + 100)
        bins <- lapply(pred.CTR, function(df) .bincode(df$yb, bin.ref))

        bin.biasREF <- t(sapply(1:(length(pred)), function(x) get.biasCurve(pred[[x]],
            bins[[x]], bin.ref, "yr")))
        bin.biasCTR <- t(sapply(1:(length(pred.CTR)), function(x) get.biasCurve(pred.CTR[[x]],
            bins[[x]], bin.ref, "yb")))

        list(opt.cov = best.cov, pred.REF = pred, pred.CTR = pred.CTR, err.REF = err.REF,
            err.CTR = err.CTR, biasCurve.REF = bin.biasREF, biasCurve.CTR = bin.biasCTR)
    }
}

get.biasCurve <- function(per1, bins.p1, bin.ref, turb.type) {
    sapply(1:(length(bin.ref) - 1), function(bin) {
        id.bin <- which(bins.p1 == bin)
        return(stats::median(per1[id.bin, ][[turb.type]] - per1[id.bin, "pred"]))
    })
}

