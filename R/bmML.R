#' @title Brownian motion maximum likelihood estimation
#'
#' @description Fits the parameters of a Brownian motion model to phylogenetic
#' and trait data
#'
#' @details stub
#'
#' @param phy a phylogeny object of class \code{phylo} from package \code{ape}
#' @param x a numeric vector of traits, length equal to number of tips in
#' \code{phy}; order of trait values is assumed (and not checked) to be same as
#' order of tips in \code{phy}
#'
#' @return a named list giving the MLE of the root trait (\code{x0}), BM
#' variance (\code{s2}), and variance-covariance matrix (\code{Smat})
#'
#' @export

bmMLE <- function(phy, x) {
    # calculate VCV matrix and inverse
    S <- ape::vcv(phy, model = 'Brownian', corr = FALSE)
    Sinv <- solve(S)

    # a vector of 1s for matrix math below
    one <- rep(1, nrow(Sinv))

    # MLE of trait value at root
    x0 <- as.vector(solve(one %*% Sinv %*% one) %*% (one %*% Sinv %*% x))

    # MLE of BM variance
    s2 <- as.vector(((x - x0) %*% Sinv %*% (x - x0)) / nrow(Sinv))

    return(list(x0 = x0, s2 = s2, Smat = s2 * S))
}

# n <- 10
# tre <- rphylo(n, 1, 0.8)
# trt <- rTraitCont(tre, sigma = sqrt(2), root.value = 1)
#
# foo <- bmMLE(tre, trt)
#
# boo <- mvtnorm::rmvnorm(100, mean = rep(foo$x0, n), sigma = foo$Smat,
#                         method = 'chol')
#
#
# plot(simpECDF(trt), type = 'n')
# for(i in 1:100) {
#     lines(simpECDF(boo[i, ]), col = gray(0, 0.5))
# }
# points(simpECDF(trt), col = 'red')
#
#
# mvtnorm::dmvnorm(trt, mean = rep(foo$x0, n), sigma = foo$Smat, log = TRUE)
