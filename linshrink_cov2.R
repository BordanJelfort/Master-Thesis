linshrink_cov2 <- function (X, k = 0) 
{
  X.tmp <- na.omit(X)
  n <- nrow(X.tmp)
  p <- ncol(X.tmp)
  cMeans <- colMeans(X, na.rm=T)
  if (k == 0) {
    X <- X.tmp - tcrossprod(rep(1, n), cMeans)
    k = 1
  }
  if (n > k) 
    effn <- n - k
  else stop("k must be strictly less than nrow(X)")
  S <- crossprod(X)/effn
  Ip <- diag(p)
  m <- sum(S * Ip)/p
  d2 <- sum((S - m * Ip)^2)/p
  b_bar2 <- 1/(p * effn^2) * sum(apply(X, 1, function(x) sum((tcrossprod(x) -
                                                                S)^2)))
  b2 <- min(d2, b_bar2)
  a2 <- d2 - b2
  return(b2/d2 * m * Ip + a2/d2 * S)
}