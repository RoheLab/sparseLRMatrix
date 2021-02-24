test_that("singular values match", {

  set.seed(528491)

  n <- 50
  m <- 40
  k <- 3

  A <- rsparsematrix(n, m, 0.1)

  U <- Matrix(rnorm(n * k), nrow = n, ncol = k)
  V <- Matrix(rnorm(m * k), nrow = m, ncol = k)

  # construct the matrix, which represents A + U %*% t(V)
  X <- sparseLRMatrix(sparse = A, U = U, V = V)

  s <- svds(X, 5)  # efficient

  Y <- A + tcrossprod(U, V)
  s2 <- svds(Y, 5)  # inefficient, but same calculation

  expect_equal(s, s2)
})
