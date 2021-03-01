#' @import Matrix
#' @import methods
NULL

#' Sparse plus low rank matrix
#'
#' Eventually this class will subclass `Matrix` objects,
#' but for now this is a basic implementation that essentially
#' only supports singular value decomposition.
#'
#' To learn more about S4 classes, please see
#' <https://adv-r.hadley.nz/s4.html>.
#'
#' @slot sparse sparseMatrix.
#' @slot U Matrix.
#' @slot V Matrix.
#'
#' @examples
#'
#' set.seed(528491)
#'
#' n <- 50
#' m <- 40
#' k <- 3
#'
#' A <- rsparsematrix(n, m, 0.1)
#'
#' U <- Matrix(rnorm(n * k), nrow = n, ncol = k)
#' V <- Matrix(rnorm(m * k), nrow = m, ncol = k)
#'
#' # construct the matrix, which represents A + U %*% t(V)
#' X <- sparseLRMatrix(sparse = A, U = U, V = V)
#'
#' dim(X)
#'
#' s <- svds(X, 5)  # efficient
#'
setClass(
  Class = "sparseLRMatrix",
  # contains = "Matrix",
  slots = c(
    sparse = "sparseMatrix",
    U = "Matrix",
    V = "Matrix"
  ),
  prototype = list(
    sparse = as(Matrix(), "CsparseMatrix"),
    U = Matrix(),
    V = Matrix()
  )
)

#' Create a sparse plus low rank matrix
#'
#' @param sparse sparseMatrix.
#' @param U Matrix.
#' @param V Matrix.
#'
#' @return A [sparseLRMatrix-class] S4 object.
#'
#' @inherit sparseLRMatrix-class examples
#' @export
#'
sparseLRMatrix <- function(sparse, U, V) {
  methods::new(
    Class = "sparseLRMatrix",
    sparse = sparse,
    U = Matrix(U),
    V = Matrix(V)
  )
}

setValidity("sparseLRMatrix", function(object) {
  if (nrow(object@sparse) != nrow(object@U)) {
    return("@sparse and @U must have the same number of rows.")
  }

  if (ncol(object@sparse) != nrow(object@V)) {
    return("Number of columns in @sparse must equal number of rows in @V.")
  }

  if (ncol(object@U) != ncol(object@V)) {
    return("Number of columns in @U and @V must match.")
  }

  TRUE
})

#' Check the dimension of a sparseLRMatrix
#'
#' @param x A [sparseLRMatrix-class] object.
#'
#' @return Dimension of `x`.
#' @export
#' @inherit sparseLRMatrix-class examples
#'
setMethod("dim", "sparseLRMatrix", function(x) dim(x@sparse))

Ax <- function(x, A) {
  out <- A@sparse %*% x + A@U %*% Matrix::crossprod(A@V, x)
  drop(out)
}

Atx <- function(x, A) {
  out <- crossprod(A@sparse, x) + A@V %*% Matrix::crossprod(A@U, x)
  drop(out)
}

#' @export
RSpectra::svds

#' Truncated singular value decomposition of a matrix
#'
#' A thin wrapper around [RSpectra::svds()], please see more detailed
#' documentation there. In particular, this function leverages the
#' function interface.
#'
#' @param A Matrix to decompose.
#' @param k Number of singular values to estimate.
#' @param nu Number of left singular vectors to estimate.
#' @param nv Number of right singular vectors to estimate.
#' @param opts Passed to [RSpectra::svds()].
#' @param ... Passed to [RSpectra::svds()].
#'
#' @inherit RSpectra::svds return
#'
#' @importFrom RSpectra svds
#' @method svds sparseLRMatrix
#'
#' @export
#'
#' @examples
#'
#' set.seed(528491)
#'
#' n <- 50
#' m <- 40
#' k <- 3
#'
#' A <- rsparsematrix(n, m, 0.1)
#'
#' U <- Matrix(rnorm(n * k), nrow = n, ncol = k)
#' V <- Matrix(rnorm(m * k), nrow = m, ncol = k)
#'
#' X <- sparseLRMatrix(sparse = A, U = U, V = V)
#'
#' svds(X, 5)
#'
svds.sparseLRMatrix <- function(A, k, nu = k, nv = k, opts = list(), ...) {
  RSpectra::svds(
    Ax,
    k,
    nu = nu,
    nv = nv,
    opts = opts,
    Atrans = Atx,
    dim = dim(A),
    args = A,
    ...
  )
}

