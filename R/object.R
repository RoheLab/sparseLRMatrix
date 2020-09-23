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
#' @return
#' @export
#'
#' @examples
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

# the helper
sparseLRMatrix <- function(sparse, U, V) {
  new(
    Class = "sparseLRMatrix",
    sparse = A,
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

  TRUE
})

setMethod("dim", "sparseLRMatrix", function(x) dim(x@sparse))

Ax <- function(x, A) {
  A@sparse %*% x + A@U %*% crossprod(A@V, x)
}

Atx <- function(x, A) {
  crossprod(A@sparse, x) + A@V %*% crossprod(A@U, x)
}


#' @inherit RSpectra::svds title params return
#' @method svds sparseLRMatrix
#' @export
#'
#' @examples
#'
#' library(RSpectra)
#'
#' set.seed(528491)
#'
#' n <- 50
#' k <- 3
#'
#' A <- rsparsematrix(n, n, 0.1)
#'
#' U <- Matrix(rnorm(n * k), nrow = n, ncol = k)
#' V <- Matrix(rnorm(n * k), nrow = n, ncol = k)
#'
#' X <- sparseLRMatrix(sparse = A, U = U, V = V)
#'
#' svds(X, 5)
#'
svds.sparseLRMatrix <- function(A, k, nu = k, nv = k, opts = list(), ...) {
  svds(
    Ax,
    k,
    nu = nu,
    nv = nv,
    opts = opts,
    Atrans = Atx,
    dim = dim(A),
    args = A
  )
}

