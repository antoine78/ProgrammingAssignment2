## This script provides a type of matrix object that allows
#  the inverse of the matrix to be computed only once and
#  cached for subsequent calls
## 

## 'makeCacheMatrix' builds the object with following attributes:
# - cmat and cinv are the matrix data and its inverse
# - 'get()' and 'set()' allows to get and set the matrix
# - 'getinv()' and 'setinv()' allows to get and set the inverse
# - 'csolve()' computes the inverse if not available, and caches it

makeCacheMatrix <- function(x = matrix()) {
# Create the data and methods for caching a matrix and its inverse
    cinv   <- NULL
    cmat   <- x
    set    <- function(y) {
        cmat <<- y
        cinv <<- NULL
    }
    get    <- function() cmat
    setinv <- function(inv) cinv <<- inv
    getinv <- function() cinv
    csolve <- function(...) {
        if(is.null(cinv)) 
            cinv <<- solve(cmat, ...)
        else
            message("getting cached inverse")  
        cinv
    }
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv,
         solve  = csolve)
}


## 'cacheSolve' computes the inverse if not yet available and
# chaches it for subsequent calls.

# Note that this function is a wrapper to the 'solve()' method
# of the object 'makeCacheMatrix' so that after defining such
# a cache matrix :
#       cx <- makeCacheMatrix(x)
# then the following are equivalent to compute/retrieve the inverse:
#       xinv <- cacheSolve(cx)
#       xinv <- cx$solve()
#

cacheSolve <- function(cx, ...) {
## Return a matrix that is the inverse of 'cx'
    cx$solve(...)
}
