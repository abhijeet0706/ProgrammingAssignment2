## This program provides the inverse for a given matrix. If the inverse 
## a matrix is already computed, its inverse is returned from cache, thus
## saving computation time. If the matrix is new, its inverse is stored 
## in the cache after computation.

## makeCacheMatrix() returns a special vector which is a list of functions
## which stores and retrieves information about the given matrix.

makeCacheMatrix <- function(x = matrix()) {
     invrs <- NULL
     
     setl <- function(y) {
          x <<- y
          invrs <<- NULL
     }
     
     getl <- function() x
     setinv <- function(inv) invrs <<- inv
     getinv <- function() invrs
     
     list(setl=setl,getl=getl,setinv=setinv,getinv=getinv)
}

## cacheSolve() returns the inverse of the given matrix. Its input is the
## list returned by makeCacheMatrix() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     invrs <- x$getinv()
     
     if(!is.null(invrs)) {
          message("Getting from cache")
          return(invrs)
     }
     
     nwmatrix <- x$getl()
     invrs <- solve(nwmatrix, ...)
     x$setinv(invrs)
     
     invrs
}
