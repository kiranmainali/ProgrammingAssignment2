## Put comments here that give an overall description of what your
## functions do

## Assign matrix to variable x, and initialize invMAt to NULL

makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
            set <- function(y) {                ## reset matrix
                    x <<- y                     ## reassign new matrix to x
                    invMat <<- NULL             ##reinitiate invMat to NULL
            }
            get <- function() x
            setInverse <- function(solveMatrix) invMat <<- solveMatrix
            getInverse <- function() invMat
            list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        invMat <- x$getInverse()
        if(!is.null(invMat)) {                  ## check the cahce of inverse
                message("getting cached data")
                return(invMat)                  ## return the old value for inverse matrix
        }
        data <- x$get()                         ##get the new matrix ifinverse is not calculated before
        invMat <- solve(data, ...)              ## calculate the inverse
        x$setInverse(invMat)                    ## assign the inverse matrix
        invMat                                  ## print the inverse matrix
}
