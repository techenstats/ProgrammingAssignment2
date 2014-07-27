## These functions are used to create a special object that
##  stores a matrix plus cache the inverse of the matrix.
##  Caching the inverse can provide time savings where
##  the inverse of the matrix is needed multiple times.

## Create a list of functions that define the matrix object

makeCacheMatrix <- function(x = matrix()) {

        xinv <- NULL
        
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(matinv) xinv <<- matinv
        
        getinv <- function() xinv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv) 
}


## This function returns the inverse of 'x'.  If the inverse
##  has already been computed, the cached value is returned;
##  otherwise, the inverse is calculated, cached, and returned.

cacheSolve <- function(x, ...) {

        xinv <- x$getinv()
        
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        
        mdata <- x$get()
        
        xinv <- solve(mdata, ...)
        
        x$setinv(xinv)
        
        xinv
}
