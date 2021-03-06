## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) i <<- solve
        invmatrix <- function() i
        list(set = set, get = get, setmatrix = setmatrix, invmatrix = invmatrix)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'i'
        i <- x$invmatrix()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
                }
        i <- solve(x$get())
        x$setmatrix(i)
        i
}




