## Assignment: Caching the Inverse of a Matrix
##      The following functions cache the inverse of a matrix, assuming the 
##      matrix supplied is always invertible.      

##---------------------------------------------------------------------------
##---------------------------------------------------------------------------

## `makeCacheMatrix`: Creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        mt <- NULL
        set <- function(y){
                x <<- y
                mt <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) mt <<- solveMatrix
        getInverse <- function() mt
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##---------------------------------------------------------------------------
## `cacheSolve`: Computes the inverse of the special "matrix" returned by 
##      `makeCacheMatrix` above. If the inverse has already been calculated 
##       (and the matrix has not changed), then `cacheSolve` retrieves the 
##       inverse from the cache.

cacheSolve <- function(x, ...) {
        mt <- x$getInverse()
        if(!is.null(mt)){
                message("getting cached data")
                return(mt)
        }
        data <- x$get()
        mt <- solve(data)
        x$setInverse(mt)
        mt      
}

##---------------------------------------------------------------------------
##---------------------------------------------------------------------------