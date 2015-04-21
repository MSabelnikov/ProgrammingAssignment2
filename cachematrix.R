## makeCacheMatrix function sets matrix and caches its inverse
## cacheSolve gets the inversed matrix out of cache (in case it is in there)

## Caching inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmatrix <- function(y) {   ##Setting matrix
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) m <<- solve   ##Calculating inverse matrix
        getinverse <- function() m
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)   ##Returning a list of results
}


## Getting inversed matrix out of cache or calculating inverse

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.logical(m==solve(x))) {  ##Testing cached matrix if it is inverse of x
                message("getting cached matrix")
                return(m)
        }
        data <- x$getmatrix()
        m <- solve(data, ...)   ##Calculating inversed matrix if it is not in the cache
        x$setinverse(m)
        m
}
