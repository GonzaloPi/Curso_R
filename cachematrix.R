
## the first function creates a special "matrix" object that can cache its inverse.

makeMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

x = rbind(c(1, -1/4), c(-1/4, 1))
m = makeMatrix(x)
m$get()


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache
cacheinv <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
} 


## No cache in the first run
cacheinv(m)

## getting from the cache in the second run
cacheinv(m)
## getting cached data.

