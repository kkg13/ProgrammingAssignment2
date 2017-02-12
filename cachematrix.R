## Matrix inversion can be a costly computation.  The two functions below
## use scoping inside of functions to calculate the inverse of an invertible
## matrix and cache the result so that it can be re-used instead of 
## re-calculated.

## makeCacheMatrix takes a matrix as an argument, then creates a matrix
## object that can calculate the inverse of the matrix and cache the 
## value for later use.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setCache <- function(solve) m <<- solve
    getCache <- function() m
    list(set = set, get = get,
         setCache = setCache,
         getCache = getCache)
}


## cacheSolve uses the objects created by the makeCacheMatrix function to
## check if the inverse of a matrix has already been calculated for the matrix
## and retrieve that value.  If not, it calculates the inverse matrix.

cacheSolve <- function(x, ...) {
    ## Check if inverse matrix exists and return cached data if it does
    m <- x$getCache()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## if matrix has not been cached, compute the inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setCache(m)
    m
}
