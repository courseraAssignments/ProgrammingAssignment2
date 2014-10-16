## The aim of the functions in this script is to invert a matrix,
## but using cached results from previous executions

## makeCacheMatrix is a helper function for cacheSolve so that 
## it can store and read results of matrix inversion from cache 

makeCacheMatrix <- function(x = matrix()) {
#this is a test
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix, but first looks
## up the cache whether it was previously calculated

cacheSolve <- function(x, ...) {
    m <- x$getinverse() 
    if(!is.null(m)) {                   #checking whether the result is already in the cache
        message("getting cached data")  # print message in case of cached result
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)               # calculate inverse
    x$setinverse(m)
    m
}
