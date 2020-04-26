## Below are two functions that are used to create a special object that
## stores a matrix and caches it's inverse.

## cache matrix is a special "matrix, which is really a list containing
## a function to set the value of the matrix, get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


##  cacheSolve computes the inverse of a special matrix created by makeCacheMatrix()
## if the inverse has been cacluated it skips calculation

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        print("getting cached data ...")
        return(x$getinv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
