## These functions will cache the inverse of a matrix

## This function creates a list object that will store the matrix provided,
## and will also store the inverse when it is calculated. This works in 
## combination with the cacheSolve function.
## Note that if a new matrix is stored in this object, it erases any value 
## that the variable 'inverse' had stored. This forces cacheSolve to 
## recalculate the inverse using the new matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inverse <<- solve
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function uses the list object created by makeCacheMatrix.
## If we have already calculated the inverse, retrieve it from the cached
## object; otherwise, calculate the inverse and store it in the cached object.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
