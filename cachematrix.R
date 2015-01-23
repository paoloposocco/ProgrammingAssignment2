## These functions are used to calculate the inverse if a matrix.
## If the inverse has been already calculated, the function
## return the cached value avoiding spend time calculate
## the inverse again.

## This function return a list of 4 function with the aims
## of set and get the original matrix and set and get
## the reversed matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve)
        inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function control if the matrix "inverse" has already been
## calculated. If so, it returns the cached value. Otherwise
## it calculate the inverse matrix using the R function "solve".

cacheSolve <- function(x, ...) {
        inverse<- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse<- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
