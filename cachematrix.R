## makeCacheMatrix declares the main variable inverse.of.matrix and then creates four functions which take
## a matrix x as input (defaults to a simple 2-by-2 matrix). $set resets the input x and inverse.of.matrix;
## $get simply returns the current input matrix. $setinverse.of.matrix and $getinverse.of.matrix are similar
## but they set and return a new value for inverse.of.matrix.

makeCacheMatrix <- function(x = matrix(c(1,2,2,1),ncol=2)) {
        inverse.of.matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse.of.matrix <<- NULL
        }
        get <- function() x
        setinverse.of.matrix <- function(solved.inverse.of.matrix) inverse.of.matrix <<- solved.inverse.of.matrix
        getinverse.of.matrix <- function() inverse.of.matrix
        list(set = set, get = get,
             setinverse.of.matrix = setinverse.of.matrix,
             getinverse.of.matrix = getinverse.of.matrix)
}

## cacheSolve is designed to take makeCacheMatrix as its input. It calls functions defined in the latter in order
## to check to see if there is already a cached value for inverse.of.matrix and, if so, return it. If there is not,
## it calls solve on the input matrix and then sets that as the cached value.

cacheSolve <- function(x, ...) {
        
        inverse.of.matrix <- x$getinverse.of.matrix()
        if(!is.null(inverse.of.matrix)) {
                message("getting cached data")
                return(inverse.of.matrix)
        }
        data <- x$get()
        inverse.of.matrix <- solve(data, ...)
        x$setinverse.of.matrix(inverse.of.matrix)
        inverse.of.matrix
}