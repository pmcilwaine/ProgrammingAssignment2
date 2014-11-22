# Run program as
# m (some matrix) e.g. m <- matrix(rnorm(1000000), 1000)
# mat <- makeCacheMatrix(m)
# inv <- cacheSolve(mat)
# inv is the inverse matrix

# this caches the matrix and inverse matrix for faster processing
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # set the new matrix, set the inverse to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get the current matrix (non inversed form)
    get <- function() {
        x
    }
    
    # set the inverse matrix
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    
    # get the current inverse matrix, this could be NULL
    getInverse <- function() {
        inv
    }
    
    # list of available inner functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# this function solves the inverse of a matrix. It requires that 
# a cacheMatrix is used so the inverse (an expensive operation)
# can be retrieved quickly.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        print("getting cached data")
        return(inv)
    }
    
    # retrieve the matrix and then get inverse by solving
    m <- x$get()
    inv <- solve(m)
    
    # update the cached matrix with its inverse
    x$setInverse(inv)
    inv
}
