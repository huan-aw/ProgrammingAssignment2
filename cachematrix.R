## This pair of functions created a special matrix, whose inverse could be
# saved in cache. Any matrix created by the first function could be used in
# the second function to either calculated the inverse or get the cached
# inverse of the matrix. This could save time when the matrix is very big.


## The first function, makeVector creates a special "matrix", 
# which is really a list containing a function to
# 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## The following function calculates the inverse of the special "matrix" ,
# created with the above function. It could take out the cached calculation
# if the inverse of the matrix was already calculated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}

set.seed(1)
mat <- makeCacheMatrix(matrix(rnorm(9),nrow = 3))
cacheSolve(mat)
