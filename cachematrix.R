## Put comments here that give an overall description of what your
## functions do

# Usage:
# > M <- matrix(c(1,2,3,4),2,2)
# > NN <- makeCacheMatrix()
# > NN$set(M)
# > NN$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > NN$getinverse()
# NULL
# > cacheSolve(NN)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > NN$getinverse()
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(NN)
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(y) {
        a_matrix <<- y
        inverse_matrix <<- NULL

    }
    get <- function() a_matrix
    setinverse <-  function(the_inverse_matrix) inverse_matrix <<- the_inverse_matrix
    getinverse <- function() inverse_matrix
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## to be precise, 'x' is a composite object built up by a call to makeCacheMatrix()
    inverse_matrix <- x$getinverse()
    if(!is.null(inverse_matrix)) {
        message("getting cached data")
        return(inverse_matrix)
    }
    the_matrix_elements <- x$get()
    inverse_matrix <- solve(the_matrix_elements, ...)
    x$setinverse(inverse_matrix)
    inverse_matrix
}