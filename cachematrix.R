## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix and cacheSolve is a pair of functions that could save 
## to compute again the inverse of a matrix which inverse has already been computed.
## Computing an inverse of a matrix can be time consuming. Hence it ia advantageous 
## to save the result.
## The function makeCacheMatrix stores and retrives the matrix and its inverse.
## The function cacheSolve computes the inverse of the matrix.
##
##
# Usage:
# > M <- matrix(c(1,2,3,4),2,2)   # Create a matrix
# > NN <- makeCacheMatrix()       # Create an object where to store the matrix and its inverse
# > NN$set(M)                     # Store the matrix in the object
# > NN$get()                      # Retrieve the matrix
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > NN$getinverse()               # Retrieving the inverse of a matrix not yet stored
# NULL                            #         returns a NULL
# > cacheSolve(NN)                # Compute the inverse of the matrix, 
#      [,1] [,2]                  #         it automatically sotores it in the object
# [1,]   -2  1.5
# [2,]    1 -0.5
# > NN$getinverse()               # Retrieving the inverse matrix form the object 
#      [,1] [,2]                  #         now returns something meaningfull
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(NN)                # when computing, for the second time, the inverse of the 
# getting cached data             #         same matrix, we get it straight form the cache.
#      [,1] [,2]                  
# [1,]   -2  1.5
# [2,]    1 -0.5

## The function makeCacheMatrix stores and retrieves the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    
    set <- function(y) {
    	# the makeCacheMatrix$set sets a matrix in cache
        a_matrix <<- y
        inverse_matrix <<- NULL

    }
    
    get <- function() a_matrix
    	# the makeCacheMatrix$get function returns a matrix from cache
   
    setinverse <-  function(the_inverse_matrix) inverse_matrix <<- the_inverse_matrix
    	# the makeCacheMatrix$setinverse sets in cache the inverse of a matrix
    
    getinverse <- function() inverse_matrix
    	# the makeCacheMatrix$getinverse returns from cache the inverse of a matrix

	# the makeCacheMatrix function returns a list of 4 (sub)functions	
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the matrix.
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    # 'x' is a composite object built up by a call to makeCacheMatrix()

    inverse_matrix <- x$getinverse()  # try to get the inverse of the matrix
    if(!is.null(inverse_matrix)) {    # the inverse of the matrix is known and stored...
        message("getting cached data")
        return(inverse_matrix)        # returned it !
    }

    # if no inverse matrix is stored in x 
    the_matrix_elements <- x$get()    # get the matrix elements 
    inverse_matrix <- solve(the_matrix_elements, ...)  # compute its inverse
    x$setinverse(inverse_matrix)                       # stores this computed inverse matrix 
    inverse_matrix                                     # and returned it
}