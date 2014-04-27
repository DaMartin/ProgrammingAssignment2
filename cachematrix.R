## This pair of functions cache the inverse of a matrix.

## This function creates a list containing 4 functions to: set/get a matrix 'x', set/get the inverse of the matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL									 	#  Set the inverse of the matrix x given to NULL
    set <- function(GivenMat) {
        x <<- GivenMat							 	#  Set the matrix 'x' according to user's input
        inv <<- NULL								#  If there were a stored Inverse Matrix...
    }												#  ...it is better to cancel it, it is not valid anymore.
	 get <- function() x							#  Get the matrix 'x'
	 setInverse <- function(GivenInverse) inv <<- GivenInverse  #  Set the matrix 'x' to user's input
	 getInverse <- function() inv				    #  Get the inverse matrix from input
	 list(set = set, get = get, 
	     setInverse = setInverse,
	     getInverse = getInverse) 
}


## This function checks is the inverse of a matrix saved in the list 'x' 
## (originated by the previous function) was computed earlier, otherwise calculates it.
## The result is saved in the original x's cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {             		#	If the inverse was computed, or set by user, in x...
        message("getting cached data") 	
        return(inv)}             			#  ...just return that matrix, no computation needed!
    else {
        data <- x$get()             		#  otherwise, if there is no cache
        m <- solve(data, ...)       		#  compute it!
        x$setInverse(m)                     #  Now save the result back to x's cache
        m}                          		#  and return the result.
}
