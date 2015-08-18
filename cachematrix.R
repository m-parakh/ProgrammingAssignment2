## This script contains two functions that are used to create a
## special object that stores a matrix and caches its inverse.
## The functions assume that the matrix supplied in the argument
## is invertible.

## makeCacheMatrix starts with a null matrix argument and 
## returns a list containing functions to 
## set the value of the matrix, get the value of the matrix
## set the inverse of the matrix, get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	## initialize the inverse matrix
	inv <- NULL
	## the set function caches the value of the matrix supplied
	## and resets the inverse matrix to null
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	## the get function gets the value of the matrix
	get <- function() x
	## the setinverse function calculates and sets 
	## the value of the inverse matrix
	setinverse <- function(solve) inv
	## the getinverse function gets the inverse matrix
	getinverse <- function() inv
	## return the list containing the set, get, 
	## setinverse, and getinverse functions
	list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}


## cacheSolve function takes the list object returned
## by makeCacheMatrix as an argument. It gets the inverse
## from the cache if it exists. Otherwise, it calculates
## the inverse and caches it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
    	message("getting cached inverse")
    	return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
