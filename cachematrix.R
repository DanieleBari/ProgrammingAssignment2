## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## the makeCacheMatrix function creates an object which is a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
			x <<- y	# super assignment
			inverse <<- NULL # super assignment
	}
	get <- function() x
	set_inverse <- function(inverse_matrix) inverse <<- inverse_matrix # super assignment
	get_inverse <- function() inverse
	list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

## Write a short comment describing this function
## The cacheSolve function calculates the inverse of a matrix contained within the makeCacheMatrix
## object. If the inverse matrix has been already calculated, it skips the computation saving 
## computational time. Otherwise, it calculates the inverse of the matrix and set the value of the inverse
## in the cache by means of the set_inverse function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	im <- x$get_inverse() # im is a temp variable to store the actual inverse of the matrix
	if (!is.null(im)){
		message("getting cached inverse matrix")
		return(im) # if the inverse of the matrix has been already calculated then the function returns it and 
		# doesn't calculate it again
	}
	# otherwise the function calculates the inverse of the function and returns it
	x$set_inverse(solve(x$get()))
	x$get_inverse()
}