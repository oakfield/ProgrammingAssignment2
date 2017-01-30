## Put comments here that give an overall description of what your
## functions do
##
## Cache a maxtrix using makeCacheMatrix. Example:
## > m <- matrix(c(1, 2, 3, 4), 2, 2)
## > x <- makeCacheMatrix(m)
##
## Use the cached matrix by sending the return value
## of makeCacheMatrix to cacheSolve. Example:
## > cacheSolve(x)

## Write a short comment describing this function
##
## makeCacheMatrix takes a matrix and returns a vector
## of functions that will store and retrieve both it
## and its solution.
makeCacheMatrix <- function(cachedMatrix = matrix()) {
	solvedMatrix <- NULL
	set <- function(newCachedMatrix) {
		cachedMatrix <<- newCachedMatrix
		solvedMatrix <<- NULL
	}
	get <- function() cachedMatrix
	setSolvedMatrix <- function(newSolvedMatrix) solvedMatrix <<- newSolvedMatrix
	getSolvedMatrix <- function() solvedMatrix
	list(set = set,
		get = get,
		setSolvedMatrix = setSolvedMatrix,
		getSolvedMatrix = getSolvedMatrix)
}


## Write a short comment describing this function
##
## cacheSolve takes a makeCacheMatrix vector and returns
## the solution to the cached matrix. It will not re-
## compute the solution if already done.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	solvedMatrix <- x$getSolvedMatrix()

	if (!is.null(solvedMatrix)) {
		message("Getting cached solution0")
		return(solvedMatrix)
	} else {
		data <- x$get()
		solvedMatrix <- solve(data)
		x$setSolvedMatrix(solvedMatrix)
		solvedMatrix
	}
}
