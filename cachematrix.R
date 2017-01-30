## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
