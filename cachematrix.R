## Below functions allows to create a cache-enabled version of matrix and
## cache the matrix' inverse (result of solve function). This allows to cache
## a time consuming operation. For more details refer to functions descriptions.
##
## Usage Example:
## > cm <- makeCacheMatrix(matrix(1:4, ncol=2))
##
## > cm$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## > cacheSolve(cm)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(cm)
## getting cached data....
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cm$getSolve()
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

makeCacheMatrix <- function(x = matrix()) {
	# Creates cache-enabled matrix.
	#
	# Args:
	#   x: a matrix object
	#
	# Returns:
	#   An object with 4 functions:
	#   - get() returns data
	#   - set(y) changes data
	#   - getSolve() returns matrix' inverse
	#   - setSolve() sets the matrix' inverse
	
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setSolve <- function(solve) s <<- solve
	getSolve <- function() s
	list(set      = set,
		 get      = get,
		 setSolve = setSolve,
		 getSolve = getSolve)
}

cacheSolve <- function(x) {
	# Caches the inverse of a matrix created with makeCacheMatrix function.
	#
	# Notes:
	#  Function doesn't take /.../ argument as /solve/ shouldn't be passed
	#  more than the data to inverse the matrix.
	#
	# Args:
	#   x: matrix created with makeCacheMatrix function.
	#
	# Returns:
	#  /x/ matrix' inverse
	
	s <- x$getSolve()
	if(!is.null(s)) {
		message("getting cached data....")
		return(s)
	}
	data <- x$get()
	s <- solve(data)
	x$setSolve(s)
	s
}
