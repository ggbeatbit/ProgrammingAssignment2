## Computes and cache inverse of a square invertible matrix

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated, then retrieve the 
## inverse from the cache. 
cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("Datum from cache")
		return(i)
	}
	i <- solve(x$get())
	x$setinverse(i)
	i
}
