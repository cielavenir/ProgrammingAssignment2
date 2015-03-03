## makeCacheMatrix() creates an environment (assigned to my_cache here).
## You call my_cache$set(matrix) then cacheSolve(my_cache)
## to store the inversed matrix of (matrix) into my_cache.

## Returns an environment to retain cached inverse.

makeCacheMatrix <- function(x = matrix()){
	m <- NULL
	## Setting matrix clears cached inverse
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## if x$get()'s inverse is cached, returns it. otherwise, the inverse is calculated and cached.

cacheSolve <- function(x, ...){
	## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
