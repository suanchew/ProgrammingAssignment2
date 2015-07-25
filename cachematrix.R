
## The following functions computes the inverse of a matrix and then caches it. This is beneficial since matrix inversion is compute intensive. So, whenever an inverse is required and is available in the cache, the cached inverse is returned. The R function - solve - is used to compute the inverse. 


## The function makeCacheMatrix() returns a "matrix" object that is essentially a list of functions: set(), get(), setinverse(), getinverse(). The set() and setinverse() functions use the <<- operator to cache input data and inversed results as matrix inversions are being computed. This allows the most recently computed inverse to be retrieved from the cache, thereby, achieving a "least burdensome" matrix inversion.


makeCacheMatrix <- function(x = matrix()) {
	
	m <- NULL
	
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	get <- function() x
	
	setinverse <- function(solve) m <<- solve
	
	getinverse <- function() m
	
	list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)

}

## The function cacheSolve() retrieves the cached inverse if it is available (non null) and returns it. Otherwise, it retrieves the cached input data and computes the inverse using the R function - solve. Whenever an inverse is computed, it is cached. This allows the most recently computed inverse to be retrieved from the cache, thereby achieving a "least burdensome" matrix inversion.

cacheSolve <- function(x, ...) {

	m <- x$getinverse()
	
	if (!is.null(m)){
		message ("getting cached data")
		return (m)
	}

	data <- x$get()
	
	m <- solve(data,...)
	
	x$setinverse(m)
	
	m

}




