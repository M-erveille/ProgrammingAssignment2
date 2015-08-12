## Put comments here that give an overall description of what your
## functions do

### Caching the Inverse of a Matrix (using R) -- overall description:
### The following pair of functions is to calculate (and cache) the inverse of a matrix
### rather than compute it repeatedly. For a very big size matrix, it may take too long
### to compute its inverse, especially if it has to be computed repeatedly (e.g. in a loop).
### If the contents of a matrix are not changing, it may make sense to cache the value of
### the inverse so that when we need it again, it can be looked up in the cache rather than recomputed.



## Write a short comment describing this function

### The first function, makeCacheMatrix creates a special "matrix", which
### is really a list containing a function to
### 1. set the value of the matrix
### 2. get the value of the matrix
### 3. set the value of the inverse matrix
### 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(Inverse) m <<- Inverse
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

### The second function, cacheSolve calculates the inverse matrix of the 
### special "matrix" created with the above function. However, it first 
### checks to see if the inverse has already been calculated. If so, it
### gets the inverse matrix from the cache and skips the computation.
### Otherwise, it calculates the inverse matrix of the data and sets the
### value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
