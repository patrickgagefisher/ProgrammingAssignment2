## These two functions create a system for calculating and storing 
##the inverse of matrices so that they only have to be calculated once.

## makeCacheMatrix calculates the inverse of a matrix and sets it 
##to an empty container variable which will remain cached for future use.

makeCacheMatrix <- function(x = matrix(c())) {
	m <- NULL ## creates an empty matrix
	set <- function(y) { 
		x <<- y ## creates a cached matrix
		m <<- NULL ## creates a cached inverse of the matrix
	}

	get <- function() x
	setinverse <- function(cachedinverse) {
		m <<- cachedinverse ## gets the inverse of the matrix and sets it as the cached object m
	}

	getinverse <- solve (x) ## creats the inverse of the matrix
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function checks the m container object for a precalculated inverse
## If it is empty an inverse is calculated and stored in the cached object

cacheSolve <- function(x, ...) {
        m <- x$getinverse() ## gets the inverse of the matrix set in our cached object
        if(!is.null(m)) {
                message("getting cached data") ## if it's not empty, it is shown
                return(m)
        }
        data <- x$get()
        m <- cachedinverse(data, ...) ## if the cached object is empty the inverse is calculated and set to m
        x$setinverse(m)
        m
}