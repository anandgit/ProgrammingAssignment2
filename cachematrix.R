# Assignment 2 of R Programming course

# The following function creates a cache matrix to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the inverse of the matrix
# 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        m1 <- NULL
	set <- function(y)
	{
		x <<- y
		m1 <<- NULL
	}
	get <- function() x
	setInverse <- function(i) m1 <<- solve(x)
	getInverse <- function() m1
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# The following function calculates the inverse of the cache matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m1 <- x$getInverse()
	if(!is.null(m1))
	{
	        return(m1)
	}
	m1 <- solve(x$get())
	x$setInverse(m1)
	m1
}
