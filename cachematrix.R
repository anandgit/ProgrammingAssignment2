# Assignment 2 of R Programming course

# The following function creates a cache matrix to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the inverse of the matrix
# 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        m1 <- NULL
	
	# set the value
	set <- function(y)
	{
		x <<- y
		m1 <<- NULL
	}
	
	# get the value
	get <- function() x
	
	# set the inverse
	setInverse <- function(x) m1 <<- solve(x)
	
	# get the inverse
	getInverse <- function() m1
	
	# create a list
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# The following function calculates the inverse of the cache matrix

cacheSolve <- function(x, ...) {
	
        # Return a matrix that is the inverse of 'x'
        m1 <- x$getInverse()
	
	# If not null, return m1 
	if( !is.null(m1) )
	{
	        return(m1)
	}
	
	# solve
	m1 <- solve(x$get())
	
	# set inverse
	x$setInverse(m1)
	
	# return value
	m1
}
