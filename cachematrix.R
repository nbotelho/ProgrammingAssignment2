## The below functions accept a matrix as input and return the value of the Inverse of that matrix, if already computed and cached,
## else will compute the new Inverse and store in cache when a new matrix is provided.

##    The makeCacheMatrix function,  creates a special "vector" , which is really a list containing a function to
##  1. set the value of the matrix provided as input
##  2. get the value of the matrix
##  3. set the value of the Inverse of the matrix
##  4. get the value of the Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    ## Clearing the value of m in cache
    m <- NULL
    
    ## Changing the value of the matrix per the input provided, and clearing the value of m in cache
    set <- function (y) {
    x <<- y
	m <<- NULL
    }
    
    ## Getting the value of the matrix which was prvoided as input to the makeCacheMatrix function
    get <- function() x 
    	
    ## Setting the value of the inverse in cache
    setInverse <- function(Inverse) m <<- Inverse
    
    Getting the value of the inverse from cache
    getInverse <- function() m	
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

    
}


## The cacheSolve  function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache, and skips the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' if it was cached previosuly
        m <- x$getInverse()
        
        ## If found something in cache then return that something
        if (!is.null(m)) {
           message ("getting cached data")
           return (m) 
        }
        ## else compute the Inverse of the matrix
        else
        {
        	data <- x$get()
        	m <- solve (data)
        	x$setInverse(m)
        	m
        }   
}
