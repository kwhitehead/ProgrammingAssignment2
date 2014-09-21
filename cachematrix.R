##These functions are used to create a matrix that can cache its inverse. New calculations
## get solve from cache, if the value has already been calculated.

## Create a matrix to store a matrix and cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                 		
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Check solve to see if it's already been calculated. Retrieve solution from cache or 
## perform new solve and store value of solve in the cache. 

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        