## These functions combined, will be able to return an inverse of a matrix. 
## The function is written so that, should a matrix have remained the same, it will returned it's previously cached result, rather than computing the inverse again,
## making the solution more efficient. 

## makeCachematrix creates a 'special' matrix out of a simple matrix and makes it able to store the inverse in the cached memory

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y){
        x <<- y
        m <<- NULL
}
get <- function() x
setsolve <- function(solve) m <<- solve
getsolve <- function() m
list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve checks to see if the special matrix matches the special matrix from the previously cached inverse, and returns the cached inverse if true. 
## Should the special matrix have been altered, it computes and returns the appropriate inverse. 

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setsolve(m)
        m
}
