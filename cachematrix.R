## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix will take a matrix and return a list of function that can be called on it
## cacheSolve will take the list returned by makeCacheMatrix and return the cached 
## inverse or calculate the inverse, cache it, and return it.

## Write a short comment describing this function
## makeCacheMatrix create a special matrix that will:
## set - set the value of the matrix
## get - return the value of the matrix
## setinverse - set the inverse of the matrix
## getinverse - return the inverse of the matrix or NULL

makeCacheMatrix <- function(x = matrix()) {

        mInverse <- NULL
        set <- function(y) {
                x <<- y
                mInverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) mInverse <<- inverse
        getinverse <- function() mInverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve returns the inverse of a special matrix via either 
## cached value or calculated value if not already cached and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        mInverse <- x$getinverse()
        if(!is.null(mInverse)) {
                message("getting cached inverse")
                return(mInverse)
        }
        data <- x$get()
        mInverse <- solve(data, ...)
        x$setinverse(mInverse)
        mInverse
}
