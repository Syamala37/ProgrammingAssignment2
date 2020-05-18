## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
##set the value of the vector, get the value of the vector, set the value of the mean, get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## Write a short comment describing this function
##The following function calculates the inverse of the special "vector" created with the above function.
##However, it first checks to see if the inverse has already been calculated.
##If so, it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}
