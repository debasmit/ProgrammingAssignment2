## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
             m <- NULL
             set <- function(y) {
                     x <<- y
                     m <<- NULL
             }
             get <- function() x
             setinv <- function(inv) m <<- solve(x)
             getinv <- function() m
             list(set = set, get = get,
                  setinv = setinv,
                  getinv = getinv)
 }


## it calculates the inverse of the matrix and sets the value of the inverse in the cache via the `setinv` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- solve(x)
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data)
            x$setinv(m)
            m
}
