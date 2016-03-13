?## Put comments here that give an overall description of what your
## functions do
## Description: In order to shorten the computation times of solving
## the inverse of a matrix I have created two functions which, when called,
## ascribes the inverse of a matrix to the cache of R if the matrix is not  
## already stored in cache. The need for doing the same possibly lengthy 
## computation of the inverse of a square matrix is thus eliminated.

## Write a short comment describing this function
## The following function takes a square matrix as its argument (setting 
## the value of the matrix) gets the value, solves the inverse of the matrix
## and lastly setting this value. 
## 
makeCacheMatrix <- function(x = matrix()){m <- NULL
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


## Write a short comment describing this function
## cacheSolve takes the function MakeCacheMatrix as argument, 
## checks the cache for the value set in MakeCacheMatrix, and returns
## either the cached value, if it already exists, or the new value,
## storing it in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}