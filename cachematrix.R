## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix consists of set, get, SetInverse, and getInverse

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL             # initializing inverse as NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() (x)   # function to get matrix x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() {
                inver <- ginv(x)
                inver%*%x # obtain the inverse of the matrix
        }
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## this is used to get the cache data

cacheSolve <- function(x, ...) {        #gets cache data
        inv <- x$getInv()
        if (!is.null(inv)) {    # checking if inverse is NULL
                message("getting cached data!...")
                return(inv)     # returns the inverse value
        }
        mat <- x$get()
        inv <- solve(mat, ...)  # calculates inverse value
        x$setInv(inv)
        inv     # return a matrix that is the inverse of 'x'
       
}
