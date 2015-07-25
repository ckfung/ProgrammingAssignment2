## The functions below is to cache and compute the inverse of a square matrix

## makeCacheMatrix contains 4 different functions
## get() : Get the value of matrix
## set() : Set the value of matrix
## getInverse : Get the inverse value of the matrix
## setInverse : Set the inverse value of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
	m <<- NULL
	}
    get <- function() x
    setInverse <- function(inv) m <<- inv
    getInverse <- function() m
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("Getting cached data")
	return(m)
    }

    matr <- x$get()
    m <- solve(matr) %*% matr
    x$setInverse(m)
    return(m)
}
