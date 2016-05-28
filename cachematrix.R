## makeCacheMatrix, when first invoked using a matrix as an argument, creates
## a list object that contains the matrix passed and a NULL vector in lieu of the 
## inverse of the matrix.

## It manages to do so via two variables that exist in the environment of the function: x (the matrix) and 
## m (the inverse).

## It also defines via lexical scoping functions that are part ofm the list and later are called by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This works in conjunction with cacheSolve: it takes as an argument the object (list) created with makeCacheMatrix
## and then checks wether the variable m is NULL, and then calculates the inverse, stores it in the list object and
## returns the inverse of the matrix. If m is not NULL then it simply returns m with the content of the inverse of
## the matrix calculated in a prior call to the cacheSolve function.

cacheSolve <- function(x) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}