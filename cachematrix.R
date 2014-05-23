##-------------------------------------------------------------------------
##
## This module contains a pair of functions (below) designed to cache
## the inverse of a matrix, to avoid the need to compute the inverse
## each time that it is used.
##
## makeCacheMatrix : get or set the matrix and matrix inverse
## cacheSolve      : return inverse if it exists; compute and cache otherwise
##
## Refer to the comments associated with the respective functions for
## further detail.
##
##
## (R Programming course, Programming Assignment #2, May 2014)
## (This assignment is designed to illustrate the behavior and use of
## lexical scoping, using the super-assignment (<<-) operator to
## persistently store data within a function, for subsequent retrieval.)
##
##-------------------------------------------------------------------------

##
## makeCacheMatrix
##
## Cache a matrix and its inverse.
##
## It returns a list of four functions:
##   set()        : sets (stores) the matrix
##   get()        : gets (retrieves) the matrix
##   setinverse() : sets (stores) the matrix inverse
##   getinverse() : gets (retrieves) the matrix inverse
##

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    # set (store) the matrix data
    set <- function(y) {
        x <<- y         # cache the matrix data
        inv <<- NULL    # initialize matrix inverse to NULL
    }

    # get (retrieve) the matrix data
    get <- function() x

    # set (store) the pre-computed matrix inverse
    setinverse <- function(minverse) inv <<- minverse

    # get (retrieve) the stored matrix inverse
    getinverse <- function() inv

    # return a list of the above four functions
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse )
}

##
## cacheSolve
##
## Return the cached matrix inverse, if it exists.
## Otherwise, compute the matrix inverse, cache it, and return it.
##
## The first argument to this function is a 'makeCacheMatrix' object,
## which defines the necessary functions to set and get a matrix and
## the inverse of the matrix.
## 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()   # get (retrieve) the cached matrix inverse

    #
    # If the matrix inverse is not null, then it was already computed.
    # Simply return the cached matrix inverse.
    #
    if ( !is.null(inv) ) {
        message("Getting cached matrix inverse")
        return(inv)
    }

    #
    # If the matrix inverse is null (i.e., has not been computed yet),
    # then compute, cache, and return the matrix inverse.
    #
    m <- x$get()   # get the matrix

    #
    # If the matrix is not square, then its inverse does not exist.
    # However, if square, assume that the matrix is non-singular and thus
    # has an inverse.
    #
    if ( dim(m)[1] != dim(m)[2] ) {
        message("Error: cannot compute inverse because matrix is not square.")
        return(inv)
    }

    #
    # Compute, cache, and return the inverse.
    #
    inv <- solve(m, ...)     # compute the matrix inverse
    message("setting inverse...")
    x$setinverse(inv)        # set (store) the computed matrix inverse
}
