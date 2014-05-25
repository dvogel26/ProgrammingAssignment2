## This file contains two functions: makeCacheMatrix creates a special "matrix" that 
## can cache the inverse of an invertible matrix; cacheSolve returns the
## inverse of the matrix. If the inverse was previously computed, cacheSolve merely returns
## the cached value; if not, it first computes the inverse and caches the value.
## 
## Author: David Vogel
## Date: May 25, 2014
##
## Example
##
##   Create an invertible matrix:
##     hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
##     h8  <- hilbert(8)
##   Create a special "matrix" that stores h8 and can cache its inverse:
##     cm <- makeCacheMatrix(h8)
##   Get its inverse:
##     cmi <- cacheSolve(cm)
##   Check that cmi is inverse of cm:
##     round(cmi %*% cm$get(), 3)
##   Check that next call to cacheSolve(cm) displays the message "getting cached data"
##     cacheSolve(cm)


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
##
## Usage:
##   makeCacheMatrix(x)
##
## Arguments:
##   x   an optional matrix that can have an inverse; default is an empty matrix 
##
## Returns:
##  A list containing four functional objects closed over the same
##  lexical scope; given cm <- makeCacheMatrix(x):
##    cm$set(y)             sets the cached matrix to y, and its inverse to NULL   
##    cm$get()              returns the cached matrix
##    cm$setInverse(inv)    sets the cached inverse to inv
##    cm$getInverse()       returns the cached inverse
makeCacheMatrix <- function(x = matrix()) {
        # holds the inverse of x
        xInv <- NULL
        
        # reset x and xInv, which are lexically scoped in the function set
        set <- function(y) {
                # use <<- because assignment occurs when fn is called
                x    <<- y
                # previous inverse no longer valid
                xInv <<- NULL
        }
        # returns value of matrix
        get <- function() {
                x
        }
        # sets value of cached matrix inverse
        setInverse <- function(inv) {
                message("setting cached data")
               xInv <<- inv
        }
        # returns cached matrix inverse
        getInverse <- function() {
            xInv    
        }
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already 
## been calculated, and the matrix has not changed, 
## then cacheSolve retrieves the inverse from the cache.
## 
## Usage:
##   cacheSolve(x, ...)
##
## Arguments:
##   x      special "matrix" returned by makeCacheMatrix()
##   ...    optional arguments to solve()
## Returns: inverse of x$get()
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv    <- solve(matrix, ...)
        x$setInverse(inv)
        
        ## Return a matrix that is the inverse of 'x'
        inv
}
