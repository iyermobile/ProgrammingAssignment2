# github - iyermobile code Dec/21/2014
# It is beneficial to caching the inverse of a matrix rather than compute it repeatedly since Matrix inversion is usually a resource consuming computation. 
# The following two functions are used to cache the inverse of a matrix.

# Usage Code Flow:
# > x <- matrix(rnorm(16), nrow = 4)          // Create a matrix x
# > cx <- makeCacheMatrix(x)                  // Create our special matrix
# > cx$get()                                  // Return the matrix
# > cacheSolve(cx)                            // Return the inverse
# > cacheSolve(cx)                            // Call the 2nd time, so return
#                                             // the cached inverse

# makeCacheMatrix creates a list containing a function that does below:
# 1. Sets the value of the matrix
# 2. Gets the value of the matrix
# 3. Sets the value of inverse of the matrix
# 4. Gets the value of inverse of the matrix

## Define a pair of functions that cache the inverse of a matrix rather than computing.
## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	## set inverse as null
        inv <- NULL
        
	## Define a function that assigns matrix x to a new matrix y and resets the inverse as NULL
        set <- function(y) {
               x <<- y
               inv <<- NULL
        }

        ## define a function to return x
        get <- function() x

        ## define a function using function solve to get the inverse of matrix 'x' and store into 'inv'
        setinverse <- function(solve) inv <<- solve

        ## define a function to return the inverse of matrix 'x'
        getinverse <- function() inv

        ## return a list of functions defined
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Computes the inverse. If the inverse has already been calculated (and the matrix has not changed).
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
	## Return a matrix that is the inverse of 'x'
        ## call the function to get the inverse of matrix 'x'
        inv <- x$getinverse()
        
	## test whether the result exits
        if(!is.null(inv)) {
        
                message("getting cached data")
                return(inv)
        }
        
	## get the original matrix and compute the inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
