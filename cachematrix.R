

## This is a pair of function that cache the inverse of a matrix.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        ## Set the matrix
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Get the matrix
        
        get <- function() x
        
        ## Inverse of the matrix
        
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        
        ## List of methods
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse ()
        
        ## Return the inverse if it is already set
        
        if(!is.null(inv)) {
                message("getting cache data")
                return(inv)
                
        }
        
        ## Get the matrix from our object
        
        data <- x$get()
        
        ## Calculate the inverse
        
        inv <- solve(data,...)
        
        ## Set the inverse
        
        x$setInverse(inv)
        
        inv
        
}
