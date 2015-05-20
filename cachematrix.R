#The first function,  makeCacheMatrix returns a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse of matrix
#4. get the value of the inverse of matrix

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        #  sets the matrix && resets the cached value of "inverse of matrix"
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
         #  get the matrix 
        get <- function() x
        #  cache the value of "inverse of matrix"
        setInverse <- function(inv) i <<- inv
        #  get the value of "inverse of matrix"
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix  above. 
## If the inverse has already been calculated (and the matrix has not changed),then  cacheSolve  
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        ## Return a matrix that is the inverse of 'x'
        i <- solve(data, ...)
        # Caching the inverse of matrix
        x$setInverse(i)
        i

}
