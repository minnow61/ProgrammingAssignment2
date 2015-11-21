## These functions caluclates the inverse of a vector of a vector and caches it
## so it doesnt need to be calculated each time (and improves performance)

## Creates and caches the inverse of a vector
makeCacheMatrix <- function(x = matrix()) {
        # start vector as NULL
        inversevector <- NULL
        
        # set the vector
        set <- function(y) {
                x <<- y
                inversevector <<- NULL
        }
        # get the vector
        get <- function() x
        
        # set the inverse
        setInverse <- function(inverse) inversevector <<- inverse
        
        # get the inverse
        getInverse <- function() inversevector
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Computes the inverse of a matrix that is created using makeCacheMatrix
## If it's already been calculated, it will be retrieved from cache (for increased performance)
cacheSolve <- function(x, ...) {
        
        # get the vector to see if it's cached
        inversevector <- x$getInverse()
        
        # if it's not NULL get cahced version
        if(!is.null(inversevector)) {
                message("getting cached data")
                return(inversevector)
        }
        
        # load into a matrix
        m <- x$get()
        
        # get the inverse
        inversevector <- solve(m, ...)
        
        # set it
        x$setInverse(inversevector)
        
        # return the vector
        inversevector
}

