# ## Caching the Inverse of a Matrix:
# ## Matrix inversion is usually a costly computation and there may be some 
# ## benefit to caching the inverse of a matrix rather than compute it repeatedly.
# ## Below are a pair of functions that are used to create a special object that 
# ## stores a matrix and caches its inverse.
# 
# ## This function creates a special "matrix" object that can cache its inverse.
# 
# makeCacheMatrix <- function(x = matrix()) {
#         inv <- NULL
#         set <- function(y) {
#                 x <<- y
#                 inv <<- NULL
#         }
#         get <- function() x
#         setInverse <- function(inverse) inv <<- inverse
#         getInverse <- function() inv
#         list(set = set,
#              get = get,
#              setInverse = setInverse,
#              getInverse = getInverse)
# }
# 
# 
# ## This function computes the inverse of the special "matrix" created by 
# ## makeCacheMatrix above. If the inverse has already been calculated (and the 
# ## matrix has not changed), then it should retrieve the inverse from the cache.
# 
# cacheSolve <- function(x, ...) {
#         ## Return a matrix that is the inverse of 'x'
#         inv <- x$getInverse()
#         if (!is.null(inv)) {
#                 message("getting cached data")
#                 return(inv)
#         }
#         mat <- x$get()
#         inv <- solve(mat, ...)
#         x$setInverse(inv)
#         inv
# }



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

# ## This function computes the inverse of the special "matrix" created by 
# ## makeCacheMatrix above. If the inverse has already been calculated (and the 
# ## matrix has not changed), then it should retrieve the inverse from the cache.
# 
# cacheSolve <- function(x, ...) {
#         ## Return a matrix that is the inverse of 'x'
#         inv <- x$getInverse()
#         if (!is.null(inv)) {
#                 message("getting cached data")
#                 return(inv)
#         }
#         mat <- x$get()
#         inv <- solve(mat, ...)
#         x$setInverse(inv)
#         inv
# }

## Computes the inverse of a matrix that is created in makeCacheMatrix
## If it's already been calculated it will be retrieved from cache (for increased performance)
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
        inversevector <- solve(m, ...)########################## %*% data
        
        # set it
        x$setInverse(inversevector)
        
        # return the vector
        inversevector
}

# ##create a function which starts with a null matrix argument
# makeCacheMatrix <- function(x = matrix()) { 
#         ## initialize the value of the matrix inverse to NULL
#         matrixinverse <- NULL                     
#         ## delcare another function set where the value will be cached in 1. Matrix is created
#         ## for the first time. 2. changes made to cached matrix
#         set <- function(y) {                      
#                 x <<- y
#                 ## change the value of inverse of the matrix in case the matrix was changed.
#                 matrixinverse <<- NULL              
#         }
#         ## gets the value of the inverse
#         get <- function() x                           
#         #calculates the inverse of non-singular matrix via the solve function
#         setinverse <- function(solve) matrixinverse <<- solve 
#         # gets the inverse     
#         getinverse <- function() matrixinverse        
#         ## passes the value of the function makeCacheMatrix        
#         list(set = set, get = get,                    
#              setinverse = setinverse,
#              getinverse = getinverse)
# }
# 
# # used to get the cache of the matrix
# cacheSolve<- function(x, ...) {                 
#         matrixinverse <- x$getinverse()
#         #if the inverse exists, it gets it.
#         if(!is.null(matrixinverse)) {                 
#                 message("getting cached data - Inverse of the matrix")
#                 return(matrixinverse)
#         }
#         #if the inverse if not there, first it is calculated and then retrieved.
#         data <- x$get()                               
#         matrixinverse <- solve(data, ...)
#         x$setinverse(matrixinverse)
#         matrixinverse
# }