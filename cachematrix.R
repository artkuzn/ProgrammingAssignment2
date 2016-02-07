## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The following functions are used to create the object that can store a matrix
## and cache its inverse.

## The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inversedM <- NULL
        set <- function(y){
                x <<- y
                inversedM <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inversedM <<- inverse
        getinverse <- function() inversedM
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)        
}


## The function "cacheSolve" computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversedM <- x$getinverse()
        if (!is.null(inversedM)){
                message('getting cached data')
                return(inversedM)
        }
        M <- x$get()
        inversedM <- solve(M, ...)
        x$setinverse(inversedM)
        inversedM
}

