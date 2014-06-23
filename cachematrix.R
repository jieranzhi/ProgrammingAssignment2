## there are two functions in this file,
## 
## the first one is named makCachematrix which create a special
## matrix that cache the inverse of the input matrix
##
## the second one is called cacheSolve, in which the inverse of 
## the special "matrix" returned by makeCacheMatrix is computed
## and if it already exists, the exist one will be retrived
## 
## to find out more details, see comments below

## creates a special "matrix" object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
        ivs <- NULL
        set <- function(y) {
                x <<- y
                ivs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) ivs <<- inverse
        getinverse <- function() ivs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), cacheSolve
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ivs <- x$getinverse()
        if(!is.null(ivs)) {
                message("getting cached data")
                return(ivs)
        }
        data <- x$get()
        ivs <- solve(data, ...)
        x$setinvers(ivs)
        ivs
}
