## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" and returns a list of
## functions to set the matrix and cache the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )
}

## this functioncalculates the inverse of the special "matric" created with the
## makeCacheMatrix function. However, it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the data and sets the value
## of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
