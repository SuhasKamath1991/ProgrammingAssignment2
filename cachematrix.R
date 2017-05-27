## Compute inverse of a matrix and store it in cache and return the stored value from cache

## Create a matrix and compute inverse

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
                x <<- y
                a <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) a <<- solve
        getinverse <- function() a
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Check for cached inverse of the matrix and return the value. If cache not found then compute inverse and return

cacheSolve <- function(x, ...) {
        a <- x$getinverse()
        if(!is.null(a)) {
                message("getting cached data.")
                return(a)
        }
        data <- x$get()
        a <- solve(data)
        x$setinverse(a)
        a
}
