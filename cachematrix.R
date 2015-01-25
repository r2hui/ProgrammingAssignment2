## The functions below calculates the inverse of a matrix, x, assumed square.
## It caches the inverse of the matrix.

## The function below creates ways to set the value of the matrix, 
## get the value of the matrix, set the value of the inverse,
## and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## The function below calculates the inverse of 'x'.
## If it's already calculated from the above function, 
## then it just returns the inverse. Otherwise, it will solve and
## store it in the cache.

cacheSolve <- function(x, ...) {
       
	inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cache")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
