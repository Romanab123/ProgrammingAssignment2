## The function is used to create a object that stores a matrix and caches the inverse

## makeCacheMatrix is a "matrix", which is really a list containing a function to
## set the value of the matrix, get the value of the matrix, set the value of the 
## inverse and get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y 
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse have been already calculated, the function should retrieve 
## the inversed from cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)){
                message("recieving cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
