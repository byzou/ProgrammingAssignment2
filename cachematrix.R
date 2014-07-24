 ## The following two functions are created to cache the inverse of a 
## special matrix.

## The following function creates a special matrix via four steps, set
## the value of matrix, get the value of matrix,  set the value of matrix 
## inverse and get the value of matrix inverse. The output of makeCacheMatrix
## is a list containing four parts: set, get, setinverse and getinverse.

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
             setinverse = setinverse,
             getinverse = getinverse)

}

## The following function calculates matrix inverse. It first checks 
## whether the matix inverse has already been calculated. If so, it
## prints a message of "getting cached data", gets matrix inverse from  
## the cache and skips the computation. Otherwise, it calculates the 
## inverse of the data and sets the value of the inverse in the cache 
## via the setinverse function.The output of this function is a matrix that
## is the inverse of 'x'.

cacheSolve <- function(x, ...) {
        i<- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        matrix<- x$get()
        i <- solve (matrix, ...)
        x$setinverse(i)
        i  
        ## Return a matrix that is the inverse of 'x'
}
       


