makeCacheMatrix <- function(x = matrix()) {
        
        xinverse <- NULL
        set <- function(y) {
                originalMatrix <<- y
                xinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(z) xinverse <<- z
        getinverse <- function() xinverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function will return the inverse of the matrix obtained from the list

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinverse <- x$getinverse()
        if(!is.null(xinverse)) {
                message("getting cached data")
                return(xinverse)
        }
        data <- x$get()
        xinverse <- solve(data, ...)
        x$setinverse(xinverse)
        xinverse
}
