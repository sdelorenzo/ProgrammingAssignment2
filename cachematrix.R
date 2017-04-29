## two functions working together to save system resources in the event
## that an inverse of the same square matrix must be used repeatedly

##make a blank matrix to store an inverse, set variables/functions for use in
##the next function

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y){
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


## calculate the inverse of the given matrix if it hasnt been calculated already

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getinverse()
          if(!is.null(inv)){
              message("getting cached data")
            return(inv)
          }
          data <- x$get()
          inv <- solve(data, ...)
          x$setinverse(inv)
          inv
}
