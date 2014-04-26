## This function efficently computes the inverse of a given matirx with cache.
##Cache reduces unnecessary computation.

##This function extract data and combine it with the function and variables later
##used for cache.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {##list to confirm to sample code 
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse ##function that store computed reselt 
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function determines whether computer computes or display cache.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {##determine if the data was cached
                message("getting cached data")
                return(i)
        }
        data <- x$get()##get data will used for computation
        i <- solve(data, ...)##compute inverse matrix
        x$setinverse(i)##store the computed result into external variable
        i##discplay computed reselt as the output of the function cacheSolve
        ## Return a matrix that is the inverse of 'x'
}
