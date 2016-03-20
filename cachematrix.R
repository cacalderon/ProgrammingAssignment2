## makeCacheMatrix is function that contains other functions. 
## get returns a matrix.
## set can be used to change the matrix in the main function. 
## setmean stores the value of the input in a variable m into the main function.
## getmean return it the vale of the input.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

## cachesolve is a function that computes the inverse of the matrix from makeCacheMatrix.
## first it checks the value m to see if its empty, and proceeds to calculate inverse.

cacheSolve <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        #print(data)
        m <- solve(data, ...)
        #print(data)
        x$setmean(m)
        m
}
