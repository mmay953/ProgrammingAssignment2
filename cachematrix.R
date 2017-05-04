## Compute the inverse of a matrix and store it in a cache
## if the inverse has already been computed, retrieve it from cache

## Create a list of functions that can "cache" the inverse of a matrix


# note:  m = inverse of the matrix

makeCacheMatrix   <- function(x=matrix()) {
    m <- NULL                   # create the empty "inverse" matrix that is to be filled later on
    set <- function(y) {        # create set function (allows to update the input matrix x and reset the output "inverse matrix" m)
        x <<- y
        m <<- NULL
    }
    get <- function() x         # function that returns the input matrix x (will be called on in the cacheSolve function below) 
    setinverse <- function(inversevar) m <<- inversevar       # applies the "inversevar" (computed inverse matrix from cacheSolve) to the variable m
    getinverse <- function() m   # returns the inverse matrix m
    list(set=set, get=get, 
         setinverse=setinverse, getinverse=getinverse)    #creates an named list with each of the functions
}

## Return the inverse of a matrix 'x', if the inverse has already been calculated, retrieve it from the cache

cacheSolve <- function(x, ...) {
    m <- x$getinverse()  # retrieves the value of the inverse matrix m (will be NULL if set() has been run or function ran for the first time)
    if(!is.null(m)) {    # tests if the inverse matrix has already been computed ("not null"), if so, retrieve the inverse matrix m 
        message("getting cached data")
        return(m)        # and end function here
    }
    data <- x$get()         #otherwise retrieve the input matrix (x)
    m <- solve(data,...)    # compute the inverse of the matrix
    x$setinverse(m)         # store the computed inverse of the matrix in "m"
    m               
}
