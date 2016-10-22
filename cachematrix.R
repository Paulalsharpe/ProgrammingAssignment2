## These function will first, create a list of functions that can be called
## and then a function twill invert a martix.
## However, thes second function will not invert the matrix if the 
## function matrix combination has previously been run. If the matrix has been 
## changed using the $setmatrix function the cached i is reset to NULL
## ensuring that an incorrect inverse is not returned from cache.

## This Function defines a list of functions, these include setmatrix,
## getmatrix, setinverse and getinverse.  The function also sets i to NULL
## when the funciton is instantiated.

makeCacheMatrix <- function(x = matrix()) {
    ## initialises i NULL
    i <- NULL
    ## defines function that can be used to change the matrix x. also sets i to 
    ## NULL cache so any following initialisation of the cachesolve function
    ## does not return the previous cached inverse.
    setmatrix <- function(y) {
        x <<- y
        i <<- NULL
    }
    #defines function that will return the matrix x
    getmatrix <- function() x
    #defines function that will store a variable in cache
    setinverse <- function(inverse) i <<- inverse
    #defines funciton that will return the inverted matrix i
    getinverse <- function() i
    #creates and returns the list of functions
    list(setmatrix=setmatrix, getmatrix = getmatrix,
         setinverse = setinverse,getinverse = getinverse)
}


## This function calls the getinverse to check the cached i and then checks to
## see if it is not NULL. If it is not NULL the function returns the cached 
## inverse.  If the cached i is NULL the matrix is assigned to data, solved 
## (i.e. inverted), cached and returned.

cacheSolve <- function(x, ...) {
    ## Calls the getinverse function this will return the object i 
    ## (matrix inverse) that is stored in the cache and assign to i in the 
    ## current enviroment. Returns NULL if function has not been previously run.    
    i <- x$getinverse()
    ## Checks to see if i is not equal to NULL if TRUE then prints 
    ## message, returns cached inverse and breaks out of function.
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## calls getmatrix function and assigns matrix to data
    data <- x$getmatrix()
    ## calls solve function on the matrix to return the inverse and assigns to i
    i <- solve(data,...)
    ## calls setinverse function and passes the inverse matrix (i) as an arguement
    ## this loads the inverse into cache assigned to i
    x$setinverse(i)
    ## returns the inverse matrix
    i
}
