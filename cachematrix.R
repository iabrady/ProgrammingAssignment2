## makeCacheMatrix is a function that takes a matrix as its arguement 
## and does the following:
##  1. It returns a list of stored functions that have been created here
##  2. it caches the input maxtrix to be inverted

makeCacheMatrix <- function(x = matrix()) {
        ## validate we have the right type of data input
    if(!is.matrix(x)) 
        return(print("Error: The input is not a matrix. Please try again")) 
        ## make sure it is a square matrix or return an error
    if(nrow(x) != ncol(x)) 
        return(print("Error: The matrix is not square. It must have the same number of rows and cols to be inverted."))       
        ## initialise the stored inverse matrix
    mxinv <- NULL
        ## create the function to re-set the matrix and initialise the inverse
    setmx <- function(y) {
        x <<- y
        mxinv <<- NULL
    }
        ## create the function to return the cached matrix
    getmx <- function() x
        ## create the function to set the stored inverted matrix
    setinv <- function(solve) mxinv <<- solve
        ## create hte function to retrieve the inverted matrix
    getinv <- function() mxinv
        ## assign the fuctions to the returned list so they can be exected
    list(setmx = setmx, getmx = getmx,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve takes the list of functions constructed in makeCacheMatrix
## and the cached input matrix and executes them to either:
##      1. return the existing cached inverse matrix if it already exists OR 
##      2. calculate, set in cache and return, the inverse matrix of the matrix 
##         that was put in cach by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##
        ## retrieve the cached inverse matrix if it exists using getinv()
    mxinv <- x$getinv()
        ## if it has a value, retrun that and exit
    if(!is.null(mxinv)) {
        message("The cached inverse matrix is already set.")
        message("If you want to get inverse of another matrix re-run makeCacheMatrix again with new matrix.")
        message("Getting cached data.")
        return(mxinv)
    }
        ## Otherwise, retrieve the matrix to be inverted from the cache
    data <- x$getmx()
        ## use solve to get the inverse
    mxinv <- solve(data, ...)
        ## store the inverse in cache & return the inverse matrix
    x$setinv(mxinv)
    mxinv
}
