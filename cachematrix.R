## makeCacheMatrix function is used to make a special list to store a matrix and its inverse matrix.
## cacheSolve function with the argument x returned by makeCacheMatrix is used to calculate the inverse matrix
##    of a matrix stored in x.
##

##
## Return a list to use mMatrix variable to store the original matrix and and mInverseMatrix for its inverse matrix.
## The list also provides set and get functions for setting or getting these two variables.
##

makeCacheMatrix <- function(mMatrix = matrix()) {
    
    mInverseMatrix <- NULL
    set <- function(ivm) {
        mMatrix <<- ivm
        mInverseMatrix <<- NULL
    }
    get <- function() mMatrix
    setInverseMatrix <- function(im) mInverseMatrix <<- im
    getInverseMatrix <- function() mInverseMatrix
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}

##
## Use solve function to get inerse matrix and cache the result for the following
## calculation
##

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mInverseMatrix <- x$getInverseMatrix()
    if(!is.null(mInverseMatrix)) {
        message("Getting cached Inverse Matrix")
        if(anyNA(mInverseMatrix)) {
            message("The matrix is not an invertible matrix")
        }
        return(mInverseMatrix)
    }
    mMatrix <- x$get()
    if(det(mMatrix) == 0) {
        message("The matrix is not an invertible matrix")
        mInverseMatrix <-NA
    }
    else {
        mInverseMatrix <- solve(mMatrix, ...)        
    }
    x$setInverseMatrix(mInverseMatrix)
    mInverseMatrix
}
