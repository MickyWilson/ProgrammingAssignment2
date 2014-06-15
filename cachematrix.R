###############################################################################
## Caching the Inverse of a Matrix
###############################################################################
##
## This module conatins a pair of functions 
##   makeCachedMatrix
##   cacheSolve
## that together can be used to calculate and cache the inverse of a matrix.
##
## Usage:
##    cm = makeCachedMatrix(matrix)
##    inverse = cacheSolve(cm,...)
##    ....
##    inverse = cacheSolve(cm,...)
##
## The first call to cachSolve will calculate the inverse of matrix, 
## which may be a relatively long process.  The calculated inverse will be 
## cached (saved) and returned to the caller.
## The 2nd and subsequent calls will uses the pre-computed (cached) value.
## 
## The user may also call cm$set(new_matrix) to set the underlying matrix, in which
## case the cached value will be reset, meaning the inverse will be recalculated next time 
## it is required.
##
###############################################################################

###############################################################################
## Function   : makeCacheMatrix
## Description: Make a 'CachedMatrix'
## Argument(s): mat -> matrix to cache inverse calculations for.
##                  User is responsible for ensuring matrix can be inverted
##                  Default value is an empty matrix
## Returns    : List object that can be used as agrument to cacheSolve
###############################################################################
makeCacheMatrix <- function(mat = matrix()) {
    
    # Variables in this lexical scope
    # mat        is the matrix we are caching the inverse for - (passed in as argument)
    # inverse    holds the inverse of the given matrix mat, or NULL
    #
    inverse <- NULL
    
    # set: Set the underlying matrix.  Resets the cached inverse value. 
    set <- function(new_matrix) {
        # set matrix
        mat <<- new_matrix
        
        # un-cache marix
        inverse <<- NULL
    }
    
    # get: Returns the underlying matrix
    get <- function() 
        mat
    
    # setinverse: Sets the inverse of the underliying matrix
    setinverse <- function(inv) 
        inverse <<- inv
    
    # getinverse: Gets the inverse of the underliying matrix
    getinverse <- function() 
        inverse
    
    # return list of functions
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

###############################################################################
## Function   : cacheSolve
## Description: Returns identity matrix for cached matrix
## Argument(s): cachedMatrix - object returned from makeCachedMatrix                 
## Returns    : Identity matrix 
###############################################################################
cacheSolve <- function(cachedMatrix, ...) {
    # get inverse from 'x'
    inverse <- cachedMatrix$getinverse()
    # if inverse is calculated return it now
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    # we need to calculate it
    message("calculating inverse")
    # get identity matrix.  2nd argument is missing so as to get identity matrix
    inverse <- solve(cachedMatrix$get(),,...)
    # store/cache results before returing result
    cachedMatrix$setinverse(inverse)
    
    inverse
}

##
## Test harness
##
test <- function() {
    # Get reference data (matrix and its inverse)
    m = matrix(c(1,2,3,4), nrow=2)
    id = matrix(c(1,0,0,1),nrow=2)
    m_inv = solve(m)
    # sanity check
    stopifnot(m%*%m_inv==id)
    
    # create cache of matrix
    cm = makeCacheMatrix(m)
    message("Testing $get")
    stopifnot(cm$get()==m)

    for(i in 1:3){
        message("Calling cachSolve")
        cm_inv = cacheSolve(cm)
        stopifnot(cm_inv== m_inv)        
    }

    #
    # Check set works
    #
    m2 = matrix(c(4,3,2,1), nrow=2)
    message("Testing $set")
    cm$set(m2)
    stopifnot(cm$get()==m2)
    
    #
    # check inverse now works on new matrix data
    #
    # Should get cached message first time in loop
    for(i in 1:3){
        message("Calling cachSolve")
        m2_inv = cacheSolve(cm)
        stopifnot(m2_inv%*%m2==id)
    }
    
    message("Passed all tests")
}
