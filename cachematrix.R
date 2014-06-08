## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## makeCacheMatrix(mat)
makeCacheMatrix <- function(mat = matrix()) {
    # 
    # mat        is the matrix we are caching the inverse for
    # inverse    holds the inverse of given matrix
    #
    inverse <- NULL
    
    #
    # function: set(new_matrix)
    #    sets 
    #
    set <- function(new_matrix) {
        # set matrix
        mat <<- new_matrix
        
        # un-cache marix
        inverse <<- NULL
    }
    
    # Returns the matrix
    get <- function() mat
    
    # Sets the inverse matrix
    setinverse <- function(inv) inverse <<- inv
    
    # returns the current inverse
    getinverse <- function() inverse
    
    # Return our 'object'
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Write a short comment describing this function
## Returns identity matrix for cached matrix
## Varaible arguments
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
    # store/cache results
    cachedMatrix$setinverse(inverse)
    
    inverse
}

##
## Test
##
test <- function() {
    c = matrix(c(1,2,3,4), nrow=2)
    inv_c = solve(c)
    cm = makeCacheMatrix(c)
    stopifnot(cm$get()==c)
    result = cacheSolve(cm)
    stopifnot(result==inv_c)
    result2 = cacheSolve(cm)
    stopifnot(result2==inv_c)    
    result3 = cacheSolve(cm)
    stopifnot(result3==inv_c)
    cm$set(c)
    result3 = cacheSolve(cm)
    result3 = cacheSolve(cm)    
}
makeVector <- function(x = numeric()) {
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

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
