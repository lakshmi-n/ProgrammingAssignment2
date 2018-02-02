
makeCacheMatrix <- function(x = matrix()) {
    ## 1.  `makeCacheMatrix`: This function creates a special "matrix" object
    ## that can cache its inverse.
    ## Returns a list of four functions that store/retrieve the inverse of the matrix
    
    myMatrix <- NULL
    set <- function(y) {
        ## If the matrix changes, the inverse is reset to NULL
        x <<- y
        myMatrix <<- NULL
    }
    get <- function() {
        x
    } 
    setinverse <- function(myInverse) {
        myMatrix <<- myInverse  
    } 
    getinverse <- function() {
        myMatrix
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

cacheSolve <- function(x, ...) {
    ## `cacheSolve`: This function computes the inverse of the special
    ## "matrix" returned by `makeCacheMatrix` above. If the inverse has
    ## already been calculated (and the matrix has not changed), then
    ## retrieves the inverse from the cache.
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
