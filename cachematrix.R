## The makeCachedMatrix function defines the process of
## creating an instance method for this object.  It defines
## the calling methods for working with the potentially 
## cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    cachedMatrix <- NULL
    set <- function(y) {
        x  <<- y
        cachedMatrix <<- NULL
        }
   
    ## $get - Will return the original matrix submitted
    get <- function() x
    
    ## $setinverse - Will store the inverse matrix in the parent
    ##            environment
    setinverse <- function(imatrix) cachedMatrix <<- imatrix
    
    ## $getinverse - Retrieves the cached inverse matrix
    getinverse <- function() cachedMatrix
    
    ## Defines the entry points for the function calls
    list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse) 
}


## This function is more of a helper function for working
## with the MakeCacheMatrix functions functions.  It takes
## the instance variable of the makeCacheMatrix function as
## a parameter so as to retrieve or calculate then cache the
## inverse matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getinverse()
   
    ## Check if inverse matrix exists, yes return it and 
    ## be done    
    if (!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    
    ## If inverse matrix not cached calculate it and
    ## then store it in cache
    message("Cacheing the inverse matrix")
    originalMatrix <- x$get()
    inverseMatrix <- solve(originalMatrix, ...)
    x$setinverse(inverseMatrix)
    
    inverseMatrix    # Return Value        
}

## These are my test methods, per instructions ignore
## when grading
unitTest <- function() {
    mat <- matrix(c(3,7,2,9),2,2)
    mcm <- makeCacheMatrix(mat)
    t <- mcm$get()
    print(t)
    t <- mcm$getinverse()
    print(t)
    
    cacheSolve(mcm)
    cacheSolve(mcm)

    mat <- matrix(c(3,3,2,3,7,2,7,1,9),3,3)
    mcm <- makeCacheMatrix(mat)
    t <- mcm$get()
    print(t)
    t <- mcm$getinverse()
    print(t)
    
    cacheSolve(mcm)
    cacheSolve(mcm)
}

unitTest()

