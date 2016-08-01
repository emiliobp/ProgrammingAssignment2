## MakeCacheMatrix builds the functions that handle the data for parent environment
## and CacheSolve cache the matrix and returns its inverse.

## This function basically builds different kinds of functions and then returns
## the functions in a list to the parent environment for later use.

makeCacheMatrix <- function(x = matrix()) {
    ## initialize inv variable to NULL
    i <- NULL
    
    ## set variable that has asigned a function that will assign y value to the parent environment of x
    ## same for i, instead is a NULL
    set <- function(y) { 
        x <<- y 
        i <<- NULL 
    }
    
    ## get varibale thar has a function assigned and returns x
    get <- function(){
        x
    }  
    
    ##setinverse assigns the parameter inverse to the parent env. of i
    setinverse <- function(inverse) {
                    i <<- inverse 
                }
    ## getinverse return the matrix
    getinverse <- function() {
                    i 
                }
    ##Assigns each functions  as an element of a list and return them to the parent environment
    list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)	 
 } 
    


## CacheSolve cache the matrix and returns its inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse() 
     
    ## if the matrix contains data will return the inverse from cache
    if(!is.null(i)) { 
        message("Getting the cached matrix") 
        return(i) 
    } 
     
    ## If its empty get the matrix from the parent 
    data <- x$get() 
     
    ## Uses Solve() to calculate inverse
    i <- solve(data, ...) 
    
    ## Cache this result in the object 
    x$setinverse(i) 
    
    ## Return new matrix 
    i  
}
