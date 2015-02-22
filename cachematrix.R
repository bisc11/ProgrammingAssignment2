## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverse matrix
## 4 get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL                         ## Initialise inv as future variable to store the inverse.
    
    set <- function(y) {                ## Create function 'set' which takes y as an argument and
        x <<- y                         ## searches parent environments for x and inv, and redefines x to y and
        inv <<- NULL                    ## inv to NULL if found. (here: set function is local environment and 
    }                                   ## makeCacheMatrix is parent environment) (the global environment is used 
                                        ## for x and inv, if they are not found)
    
    get <- function() x                 ## Create function 'get' which returns x
    
    setinverse <- function(inverse) {   ## Create function 'setinverse' which takes 'inverse' as an argument
        inv <<- inverse                 ## and searches parent environments for inv and redefines inv to inverse if found.
    }                                   ## (here: setinverse function is local environment and makeCacheMatrix
                                        ## is parent environment)
    
    getinverse <- function() inv        ## Create function 'getinverse' which returns inv
    
    list(set = set, get = get,          ## list makes a list of the created functions.
         setinverse = setinverse,             
         getinverse = getinverse)             
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function.
## However, it first checks to see if the inverse has already been computed. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value
## of the inverse in the cache via the setinverse function.
## You have to use makeCacheMatrix first, before you can use cacheSolve

cacheSolve <- function(x, ...) {
                                                
    inv <- x$getinverse()                ## Assigns the value of existing inv from makeCacheMatrix to inv
    if(!is.null(inv)) {                  ## Check if inv is not NULL. This is TRUE when inv was computed before and not reset.
        message("getting cached data")   ## If TRUE than print message
        return(inv)                      ## and return inv. This also ends the function cacheSolve.
    }
    data <- x$get()                      ## If FALSE, and inv was not calculated before or was reset, assign x 
                                         ## from makeCacheMatrix to data,
    inv <- solve(data, ...)              ## compute the inverse of data and assign this to inv
    x$setinverse(inv)                    ## store the computed inverse inv into inv from makeCacheMatrix environment
    inv                                  ## print inv
}


## Personal note.
## Without the thread “Caching in practise” in the discussion forum I don’t think I would have understand
## this assignment. So thanks to all my fellow students and TA’s who contributed!