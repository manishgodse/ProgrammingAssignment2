## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates object 'matrix' which contains multiple functions for caching matrix. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## set the value for matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get the value of matrix
        get <- function() x
        
        ## set the value of inverse of matrix
        setinverse<- function(inverse) m <<-inverse
        
        ## get the value of inverse of matrix
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function check whether inverse is already calculated. 
## If it exists then return the value else it calculates and then returns

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## get the value of inverse of matrix
        m <- x$getinverse()
        
        ## check whether inverse is already calculated. If it is calculated then return it with message.
        if (!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        } 
        
        ## get data for calculating inverse of matrix
        data <- x$get()
        
        # calculate inverse of matrix and return value
        m <- solve(data)
        x$setinverse(m)
        return(m)
}
