## This code contains two functions accomplishing the following:
##- makeCacheMatrix: This function creates a special "matrix" object that...
##...can cache its inverse
##- cacheSolve: This function computes the inverse of the special "matrix"...
##...returned by makeCacheMatrix


## The objective is to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Instantiate to NULL the variable that will receive the matrix's inverse
        minv <- NULL
        
        ## Define the set function to cache values
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        
        ## Define the get function to retrieve cached values
        get <- function() {
                x
        }
        
        ## Define the setinverse function to set the matrix's inverse value
        setinverse <- function(inverse){
                minv <<- inverse
        }
        
        ## Define the getinverse function to retrieve the inverse
        getinverse <- function(){
                minv
        }
        
        ##
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),... 
##...then the cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {        
        ## Retrieve value from cache
        minv <- x$getinverse()
        
        ## check if matrix's inverse exists in cache
        if (!is.null(minv)){
                return(minv) 
        }
        
        data <- x$get()
        
        ##inverse the matrix
        minv <- solve(data)
        
        ##set the matrix's inverse
        x$setinverse(minv)
        
        ##return the matrix
        return(minv)
}
