## This code contains two functions accomplishing the following:
##- makeCacheMatrix: This function creates a special "matrix" object that...
##...can cache its inverse
##- cacheSolve: This function computes the inverse of the special "matrix"...
##...returned by makeCacheMatrix


## The objective is to create a special "matrix" object that can cache its inverse
## Use makeCacheMatrix to instanciate a matrix:
## For instance this sets a 3x3 indentity matrix:
## mat <- makeCacheMatrix(matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, ncol = 3, byrow = TRUE, dimnames = list(c("row1", "row2", "row3"), c("col1", "col2", "col3"))))
## Call the inverse matrix using cacheSolve(mat), which will return the inverse from the cache if it has been cached previously

makeCacheMatrix <- function(x = matrix()) {
        ## Instantiate to NULL the variable that will receive the matrix's inverse
        minv <- NULL
        
        ## Define the set function to cache values by substitution if a new value is set
        set <- function(y) {
                x <<- y
                ## R-initialize minv to NULL
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
        
        ## This list stores the 4 functions that have been defined in the main function
        ## Those functions can be accessed by:
        ## Creating a matrix: mat <- makeCacheMatrix(matrix(c(), nrow = , ncol = , ...))
        ## calling the object follwed by $function_name(): mat$get()
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
                message("Returning cached data")
                return(minv) 
        }
        
        ## else
        message("First time calculating this inverse, the result was not in the cache")
        data <- x$get()
        
        ##inverse the matrix
        minv <- solve(data)
        
        ##set the matrix's inverse
        x$setinverse(minv)
        
        ##return the matrix
        return(minv)
}
