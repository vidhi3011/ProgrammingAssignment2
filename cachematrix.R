## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.

  makeCacheMatrix <- function(x = matrix()) {            ## define the argument with default mode of "matrix"
    inv <- NULL                                    ## initialize inv as NULL 
    
    #set the value of the Matrix
    setMatrix <- function(y) {                           ## define the set function to assign new 
      x <<- y                                            
      inv <<- NULL                                      ## if there is a new matrix, reset inv to NULL
    }
    
    getMatrix <- function() x                              
    setInv <- function(inverse) inv <<- inverse  
    getInv <- function() inv                     
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
    
  }
  

## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'


#get the value of the invertible matrix from the makeCacheMatrix function
inv <- x$getInverse()
if(!is.null(inv)) {                       #if inverse matrix is not NULL
  message("Getting Cached Invertible Matrix")   #Type a suitable message 
  return(inv)                             #to return the inverted matrix
}

#else if the inv value is NULL  
MatrixData <- x$getMatrix()                     #fetch original Matrix Data 
inv <- solve(MatrixData, ...)             # get inverse of matrix
x$setInverse(inv)                         #set the invertible matrix 
return(inv)                               #return the invertible matrix
} 

  
  
  
  
  