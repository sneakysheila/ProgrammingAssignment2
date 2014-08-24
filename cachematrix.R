## This file contains two functions which used in conjunction with each other efficiently
## return the inverse of a matrix

## The makeCacheMatrix function is a sort of initiator function which returns an object
## the original matrix, the variable for the inverse and the methods which will be called
## in the second function
makeCacheMatrix <- function(x = matrix()){
    
  inv <- NULL  
 
  get <- function() x  
  
  set <- function(y){ 
    x <<- y   
    inv <<- NULL    
  }
    
  setinverse <- function(inverse) inv <<- inverse   
  
  getinverse <- function() inv   
  
  list (get = get, set= set, setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function can be called after the makeCacheMatrix function has been called at
## least once. The input for cacheSolve is the output from the makeCacheMatrix or created using 
## the set function (which is part of the object initially returned by makeCacheMatrix)
cacheSolve <- function(x, ...){   
  
  inv <- x$getinverse()
  
  if(!is.null(inv)){   
      message("getting cached data")   
      return(inv)                      
  }
  
  data <- x$get()   
  inv <- solve(data) 
  x$setinverse(inv)         
  inv                      
}