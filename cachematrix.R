makeCacheMatrix <- function(x = numeric()){   ##assumption that x is a square inversible matrix
    
  inv <- NULL ## inv stores the inverse and is reset to NULL when makeCacheMatrix is called
  
 
  get <- function() x   ## this function returns the original matrix
      ## get() is not run when makeCacheMatrix is called. Instead it is used by cacheSolve()
  
  set <- function(y){ ## this function allows another cacheSolve to be run on another matrix
    x <<- y         ## without having to rerun makeCacheMatrix. So if the first run on matrix 
    inv <<- NULL    ## m1 is b <- makeCacheMatrix(m1). Then for matrix m2 just do b$set(m2) and                      ## then cacheSolve. Then can do m$set(matrix("newmatrix")) and only need to
    ## then run cacheSolve(b) which is more efficient. This is lexical scoping!
  }
    
  setinverse <- function(inverse) inv <<- inverse   ## set outside environment by superassignment
      ## setInverse is not run when makeCacheMatrix is called. Instead it used by cacheSolve()
      ## during the first cacheSolve for a particular matrix
  
  getinverse <- function() inv   ## returns the cached value to cacheSolve on subsequent occasions
                                 ## that cacheSolve() is called for a particular matrix
  
  list (get = get, set= set, setinverse = setinverse, getinverse = getinverse)
      ## this list is returned with the newly created object
      ## the list lists all the functions (methods) that are part of the object
      ## a function needs to be on the list so that it can be accessed externally
}

cacheSolve <- function(x, ...){   ## the input is an object created by makeCacheMatrix
                                  ## this object has two variables: 
                                  ## the original input matrix and the inverse variable and 
                                  ## a list of the internal functions (methods)
                                  ## NB x for cacheSolve is the object created by makeCacheMatrix,
                                  ## not the original matrix which is x in makeCacheMatrix
  
  inv <- x$getinverse()           ## x refers to object, not the original matrix
  
  if(!is.null(inv)){   ## if the inverse already cached is not null
      message("getting cached data")   ## send message to the console, and
      return(inv)                      ## return inverse; return ends the function cacheSolve
  }
  data <- x$get()   ## this code is only executed if x$getinverse returned null
  inv <- solve(data)  ## calculates inverse if needed
  x$setinverse(inv)         ## stores inverse in x
  inv                       ## return inverse to the code that called this function
}