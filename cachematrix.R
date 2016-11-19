## caching the inverse of a matrix 

## function that returns a list of functions that get / set the value of an invertible matrix 
#and set/get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #set inverse of matrix (initially) to NULL
  i <- NULL
  
  # set the value of the matrix if x is initially NULL
  setmvalue <- function(m) {
    #set value    
    x <<- m
    #inverse still null at this point
    i <<- NULL        
  }
  
  #get value of matrix
  getmvalue <- function() x
  
  #assign the value of the inverse of matrix
  assigninverse <- function(inverse) i <<- inverse
  
  #retrieve the value of the inverse
  retrieveinverse <- function() i
  
  #return special matrix functions
  list(setmvalue=setmvalue, getmvalue=getmvalue,
       assigninverse=assigninverse,
       retrieveinverse=retrieveinverse)
   
}


## calculating the inverse of a matrix by checking initially if it has not been already calculated with its value cached

cacheSolve <- function(x, ...) {
  #get inverse of matrix
  i <- x$retrieveinverse()
  
  #check if inverse is already cached
  if(!is.null(i)){
    message("getting cached inverse matrix")
    
    return(i)
  }
  
  #if not get matrix
  mat <- x$getmvalue()
  
  #calculate inverse
  i <- solve(mat)
  
  #assign new value of inverse for future use
  x$assigninverse(i)
  
  #return inverse
  i
}
