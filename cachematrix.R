makeCacheMatrix <- function(x = matrix()) {   ## creates a function called makeCacheMatrix which has a matrix as an argument
  
  
  inv = NULL                                  ## set the variable inv to null
  set = function(y) {                         ## sets the matrix to an object created by the makeCacheMatrix function
    x <<- y                                   ## sets the matrix as an internal variable which is not exposed to the outside environment 
    inv <<- NULL                              ## set the variable inv to null
  }
  get = function() x                          ## returns the result of the set   
  setinv = function(inverse) inv <<- inverse  ## sets the inverse matrix for matrix x
  getinv = function() inv                     ## gets the inverse matrix
  list(set=set, get=get, setinv=setinv, getinv=getinv) ##creates a list with the 'set' and 'get' functions for the matrix and the inverse matrix
}

cacheSolve <- function(x, ...) {              ## creates a function call cacheSolve that gets the inverse matrix from the cache, or creates it if its not there

  inv = x$getinv()
  
  if (!is.null(inv)){                         ## checks if the inverse matrix has been created 
    message("getting cached data")            ## if it is then it returns a message 'getting cached data'
    return(inv)                               ## returns the cached data, ie the inverse matrix
  }
  
  mat.data = x$get()                          ## if the inverse matrix hasn't been created, then the the x#get() calls matrix x 
  inv = solve(mat.data, ...)                  ## solve function creates the inverse matrix for matrix x
  
  x$setinv(inv)                               ## sets the inverse matrix to the object inv
  
  return(inv)                                 ## returns the object inv
}


                                              ## tests whether the functions work

r <- c(5, 4, 3, 2)                            ## creates a character list 
mat1 <- matrix(r, nrow=2, ncol=2)             ## create a matrix called mat1 with two rows and two columns populated with the values from r
mat1                                          ## return the matrix mat 1
solve(mat1)                                   ## get the inverse matrix of the matrix mat1
temp <- makeCacheMatrix(mat1)                 ## pass the values from makeCacheMatrix for matrix mat1 to an object temp
cacheSolve(temp)                              ## returns the value of the temp object which is the inverse of the matrix


