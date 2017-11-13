

## This function takes a matrix as an argument. 
#It then creates an S3 objects that contains function to access and set the matrix and its inverse
makeCacheMatrix <- function(x)
{
  inverseMatrix <- NULL
  
  getMatrix <- function() x
  
  setMatrix <- function(y)
  {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  getInverseMatrix <- function() inverseMatrix
  
  setInverseMatrix <- function(matrixSolved) 
  {
    inverseMatrix <<- matrixSolved
  }
  
  list(getMatrix=getMatrix, setMatrix=setMatrix, getInverseMatrix = getInverseMatrix, setInverseMatrix = setInverseMatrix )
  
}

## This function first tries to access the cached inverse of a given matrix. 
# If it finds it, it returns it. If it doesn't find it it calculates it (using the solve()), then returns it
# And finally set the inverse Matrix inside the original S3 object (caching) created with the makeCacheMatrix function

cacheSolve <- function(x)
{
  inverseMatrix <- x$getInverseMatrix()
  
  if(!is.null(inverseMatrix))
  {
    message("Getting cached data")
    return(inverseMatrix)
  }
  
  data <- x$getMatrix()
  inverseMatrix <- solve(data)
  x$setInverseMatrix(inverseMatrix)
  
  
  
  inverseMatrix
  
}



## Here are the functions in action


# Creates the matrix
myMatrix <- makeCacheMatrix(matrix(c(2,3,9,12,78,98,100,23,86), nrow=3))

# First call of the cacheSolve function : tries to access the inverse of the matrix, doesn't find it, calculates, caches it and returns it
cacheSolve(myMatrix)


# Second call of the cacheSolve function : directly returns the inverse of the matrix that was cached
cacheSolve(myMatrix)


















