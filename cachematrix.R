## Below functions help in caching inverse of a given matrix. Computation of inverse of a matrix is time consuming. 
## Use these functions when dealing with large matrix inverse computation.

## Prepares and returns a list where each element points to functions that helps in dealing with caching a given matrix X.
makeCacheMatrix <- function(myMatrix = matrix()) {
  inverseMatrix <- NULL
  # Function to set matrix to be inverted
  set <- function(matrixParam) {
    myMatrix <<- matrixParam
    inverse <<- NULL
  }
  # Function to get input matrix
  get <- function() myMatrix
  # Function to set inverse of a matrix directly
  setInverse <- function(inverse) inverseMatrix <<- inverse
  # Function to get inverse matrix
  getInverse <- function() inverseMatrix
  # List pointing to each method
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Function to get inverse of a matrix. This function caches inverse of a matrix to improve performance.
cacheSolve <- function(x, ...) {
  ## Try to get inverse of a matrix.
  inverseMatrix <- x$getInverse()
  ## If inverse is already computed, return cached data
  if(!is.null(inverseMatrix)){
    message("getting cached data")
    return(inverseMatrix)
  }
  
  ## Inverse doesnot exist in cache. Compute inverse, store it in cache for future calls and then return inverse.
  matrix <- x$get()
  inverseMatrix <- solve(matrix, ...)
  x$setInverse(inverseMatrix)
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix
}