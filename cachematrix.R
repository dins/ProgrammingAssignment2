## Assignment 2 solution with functions makeCacheMatrix that 
## creates a cacheing object and cacheSolve that can invert a matrix inside
## such object.

## Create a object that is able to cache an output value, 
## but does not allow to mutate the first value.
makeCacheMatrix <- function(input = matrix()) {
  cachedOutput <- NULL
  cachedInput <<- input
  getInput <- function() cachedInput
  setOutput <- function(output) cachedOutput <<- output
  getOutput <- function() cachedOutput
  list(getInput = getInput,
       setOutput = setOutput,
       getOutput = getOutput)
}


## Invert the matrix in cacheMatrix usign the ... params. 
## If cached value exists the that is returned. 
## cacheMatrix must be created using makeCacheMatrix.
cacheSolve <- function(cacheMatrix, ...) {
  inverse <- cacheMatrix$getOutput()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- cacheMatrix$getInput()
  inverse <- solve(data, ...)
  cacheMatrix$setOutput(inverse)
  inverse
}
