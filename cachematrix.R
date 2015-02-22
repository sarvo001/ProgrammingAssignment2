## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix is a function which is a frame of other functions
# * setMatrix
# * getMatrix
# * setcacheValue (invers of matrix)
# * getCacheValue (invers of matrix)
###############################################
makeCacheMatrix <- function(x = numeric()) {
  
  # initialize cache value    
  cache <- NULL
  
  # function stores the matrix
  ###############################################
  setMatrix <- function(newValue) {
    # newValue will store in cache
    x <<- newValue
    # Cache flushing
    cache <<- NULL
  }
  
  # funtion return stored matrix
  ###############################################
  getMatrix <- function() {
    x
  }
  
  # function cache given parameter
  ###############################################
  setcacheValue <- function(solve) {
    cache <<- solve
  }
  
  # function retern cached value
  ###############################################
  getCacheValue <- function() {
    cache
  }
  
  # return the list of given functions
  ###############################################
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix, 
       setcacheValue = setcacheValue, 
       getCacheValue = getCacheValue
  )
}

# cacheSolve is a function which calculate the invers of a square invertible matrix
# with function makeCacheMatrix
###############################################
cacheSolve <- function(y, ...) {
  
  # get value from function makeCacheMatrix, 
  # if its already cached
  ###############################################
  inverse <- y$getCacheValue()
  
  # check if value is in cache
  ###############################################
  if(!is.null(inverse)) {
    # return message and cached value
    message("getting cached data")
    return(inverse)
  }
  
  # get matrix from function makeCacheMatrix
  ###############################################  
  data <- y$getMatrix()
  
  # calculate the invers
  ###############################################
  inverse <- solve(data)
  
  # save result in cache via makeCacheMatrix function
  ###############################################
  y$setcacheValue(inverse)
  
  # return the inverse
  ###############################################
  inverse
}
