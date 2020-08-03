makeCacheMatrix <-function(x = matrix()){
  inv <- NULL #initializing Matrix
  set <- function(y){
    x <<-y
    inv <<-NULL
  }
  get <- function() {x} #function to get matrix x
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv} #fubction to get inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
CacheSolve <- function(x, ...) #gets cache data
  { 
  inv <- x$getInverse()
  if(!is.null(inv)) #to check inverse is null
    {
    message("getting cached data") 
    return(inv)    #returns inverse value
  } 
  mat <- x$get()
  inv <- solve(mat, ...)  #calculates inverse value
  x$setInverse(inv)
  inv  #Returns inverse of x
}
