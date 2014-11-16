makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                     #initialize m so cache is not pulled
  set <- function(y) {          #function to modify the existing matrix
    x <<- y                     #modify the matrix
    m <<- NULL                  #initialize m so cache is not pulled
  }
  get <- function() x           #function to return the matrix
  setinverse <- function(inverse) m <<- inverse #function to cache m
                                                #in the parent env 
  getinverse <- function() m    #function to return the inverse
  list(set = set, get = get,    #list of functions to reference with $
       setinverse = setinverse,
       getinverse = getinverse)
}