cacheSolve <- function(x, ...) {
  m <- x$getinverse()               #get the value of the stored inverse
  if(!is.null(m)) {                 #is it not NULL (i.e., cached)?
    message("getting cached data")  #message
    return(m)                       #pass m to cacheSolve
  }
  data <- x$get()                   #get the original matrix
  m <- solve(data, ...)             #invert it
  x$setinverse(m)                   #cache the inverse
  m
}