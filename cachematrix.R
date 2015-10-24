## Modelling the inverse function after the example in the course for mean 
## make cache matrix makes a cache 

## Makes a special matrix which caches its inverse - final version

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x <<- y
            m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## this function either computes the inverse or get the cached 
## version of the inverse - final version

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
