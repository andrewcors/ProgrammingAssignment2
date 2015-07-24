##These functions provide a convenient way to return the inverse of a matrix 
##and to ensure that the inverse is computed only once via caching


##makeCacheMatrix creates a "CacheMatrix" object that provides functions to 
##1)set the matrix 
##2)get matrix
##3)set the inverse of the matrix
##4)get the inverse of the matrix
 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL 
	set<-function(y){
	x <<- y
	inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<-inverse
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}




## cacheSolve takes a "CacheMatrix" as created by makeCacheMatrix and returns the inverse of the matrix.
## This is accomplished by using the getInverse function to check if the inverse has already been computed, 
##and if so, to return the inverse (along with a message indicating that the value was from the precomputed cache)
##If the inverse has not been computed, cacheSolve computes the inverse via the solve function, 
##sets the inverse of the "CacheMatrix" via the setInverse function, and returns the inverse

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)){
	message("getting cached data")
	
	## Return a matrix that is the inverse of 'x'
	return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	##set the cached inverse of 'x'
	x$setInverse(inv)
	
	##return the inverse of 'x'
	inv

}
