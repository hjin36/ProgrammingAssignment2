## Put comments here that give an overall description of what your
## functions do
## overall, this file contains two functions that creates a special version of a matrix and the other function calculate/store the matrix's inverse


## Write a short comment describing this function
## This function create a special matrix, which is really a list containing a function to:
### 1. set the value of the matrix
### 2. get the value of the matrix
### 3. set the inverse of the matrix
### 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inverse <<- null
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function calculates the inverse of the special matrix created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the mean in the cache via the setmean function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if (!is.null(inv)){
 		message("getting cache data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinverse(inv)
	inv
}
