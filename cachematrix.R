# cachematrix.R is R function for caching the inverse of matrix. Solution is implemented trough two functions:

# 1) makeCacheMatrix - creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	invM <- NULL
	#Set the value of the matrix
	set <- function(y) {
		x <<- y
		invM <<- NULL
	}
	#Get the value of the matrix	
	get <- function() x
	#Set the value of the inverse matrix
	setinv <- function(inverse) invM <<- inverse
	#Get the value of the inverse matrix
	getinv <- function() invM
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# 2) cacheSolve - computes the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
  invM <- x$getinv()
  #First checks if the inverse matrix has been computed
  if(!is.null(invM)) {
    #If variable invM is not empty, computation will be skipd
    message("Getting cached data.")
    #Return a matrix that is the inverse of 'x'
    return(invM)
  }
  #If not, it computes the inverse, sets the value in the cache via setinv function.
  dataM <- x$get()
  invM <- solve(dataM)
  x$setinv(invM)
  #Return a matrix that is the inverse of 'x'
  invM
}
