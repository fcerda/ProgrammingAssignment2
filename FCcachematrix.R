## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Set a matrix to object created by makeCacheMatrix function
## the, e.g makeCacheMatrix(testmatrix) # the matrix will be called testmatrix
## test will generate a random square matrix
##Input will be of the following form:
## PLEASE TRY THIS INPUT:      test<-matrix(runif(4,1,10),2,2)
## The following conmand will generate the makeCacheMatrix object with this matrix
##                      :      testCached <- makeCacheMatrix(test)
## Calculate or retrieve calculated inversion using the cacheSolve function
##                      :      testInv <- cacheSolve(testCached)
  

makeCacheMatrix <- function(x = matrix()) {

  xinv <- NULL # this is where the result of inversion is stored
  ##
  set <- function(y) {
    x <<- y
    xinv <<- NULL # it will also initialises xinv to null
  }
  
  get <- function() x # return the input matrix
  setInv <- function(inv) xinv <<- inv # set the inversed matrix
  getInv <- function() xinv # return the inversed matrix
  # return a list that contains these functions, so that we can use
  # makeCacheMatrix object like these
  # x <- makeCacheMatrix(testmatrix)
  # x$set(newmatrix) # to change matrix
  # x$get # to get the setted matrix
  # x$setInv # to set the inversed matrix
  # 
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
## The following conmand will generate the makeCacheMatrix object with this matrix
##                      :      testInv <- cacheSolve(testCached)

cacheSolve <- function(x, ...) {
        
  m <- x$getInv() # get the inversed matrix from object x
  # it will be null if uncalculated, remember the first line "xinv <- NULL" in the previous function
  if(!is.null(m)) { # if the inversion result is there
    message("getting cached data")
    return(m) # return the calculated inversion
  }
  data <- x$get() # if not, we do x$get to get the matrix object
  m <- solve(data) # we solve it
  x$setInv(m) # we then set it to the object
  m # return the solved result
  
}
## Calculate or retrieve calculated inversion using the cacheSolve function
##                      :      testInv <- cacheSolve(testCached)

