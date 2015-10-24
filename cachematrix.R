##########
# Author: Seth Bassett
# Date: 24 October 2015
# Organization: Florida Geological Survey
# Context: Coursera Data Science,
#            Module #2 (R Programming), 
#               Programming Assignment #2
#
# Special matrices objects for caching matrix transpositions
# Useful for optimizing multiple or sequential calls to t() by caching output
#
##########

# Creates a makeCacheMatrix object

makeCacheMatrix <- function(x = matrix()) {
  # param x (matrix) a matrix object
  
  #create transposition variable
  trans <- NULL
  
  #set function - sets matrix and transposition variables
  set <- function(y) {
    x <<- y
    trans <<- NULL
  }
  
  #return original matrix (x)
  get <- function() x
  
  #calculate transposition of original matrix and cache (x)
  settrans <- function(t) trans <<- t
  
  #return cached transposition variable
  gettrans <- function() trans
  
  #returns list of functions, accessible via '$' operator
  list(set = set, 
       get = get, 
       settrans = settrans, 
       gettrans = gettrans)
}



# Sets a cache matrid for a makeCacheMatrix object 
# OR returns a previously cached transposition
cacheSolve <- function(x, ...) {
  # param x (makeCacheMatrix) a makeCacheMatrix object
  
  # call the transposition from makeCacheMatrix object
  trans <- x$gettrans()
  
  # if cached transposition is not null
  # send message to console that the return is a cached value
  # and return cache
  if(!is.null(trans)) {
    #message console
    message("getting cached transposition")
    
    #break and exit function here by returning cached transposition
    return(trans)
  }
  
  #otherwise, calculate transposition and return
  data <- x$get()
  trans <- t(data, ...)
  x$settrans(trans)
  trans
}
