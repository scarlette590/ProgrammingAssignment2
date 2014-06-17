## Programming Assignment 2: Caching the Inverse of a Matrix
## By scarlette590



## Example of usage:
##
## > source("cachematrix.R")
## > A=matrix(c(4, 3, 3, 2),nrow=2,ncol=2,byrow=TRUE)
## > b <- makeCacheMatrix(A)
##
## > cacheSolve(b)
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
##
## > A %*% cacheSolve(b)
## getting cached data
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > 



## The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
## This is really a list containing functions to:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the matrix inverse
## 4.get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL											#initialize the inverse to NULL
        set <- function(y) {								#function to set the value of the matrix
                x <<- y										
                inverse <<- NULL							
        }
        get <- function() x									#function to return the value of the matrix
        setinverse <- function(inverse) inv <<- inverse		#function to set the value of the inverse
        getinverse <- function() inv						#function to return the value of the inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The second function calculates the inverse of the special "matrix" created with the above function "makeCacheMatrix". 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.
## Returns a matrix that is the inverse of the input 'x'

cacheSolve <- function(x, ...) {   
		inv <- x$getinverse()  					#check if the inverse has already been calculated
		if(!is.null(inv)) {						#if the inverse is there
				message("getting cached data")	#indicate that we are returning cached data
				return(inv)						#return the inverse
		}										#otherwise the inverse is not there
		data <- x$get()							#get the original matrix data				
		inv <- solve(data, ...)					#calculate the inverse
		x$setinverse(inv)						#set the inverse in the special matrix
		inv    									#return the inverse
}
