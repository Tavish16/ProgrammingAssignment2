## The code consists of two functions makeCacheMatrix(),cacheSolve().
## makeCacheMatrix() creates a matrix and cacheSolve() will be used to 
## calculate the inverse of this matrix and cache its value.

## makeCacheMatrix() is basically a function consisting of 4 functions

## set(),get(),setmatrix(),getmatrix() 

## set() is used for creating a matrix, get() is used for 

## retrieving this matrix...setmatrix() and getmatrix()

## are similar to the 

## setmean and getmean functions in the example of 

## "CACHING THE MEAN OF A VECTOR". 

## These four functions are stored in the form of a list.
   
makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) { 
                    x <<- y
                    m <<- NULL
            }
            get <- function() x 
            setmatrix <- function(solve) m <<- solve
            getmatrix <- function() m
            list(set = set, get = get,
                 setmatrix = setmatrix,
                 getmatrix = getmatrix)
    }

## cacheSolve basically computes inverse of the matrix and caches it.
## if inverse has already been computed, cacheSolve() will not compute
## it again. Instead, it will simply return the precalculated inverse
## along with a message "getting cached data".

cacheSolve <- function(x=matrix(), ...) {
            m <- x$getmatrix() 
            if(!is.null(m)) { 
                                  ## if m is not null i.e inverse has
                                  ## already been found, the inverse 
                                  ## calculated before 
                                  ## will be returned along with the                             
                                  ## message("getting cached data")
                                   
                                  
                    message("getting cached data")
                    return(m)
            }
            matrix <- x$get() 
            m <- solve(matrix, ...) ## calculates and returns inverse.
            x$setmatrix(m)
            m
    }


## To check for results, 

## first type "a <- makeCacheMatrix()" and then type "a$set(matrix())" 

## to create a matrix of your choice

## Note that matrix has to be invertible i.e square and singular

## A valid example is "a$set(matrix(1:4,2,2))"

## Use "a$get()" {make sure to put () after a$get} to retrieve your 

## matrix. Then type "cacheSolve(a)" to calculate inverse of this 

## matrix. The inverse will be returned.

## If you type "cacheSolve(a)" again, this time the

## precalculated inverse will be

## returned along with a message "getting cached data".
