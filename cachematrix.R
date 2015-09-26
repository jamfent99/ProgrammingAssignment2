## cachematrix.R:  This file contains a set of functions that enable the caching of 
##  of the inverse of the input matrix.
##  usage:
##  1)  matrix1 <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
##  2)  matrix1get()  will print out input matrix.
##  3)  cacheSolve(matrix1)   creates/retrieves/outputs inverse matrix.
##  4)  "martrix1$get()  %*% cacheSolve(matrix1)" prints out identity matrix.  Example:
##                 [,1] [,2]
##              [1,]    1    0
##              [2,]    0    1
##       my_matrix <- (identity matrix of my_matrix)  %*%  (my_matrix)

##  function makeCacheMatrix:  Sets up a list of functions (set, get, setinvmat, getinvmat) 
##  for manipulating an input matrix and its inverse.  set and get are used to save and 
##  retrieve the input matrix.  The value of "im" tells the cacheSolve function to either 
##  use precomputed inverse or calculate the inverse if "im == NULL".  setinvmat and 
##  getinvmat cache or retrieve the computed inverse.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinvmat <- function(invmat) im <<- invmat
        getinvmat <- function() im
        list(set = set, get = get,
             setinvmat = setinvmat,
             getinvmat = getinvmat)
}


##  cacheSolve function:  uses the R solve command to compute the inverse of the 
##  input square Matrix.  Could use the ginv() function from the MASS package to allow
##  use of non-square matrices as input.  The inversed matrix is cached for future use
##  until a new matrix is input.  Precomputed inverse is used if "im != NULL".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinvmat()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinvmat(im)
        im
}


