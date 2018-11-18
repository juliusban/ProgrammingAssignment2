## makeCacheMatrix will store the inverse of a invertable square matrix
##that it received from cacheSolve.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {   ## Creating and setting a matrix that
     inv <- NULL                              ## is empty.
     
     testmatrix <- function(y) {              ##Setting the route of our matrix to 
         x <<- y                              ##be tested.
         inv <<- NULL                         ##
     }                                        ##
     i <- function() x                        ##
     
    
     cache_inverse <- function(inverse) inv <<- inverse    ##receiving(from cacheSolve) and 
     matrixinverse<- function()inv                         ##Storing the inverse of the matrix.
    
     list(inv = inv,
          i = i,
          cache_inverse =cache_inverse,
          matrixinverse = matrixinverse)


}


## cacheSolve will give the inverse of a invertible square matrix using solve()
##and send the inverse to  makeCacheMatrix.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'     
     
     inv <- x$matrixinverse()               ##Testing if there is a stored inverse       
     if (!is.null(inv)) {                   ##for the matrix that we call. 
         message("getting cached data")     ##"getting cached data" will be displayed if there is a 
         return(inv)                        ##stored inverse matrix and that matrix will be called.
     }
    
     mat <- x$i()                            
     inv <- solve(mat, ...)                 ##Calculating the inverse of the test matrix
     x$cache_inverse(inv)                   ##and sending it to makeCacheMatrix to be stored        
     inv                                    ##then display the inverse of the matrix.
     

}
 

