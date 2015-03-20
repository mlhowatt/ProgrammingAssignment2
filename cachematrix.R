## The two functions 'makeCacheMatrix' and 'cacheSolve' together calculate the inverse of matrix
## and cache the matrix's inverse for use in subsequent calculations
##  
## The required argument to the first function 'makeCacheMatrix' is a invertible matrix and it returns a list
## of functions that is the required argument for the second function 'cacheSolve'
## 
## Example:
## my_matrix<-makeCacheMatrix(rbind(c(1, 2), c(2, 1)))
## cacheSolve(my_matrix)
##
## 'makeCacheMatrix' takes an matrix as input and returns a list of functions that serve
## as an input to the 'solveCache' function. The functions defined are 'set', 'get', 'setInverse' and
## 'getInverse'. The function also defines two variables (x and m) via the <<- operator
## that can be accessed outside the scope of this function

makeCacheMatrix <- function(x = matrix()) {
       
       #check to ensure input argument is a matrix
       if (!is.matrix(x)){
              print("x is not a matrix")
       }
       
       else{
              ## initialize m with default value NULL
              m <- NULL
              
              ## function to set value of matrix
              ## define x and m as variables that can be accessed outside the scope of the function
              ## x is assigned matrix defined in function argument and m is assigned NULL
              set <- function(y) {
              x <<- y
              m <<- NULL
              }
              
              ## function to get value of the matrix
              get <- function() {
                     x
              }
              
              ##function to calculate the value of the inverted matrix
              setinverse <- function(solve){
              m <<- solve
              } 
              
              ##function to get value of the inverted matrix
              getinverse <- function(){
                     m   
              }
             
              #return functions in list variable  
              return(list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse))
       
       }
       
}

## The 'cacheSolve' function takes the returned list from 'makeCacheMatrix' as its input argument. The function
## checks to see if the inverse of the matrix has been cached. If yes, it returns the cached matrix.
## If the inverse of the matrix has not been cached, it calculates the inverse, caches the result
## and returns the inverse.

cacheSolve <- function(x, ...) {

       ## assign the cached value of the matrix to m if it exists
       m <- x$getinverse()
      
       
       ## if the inverse of the matrix has already been calculated (namely m is not NULL), 
       ## return m
       if(!is.null(m)) {
              
              message("getting cached data")
              return(m)
              
       }
       
       ## else if the inverse of the matrix has not been calculated and cached (namely m is NULL):
       ## assign value of x (matrix) to 'data',
       ## calculate inversion
       ## cache data via the setinverse function
       ## and return m 
       else{
              
              data <- x$get()
              m <- solve(data, ...)
              x$setinverse(m)
              message("caching data")
              return(m)
              
       }
}
