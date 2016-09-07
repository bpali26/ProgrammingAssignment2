
## 1. The arguement of the makeCacheMatrix function is a matrix class object which 
##    it uses to return a list of four functions viz. set,get,setinversex,getinversex.
## 2. cacheSolve function however,takes the arguement as list returned by makeCacheMatrix.
## 3. cacheSolve first checks whether the list arguement is previously solved or not.
## 4. This is done by getting the inverse value of the list arguement using list's getinversex function..
## 5. if previously solved(i.e. inverse is not NULL), it returns the cached value of inverse,
##    ,using the saved value in makeCacheMatrix(by the help of <<- operatorhelp).
## 6. Otherwise, it calculates the inverse for the matrix and 
##    sets (or save) the value in list's setinversex subfunction 
## 7. and then, prints out the inverse.   
## 8. In this way, if the inverse is already solved(previously), the cacheSolve 
##    does not solve for it. It just passes the cache value with the message--"getting cached data"
## 9. This helps in saving the "matrix inverse" calculation time. 

## Takes a matrix 'x' as arguement & Return a List of four basic sufunctions
makeCacheMatrix <- function(x = matrix()) {  
   inverse <- NULL
   set <- function(y) {
   x <<- y                                 ##assigning matrix y to x without including x in set
                                           ##function environment(using <<- operator)
   inverse <<- NULL                        ##setting inverse as NULL without including inverse
   }                                       ##(using <<- operator)in set function environment
   get <- function() x
   setinversex <- function(inversex) inverse <<- inversex
   getinversex <- function() inverse
   list(set = set, get = get,setinversex = setinversex,getinversex = getinversex)
}

## Takes a List as arguement(structure identical to the return of makeCacheMatrix)
## & Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {   
   inverse <- x$getinversex()              ##getting the stored value of inverse
   if(!is.null(inverse)) {                 ##Applying if statment if inverse is not NULL
     message("getting cached data")        
     return(inverse)                       ##returning previously cached value of inverse
   }
   mat <- x$get()                          ##assigning original matrix to mat object
   inverse <- solve(mat, ...)              ##calculating inverse
   x$setinversex(inverse)                  ##setting the inverse value using list's setinversex function
   inverse                                 ##printing newly calculated value
}
