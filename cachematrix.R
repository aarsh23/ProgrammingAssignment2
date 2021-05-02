## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL                                ## initialize inv as null to store value of matrix inverse
  set<-function(y){                        ## define the set function to assign new
    x<<-y                                  ## value of matrix in parent environment
    inv<<-NULL                             ## if there is a new matrix, reset inv to null
  }
  get<-function(){x}                       ## define the get function returns value of given matrix to function
  inverseSetting<-function(inverse){inv<<-inverse} ##assigns value of inv in parent environment
  inverseGetting<-function(){inv}                  ## gets the value of inv when called
  list(set=set,get=get,inverseSetting=inverseSetting,inverseGetting=inverseGetting)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## returns a matrix that is inverse of 'x'
  inv<-x$inverseGetting()        ##check if inverse is calculated and retrieved from cache
  if(!is.null(inv)){
    message("Cached data coming")
    return(inv)
  }
  matri<-x$get()                ## otherwise compute inverse
  inv<-solve(matri,...)         ## inverse calculating 
  x$inverseSetting(inv)
  inv
}
