

##    These functions take a nonsingular matrix and retrun the inverse.  This set off functions
##w   will check to see if there is already an inverse computed and return it if found, otherwise
##   it will compute the inverse of the matrix.



# #   This function accepts the user defined matrix and creates the functions to get and
##    set the mantrix and its inverse.

##  For example creating the functions to finding or compute the inverse of a matrix
## x<-matrix(c(1,3,2,4),2,2)
## mat.inv<-makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
  #check for square and non sigular
  if(dim(x)[1]!=dim(x)[2]) stop("Requires a Square Matrix")
  if(sum(ifelse(eigen(x)$values==0,1,0))!=0) stop("Requires a non-singular matrix")
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<-function(inv) m<<-inv
  getinv<-function() m
  list(set=set, get=get,
       setinv=setinv, getinv=getinv)
}



##  This function will check to see if the value of the inverse is already in memory.  If 
##  it is, it will retrieve and display it, other wise, it computes the inverse of the matrix.
##  Example of use:  cacheSolve(mat.inv)   where mat.inv was obtained from the function above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinv(m)
  m
  
}


## Examples of how to use the above fundtions
x<-matrix(c(1,2,5,4),2,2)
mat.inv<-makeCacheMatrix(x)
mat.inv$get()
mat.inv$getinv()
  
cacheSolve(mat.inv)



