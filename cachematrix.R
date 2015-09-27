#Creation of the the matrix and list

makeCacheMatrix <- function(inputMat = matrix()) {
  
  ## inputMat: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##        this list is used as the input to cacheSolve() 
  ##        cacheSolve() calculates the 
  ##        matrix Inversion based on if its already been calculated or not.
  
  matInv<-NULL
  set<-function(y){
    inputMat<<-y
    matInv<<-NULL
  }
  get<-function() inputMat
  setmatrix<-function(solve) matInv<<- solve
  getmatrix<-function() matInv
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(inputMat=matrix(), ...) {
  matInv<-inputMat$getmatrix()
  if(!is.null(matInv)){
    message("getting cached data")
    return(matInv)
  }
  matrix<-inputMat$get()
  matInv<-solve(matrix, ...)
  inputMat$setmatrix(matInv)
  matInv
}