set.seed(171111)

gen.mat <- function(dim, min=0, max=1){
  mat <- matrix(sample(min:max, dim^2, replace=TRUE),dim )
  rownames(mat) <- LETTERS[1:dim]
  colnames(mat) <- LETTERS[1:dim]
  mat
}

adjmat <- gen.mat(dim=5)

adjmat

matrix.apply <- function(X, MARGIN, FUN){
  if(MARGIN==1) t(apply(X, MARGIN, FUN))
  else apply(X, MARGIN, FUN)
}

leading.one <- function(X, MARGIN){
  cummax.one <- matrix.apply(X, MARGIN, cummax)
  remove.tail <- function(x)c(x[1],diff(x))
  matrix.apply(cummax.one, MARGIN, remove.tail)
}

adjmat
leading.one(adjmat,1)
leading.one(adjmat,2)

adjmat <- gen.mat(dim = 5, min=0, max=2)

leading.num <- function(X,MARGIN){
  mask <- ifelse(X != 0,1,0)
  ifelse(leading.one(mask, MARGIN)==1, X, 0)
}

adjmat
leading.num(adjmat, 1)
leading.num(adjmat, 2)
