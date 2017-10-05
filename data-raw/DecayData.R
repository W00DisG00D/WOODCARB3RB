hl <- halfLives <- read.xlsx("../inst/extdata/CopyOfData/halfLives.xlsx", 1,header=FALSE,
                             colIndex = c(1,2,3,5,6,7,8,10,11,12,14,15,16,17))#halflives data load will need speific location and formatting

## BUILDING DECAY ARRAY BASED ON HALFLIVES DATA
decays <- 3 #decay type, 1 = exponential, 2 = k=2, 3 = k=10
enduses <- 13
years <- 151
decay_array <- array(0,dim=c(decays, enduses, years, years))

## FUNCTION SETUP NOTE - DIFFERENT THAN LOOKUP TABLES
g <- function(x)
{
  ((x^(k-1))*(exp(-x/h)))/(gamma(k)*(h^k))
}

G2 <- function(h)
{
  g <- function(x)
  {
    ((x^(k-1))*(exp(-x/h)))/(gamma(k)*(h^k))
  }
  return(g)
}

gh <- function (h)
{
  integrate(G2(h),  lower=0, upper=hl[i,j])$value - .5 #different upper limit in functiuon definition
}

## EXPONENTIAL
k <- 1
for (i in 1:years)
{
  for (j in 1:enduses)
  {
    h <- hl[i,j] / log(2)
    for (l in 1:(years - i + 1))
    {
      decay <- integrate(g, lower=0, upper=l)$value
      decay_array[1, j, i, (i + l - 1)] <- 1 - decay
    }
  }
  #print(i)
}

## K = 2
k <- 2
for (i in 1:years)
{
  for (j in 1:enduses)
  {
    h <- uniroot(gh, lower = .1, upper = hl[i,j], tol = 1e-14)$root
    for (l in 1:(years - i + 1))
    {
      decay <- integrate(g, lower=0, upper=l)$value
      decay_array[2, j, i, (i + l - 1)] <- 1 - decay
    }
  }
  #print(i)
}

## K = 10
k <- 10
for (i in 1:years)
{
  for (j in 1:enduses)
  {
    h <- uniroot(gh, lower = .1, upper = hl[i,j], tol = 1e-14)$root
    for (l in 1:(years - i + 1))
    {
      decay <- integrate(g, lower=0, upper=l)$value
      decay_array[3, j, i, (i + l - 1)] <- 1 - decay
    }
  }
  #print(i)
}

devtools::use_data(decay_array, internal = TRUE, overwrite = TRUE)



