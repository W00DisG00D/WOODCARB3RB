#' Calculate total carbon in solidwood products.
#'
#' Calculates total carbon and carbon by end use in solidwood products. Capable of
#' calculating carbon totals with different approaches, halflives, and decay distributions.
#'
#' All units returned are in od tons of wood fiber.
#'
#' @param Yrs years to calculate totals for.
#' @param decaydistribution type of decay distribution.
#' @param onlytotal
#' if false, total carbon is returned for each one of the 13 end uses.
#' if true, one total is summed and returned
#' @param lumberpre if false, carbon from lumber before 1900 is excluded from totals
#' @param approach approach type to use to calculate.
#' If onlytotal is false then the data returned won't correspond to data in WOODCARB II
#' If onlytotal is true:
#' For production approach, the data returned corresponds to the `SWCalcP$H4389` column in WOODCARB II.
#' For stock change approach, the data returned corresponds to the `SWCalc$H4389` column in WOODCARB II.
#' @param halflives data frame of half lives for end uses. Default half lives are used but
#' a data frame with 13 columns with half lives for the appropriate years can be used.
#' @param fsp Fraction of structural panel products that go to each end use.
#' Default can be substituted with data with a column for each end use and row for years
#' from 1900 to latest year of interest.
#' @param fnsp Fraction of non-structural panel products that go to each end use.
#' Default can be substituted with data with a column for each end use and row for years
#' from 1900 to latest year of interest.
#' @param fsawn Fraction of sawnwood products that go to each end use.
#' Default can be substituted with data with a column for each end use and row for years
#' from 1900 to latest year of interest.
#' @param swpdata Optional argument to provide swp production and trade time series
#' for purposes of error analysis
#'
#' @return Output is either a matrix with 13 columns or 1 column based on the onlytotal argument
#' @export
#'
#' @examples
#' swpcarbontotal(c(1990,2000,2010), onlytotal=FALSE)
#' swpcarbontotal(1950:1960, approach = "Stock Change")
swpcarbontotal <- function(Yrs = 1990:2015, decaydistribution = c("Exponential", "K=2", "K=10"),
                           onlytotal=TRUE, lumberpre = TRUE, approach = c("Production",
                                                                          "Stock Change"),
                           halflives = hl, fsp_function = fsp,
                           fnsp = fnonsp,
                           fsawn = fsw,
                           swpdata){

  type <- match.arg(decaydistribution)
  approachtype <- match.arg(approach)

  if (missing(swpdata))
  {
    swpdata <- calculateswpdata()
  }

  placeIU <- calcplacediu(total = FALSE, approach = approachtype, fsp = fsp,
                          fnsp = fnsp,
                          fsawn = fsawn, swpdata = swpdata)


  Var2_totalC_SWP <- data.frame(Years = Yrs)

  for(year in Yrs){
    yearrange <- 1:(year - minyr + 1) #number of years from 1900 to year
    for (eu in 1:13) {
      if (type == "Exponential") {
        decays <- decay_array[1, eu, yearrange, year - minyr + 1]
      }
      if (type == "K=2"){
        decays <- decay_array[2, eu, yearrange, year - minyr + 1]
      }
      if (type == "K=10"){
        decays <- decay_array[3, eu, yearrange, year - minyr + 1]
      }
      Var2_totalC_SWP[Var2_totalC_SWP$Year == year, paste("EU",eu,sep="")] <- sum(placeIU[yearrange,eu+1]*decays
                                                                                  * (1 - lossIU[yearrange,eu]))
    }

  }

  Var2_totalC_SWP[,"lp1900"] <- lp1900[Yrs - minyr + 1,"Carbon.Lumberwood"]
  Var2_totalC_SWP[,"Total Carbon"] <- rowSums(Var2_totalC_SWP[,-1])
  if(onlytotal == TRUE && lumberpre == FALSE){
    return(Var2_totalC_SWP$`Total Carbon` - Var2_totalC_SWP$lp1900)
  }
  else if (onlytotal == TRUE){
    return(Var2_totalC_SWP$`Total Carbon`)
  }
  else{
    Var2_totalC_SWP
  }
}

