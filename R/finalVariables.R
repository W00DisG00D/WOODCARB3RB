#' Estimates U.S. HWP contribution to annual greenhouse gas removals.
#'
#' Uses formulas described in WOODCARB II paper to calculate contribution contribution
#' with the HWP variables.
#'
#' Decay type and half life options allow for analysis of contribution levels under
#' different circumstances.
#'
#' The Production Approach focuses on estimating annual carbon stock change in HWP and
#' forests where the carbon is from trees harvested in the reporting country.
#'
#' The Stock Change Approach focuses on estimating annual carbon stock change in HWP and
#' forests in a country regardless of wood origin.
#'
#' The Atmospheric Flow Approach focuses on estimating annual carbon fluxes between
#' the atmosphere and forests/HWPs within a country.
#'
#' @param Years years to return carbon totals for
#' @param approach The approach used to calculate carbon contribution.
#' "Production" approach corresponds to `06 IPCC Tables`$R9 in the WOODCARB II spreadsheet
#' "Stock Change" approach corresponds to `06 IPCC Tables`$P9 in the WOODCARB II spreadsheet
#' "Atmospheric Flow" approach corresponds to `06 IPCC Tables`$Q9 in the WOODCARB II spreadsheet
#' @param decaydistribution Type of decay method to use
#' @param plot if true, returns a simple plot of time vs. carbon contribution.
#' @param halflives data frame of half lives to use. Must have a column associated with each
#' one of 13 end uses and half life values for 1900 until the maximum year of interest.
#' @param paperHL half life value for paper
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
#' @param woodToCarbon Conversion factor to convert oven-dry tons of solid wood to
#' Tg of Carbon.
#' @param paperToCarbon Conversion factor to convert oven-dry tons of paper to
#' Tg of Carbon.
#' @param swpSwdsNondegradable Percent of solid-wood products in landfills and dumps
#' that are not subject to decay.
#' @param paperSwdsNondegradable Percent of paper products in landfills and dumps that
#' are not subject to decay.
#' @param swpLandfillDecay Decay rate of solid-wood products in landfills as a half-life in years.
#' @param paperLandfillDecay Decay rate of paper products in landfills as a half-life in years.
#' @param swpDumpDecay Decay rate of solid-wood products in dumps as a half-life in years.
#' @param paperDumpDecay Decay rate of paper products in dumps as a half-life in years.
#' @return A vector of carbon contributions for all Years. Plot returned is optional
#' All units returned are in Thousand metric tons CO2 Equivalent.
#' @export
#'
#' @examples
#' finalCarbonContribution()
#' finalCarbonContribution(approach = "Stock Change")
#' finalCarbonContribution(approach = "Production",
#'                          decaydistribution ="Exponential")
finalCarbonContribution <- function(Years = 1990:2015, approach = c("Production",
                                                                   "Stock Change",
                                                                   "Atmospheric Flow",
                                                                   "Annual Harvest",
                                                                   "Annual CO2 Release",
                                                                   "Total Contribution"),
                                    decaydistribution = c("Exponential",
                                                  "K=2", "K=10"), plot = FALSE,
                                    halflives = hl, paperHL = 2.53087281800454, fsp = fsp,
                                    fnsp = fnonsp,
                                    fsawn = fsw,
                                    swpdata,
                                    woodToCarbon = 4.535925e-07, paperToCarbon = 3.9008955e-07,
                                    swpSwdsNondegradable = 0.77, paperSwdsNondegradable = 0.44,
                                    swpLandfillDecay = 0.0300063714528115 , paperLandfillDecay = 0.0478032538317204,
                                    swpDumpDecay = 0.0420089200339361 , paperDumpDecay = 0.0840178400678722){
  if (missing(approach)){
    approach = "Production"
  }
  approachtype <- match.arg(approach)
  decay<- match.arg(decaydistribution)

  fvs <- finalVariables(Years, decay, halflives, paperHL = paperHL, approach = approachtype, fsp = fsp,
                        fnsp = fnsp,
                        fsawn = fsawn, woodToCarbon = woodToCarbon, paperToCarbon = paperToCarbon,
                        swpSwdsNondegradable = swpSwdsNondegradable, paperSwdsNondegradable = paperSwdsNondegradable,
                        swpLandfillDecay = swpLandfillDecay, paperLandfillDecay = paperLandfillDecay,
                        swpDumpDecay = swpDumpDecay, paperDumpDecay = paperDumpDecay, swpdata = swpdata)
  if (approachtype == "Atmospheric Flow") {
    contrib <- ((-1*fvs[,"Var1A"]-fvs[,"Var1B"])*44/12)+(fvs[,"Var3"]-fvs[,"Var4"])*44/12
  }
  if (approachtype == "Annual Harvest") {
    contrib <- -1 * fvs[,"Var5"] * 44 / 12
  }
  if (approachtype == "Annual CO2 Release") {
    contrib <- fvs[,"Var7"] * 44/12
  }
  if (approachtype == "Total Contribution") {
    contrib <- (-1 * fvs[,"Var5"] * 44 / 12) + fvs[,"Var7"] * 44/12
  }
  else {
    #First column is 1A/2A for Stock change and production respectively,
    #and second column is 1B/2B for stock change and production respectively.
    contrib <- (-1*fvs[,names(fvs)[grep("A", names(fvs))]]-
                  fvs[,names(fvs)[grep("B", names(fvs))]])*44/12
  }
  r <- list(contrib)
  if (plot == TRUE) {
    #add smoothed line to data w/ ggplot
    p <- plot(Years, contrib, xlab="Years", ylab="Total CO2 Contribution",
              main = "Total Carbon Removals", type="l")
    r <- c(r, p)
  }

  return(unlist(r))
}

#' Calculates the 7 HWP Variables.
#'
#' Returns variables needed to calculate overall contribution.
#'
#' All units are in Gg Carbon.
#'
#' Var1A = Annual Change in stock of HWP in use form consumption
#'
#' Var1B = Annual Change in stock of HWP in SWDS from consumption
#'
#' Var2A = Annual Change in stock of HWP in use produced from domestic harvest
#'
#' Var2B = Annual Change in stock of HWP in SWDS produced from domestic harvest
#'
#' Var3 = Annual Imports of wood, paper products, wood fuel, pulp, recovered paper, roundwood/chips
#'
#' Var4 = Annual Exports of wood, paper products, wood fuel, pulp, recovered paper, roundwood/chips
#'
#' Var5 = Annual Domestic Harvest
#'
#' Var6 = Annual release of carbon to the atmosphere from HWP consumption (from fuelwood & products
#' in use and products in SWDS)
#'
#' Var7 = Annual release of carbon to the atmosphere from HWP (including fuelwood) where wood came from
#' domestic harvest (from products in use and products in SWDS)
#'
#' @param Years years to calculate
#' @param decaydistribution decay distribution to use. Defaults to exponential distribution.
#' @param halflives data frame of half lives to use. Must have a column associated with each
#' one of 13 end uses and half life values for 1900 until the maximum year of interest.
#' @param paperHL half life value for paper
#' @param approach approach to calculate corresponding final variables for.
#' If null, all final variables are calculated and returned.
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
#' @param woodToCarbon Conversion factor to convert oven-dry tons of solid wood to
#' Tg of Carbon.
#' @param paperToCarbon Conversion factor to convert oven-dry tons of paper to
#' Tg of Carbon.
#' @param swpSwdsNondegradable Percent of solid-wood products in landfills and dumps
#' that are not subject to decay.
#' @param paperSwdsNondegradable Percent of paper products in landfills and dumps that
#' are not subject to decay.
#' @param swpLandfillDecay Decay rate of solid-wood products in landfills as a half-life in years.
#' @param paperLandfillDecay Decay rate of paper products in landfills as a half-life in years.
#' @param swpDumpDecay Decay rate of solid-wood products in dumps as a half-life in years.
#' @param paperDumpDecay Decay rate of paper products in dumps as a half-life in years.
#' @return If an approach is specified then only the variables needed for that
#' approach will be returned. If approach is null then a data frame with nine
#' columns (Variables 1 and 2 have two parts each) will be returned. If the decay distribution
#' is exponential then the data frame returned will correspond to table containing
#' final variables: `06 IPCC Tables` in the WOODCARB II spreadsheet.
#' @export
#'
#' @examples
#' finalVariables(Years = 1950:2000)
#' finalVariables(halflives = hl * 1.25)
finalVariables <- function(Years = 1990:2015,
                           decaydistribution = c("Exponential", "K=2", "K=10"),
                           halflives = hl, paperHL = 2.53087281800454, approach = NULL,
                           fsp = fsp,
                           fnsp = fnonsp,
                           fsawn = fsw, woodToCarbon = 4.535925e-07, paperToCarbon = 3.9008955e-07,
                           swpSwdsNondegradable = 0.77, paperSwdsNondegradable = 0.44,
                           swpLandfillDecay = 0.0300063714528115 , paperLandfillDecay = 0.0478032538317204,
                           swpDumpDecay = 0.0420089200339361 , paperDumpDecay = 0.0840178400678722,
                           swpdata){

  decay <- match.arg(decaydistribution)

  if (missing(swpdata))
  {
    swpdata <- calculateswpdata()
  }

  environment(swp_carbon_stockchange) <- environment()
  environment(paper_carbon_stockchange) <- environment()
  environment(carbonfromdumps) <- environment()
  environment(calcP_IM) <- environment()
  environment(calcP_EX) <- environment()
  environment(annualDomesticHarvest) <- environment()

  Var1A <- function(){
    (swp_carbon_stockchange(Years, approach = "Stock Change",
                            decaydistribution = decay,
                            halflives = hl, fsp = fsp,
                            fnsp = fnsp,
                            fsawn = fsawn, swpdata = swpdata) +
       paper_carbon_stockchange(Years,
                                approach = "Stock Change")) * 1000
  }
  Var1B <- function(){
    carbonfromdumps(Years, approach = "Stock Change")  * 1000
  }
  Var2A <- function(){
    (swp_carbon_stockchange(Years, approach = "Production",
                            decaydistribution = decay,
                            halflives = hl, fsp = fsp,
                            fnsp = fnsp,
                            fsawn = fsawn, swpdata = swpdata) +
       paper_carbon_stockchange(Years,
                                approach = "Production")) * 1000
  }
  Var2B <- function(){
    carbonfromdumps(Years, approach = "Production") *  1000
  }
  Var3 <- function(){
    calcP_IM(Years, var = TRUE)
  }

  Var4  <- function(){
    calcP_EX(Years, var = TRUE)
  }

  Var5 <- function(){
    annualDomesticHarvest(Years, onlyvar = TRUE)
  }
  Var6 <- function(){
    Var5() + Var3() - Var4() - Var1A() - Var1B()
  }
  Var7 <- function(){
    Var5() - Var2A() - Var2B()
  }
  if (is.null(approach)) {

    df <- data.frame(Years, Var1A = Var1A(), Var1B = Var1B(),
                     Var2A = Var2A(), Var2B = Var2B(),
                     Var3 = Var3(), Var4 = Var4(),
                     Var5 = Var5())
    return(df)
  }

  if (approach == "Production") {
    df <- data.frame(Years, Var2A = Var2A(),
                     Var2B = Var2B())
    return(df)
  }

  if (approach == "Stock Change") {
    df <- data.frame(Years, Var1A = Var1A(),
                     Var1B = Var1B())
    return(df)
  }

  if (approach == "Atmospheric Flow") {
    df <- data.frame(Years, Var1A = Var1A(),
                     Var1B = Var1B(),
                     Var3  = Var3(),
                     Var4  = Var4())
    return(df)
  }
  return(df)
}

#' Calculates changes in carbon stock from solid wood products.
#' Units are in Tg Carbon.
#' For production approach, values correspond to `Calculation$BY` in the spreadsheet.
#' For stock change approach, values correspond to `Calculation$H` in the spreadsheet.
#'
#' @param years years to return carbon totals for
#' @param approach approach type used to calculate
#' @param decaydistribution type of decay distribution to use
#' @param halflives data frame of half lives to use. Must have a column associated with each
#' one of 13 end uses and half life values for 1900 until the maximum year of interest.
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
#' @param woodToCarbon Conversion factor to convert oven-dry tons of solid wood to
#' Tg of Carbon.
#' @return A vector of swp carbon stock changes for `years`
#' @examples
#'
#' \dontrun{
#' swp_carbon_stockchange(1990:2000)
#' swp_carbon_stockchange(1950:1975, approach = "Stock Change", decaydistribution = "K=2")
#' }
swp_carbon_stockchange <- function(years, approach = c("Production", "Stock Change"),
                                   decaydistribution = c("Exponential",
                                                         "K=2", "K=10"),
                                   halflives = hl, fsp = fsp,
                                   fnsp = fnonsp,
                                   fsawn = fsw, swpdata,
                                   woodToCarbon = 4.535925e-07){
  approach <- match.arg(approach)
  decay <- match.arg(decaydistribution)

  yearss <- (min(years)-1):max(years)

  totals <- data.frame(yearss, carbon = swpcarbontotal(Yrs = yearss, approach = approach, decaydistribution = decay,
                           halflives = hl, fsp = fsp,
                           fnsp = fnsp,
                           fsawn = fsawn, swpdata = swpdata))
  totals[2:length(yearss),"diffs"] <- diff(totals$carbon)
  stockchange <- woodToCarbon * totals[totals$yearss %in% years,"diffs"]
  return(stockchange)

}

#' Calculates changes in carbon stock from paper.
#' Units are in Tg Carbon.
#' For production approach, values correspond to `Calculation$CC` in the spreadsheet.
#' For stock change approach, values correspond to `Calculation$L` in the spreadsheet.
#'
#' @param years years to return carbon totals for
#' @param approach The approach used to calculate carbon contribution.
#' @return A vector of paper carbon stock changes for `years`
#' @examples
#' \dontrun{
#' paper_carbon_stockchange(1990:2000)
#' paper_carbon_stockchange(1950:1975, approach = "Stock Change")
#' }
paper_carbon_stockchange <- function(years, approach = c("Production", "Stock Change")){
  environment(calcUSApaper) <- environment()
  USA <- calcUSApaper(yrs)
  approach = match.arg(approach)
  calcpaper <- function(years, CarbonInputFlowFromPaper){

    papercarbon <- numeric(length(minyr:max(years)))
    for(year in minyr:max(years)){ #Total carbon in paper for year y in Tg C/yr
      if(year == minyr){
        papercarbon[year-(minyr-1)] <- exp(-log(2)/paperHL)*CarbonInputFlowFromPaper[year-(minyr-1)]
      }
      else{
        papercarbon[year-(minyr-1)] <- exp(-log(2)/paperHL)*(CarbonInputFlowFromPaper[year-(minyr-1)]+
                                                             papercarbon[year-minyr])
      }
    }
    if(min(years) == minyr){
      index <- minyr:max(years)
    }
    else{
      index <- (min(years)-1):max(years)
    }

    totals <- papercarbon[index-(minyr-1)]

    return(totals[2:length(totals)] - totals[1:length(totals)-1])
  }

  if (approach == "Production"){
    return(calcpaper(years, USA$`Production Approach-C Input from Paper Products(Calc BU)`)[years-(min(years-1))])
  }

  if (approach == "Stock Change"){
    return(calcpaper(years, USA$CarbonInputFlowPaperStockChange)[years-(min(years-1))])
  }
}

