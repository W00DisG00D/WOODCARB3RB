#' Calculates carbon in landfills and dumps.
#'
#' @param years years to calculate values for
#' @param approach approach to use
#' @return total carbon from solid waste disposal sites
carbonfromdumps <- function(years, approach = c("Production", "Stock Change")) {

    a <- match.arg(approach)

    environment(calculatedumpcarbonproduction) <- environment()
    environment(calculatedumpcarbonstockchange) <- environment()
    if (approach == "Production") {
        return(calculatedumpcarbonproduction(years, totalcarbonstockchange = TRUE))
    } else if (approach == "Stock Change") {
        return(calculatedumpcarbonstockchange(years, totalcarbonstockchange = TRUE))
    }
}

#' Calculate carbon in SWDS for the Stock Change Method
#'
#' @param years years to calculate
#' @param totalcarbonstockchange Whether to return data frame of intermedate calculations or
#'  just totals
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
#' @return If totalcarbonstockchange is false: a data frame of intermediate
#'  calculations.  If totalcarbonstockchange is true: a numeric vector of stock change
#'  for total carbon from dumps.
calculatedumpcarbonstockchange <- function(years, totalcarbonstockchange = FALSE,
                                           swpSwdsNondegradable = 0.77, paperSwdsNondegradable = 0.44,
                                           paperLandfillDecay = 0.0478032538317204, swpLandfillDecay = 0.0300063714528115,
                                           swpDumpDecay = 0.0420089200339361 , paperDumpDecay = 0.0840178400678722,
                                           woodToCarbon = 4.535925e-07, paperToCarbon = 3.9008955e-07) {
    ys <- minyr:max(years)

    environment(calcUSApaper) <- environment()
    usa <- calcUSApaper(ys)

    c <- calcplacediu(ys, approach = "Stock Change")
    swp <- swpcarbontotal(Yrs = ys, approach = "Stock Change")

    PRM45 <- 1 - swpSwdsNondegradable
    PRM46 <- 1 - paperSwdsNondegradable
    CalcUSA <- data.frame(Years = ys)

    CalcUSA$TotalCarbonOutputStockChange <- sapply(ys, function(year) {
        if (year == minyr) {
            return(c[year - (minyr - 1)] - swpcarbontotal(Yrs = year,
                lumberpre = FALSE, approach = "Stock Change"))
        } else {
            return((swp[year - minyr] + c[year - (minyr - 1)]) - (swp[year -
                (minyr - 1)]))
        }
    })

    #percent of swp to dumps 
    CalcUSA$SWPtoDumps <- wd_percent[ys - (minyr - 1), "wd_percent"]

    #Input to degradable stock (dumps only) for swp using the stock change approach 
    CalcUSA$Input_DegradableStock_Dumps_SWP_StockChangeApproach <- PRM57 * CalcUSA$TotalCarbonOutputStockChange * woodToCarbon * CalcUSA$SWPtoDumps

    #Degradable Stock (dumps only) for swp using the stock change approach 
    for (year in ys) {
        if (year == 1900)
        {
            CalcUSA$DegradableStock_Dumps_SWP_StockChangeApproach[1] <- (1/(1 + swpDumpDecay)) * CalcUSA$Input_DegradableStock_Dumps_SWP_StockChangeApproach[year - (minyr - 1)]
        }
        else {
            CalcUSA$DegradableStock_Dumps_SWP_StockChangeApproach[year - (minyr - 1)] <- (1/(1 + swpDumpDecay)) *
                (CalcUSA$DegradableStock_Dumps_SWP_StockChangeApproach[year - minyr] + CalcUSA$Input_DegradableStock_Dumps_SWP_StockChangeApproach[year - (minyr - 1)])
        } 
    }
    
    #SWP to Landfills (Tg C) 
    CalcUSA$SWP_Landfills  <- wlf_percent[ys - (minyr - 1), "wlf_percent"] * PRJ96 * CalcUSA$TotalCarbonOutputStockChange *
        woodToCarbon 

    #SWP C stock change (dumps only) using the stock change approach 
    for (year in ys) {
        if (year == minyr) {
            CalcUSA$StockChange_Dumps_SWP_StockChangeApproach [year - (minyr - 1)] <- CalcUSA$DegradableStock_Dumps_SWP_StockChangeApproach[year -
                (minyr - 1)]
        } else {
            CalcUSA$StockChange_Dumps_SWP_StockChangeApproach [year - (minyr - 1)] <- CalcUSA$DegradableStock_Dumps_SWP_StockChangeApproach[year -
                (minyr - 1)] - CalcUSA$DegradableStock_Dumps_SWP_StockChangeApproach[year - minyr]
        }
    }

    #Input to Degradable Stock (landfills only) 
    CalcUSA$Input_DegradableStock_Landfills <- PRM45 * CalcUSA$SWP_Landfills 

    #Degradable stock (landfills)
    for (year in ys) {
        if (year == minyr) {
          CalcUSA$DegradableStock_Landfills[year - (minyr - 1)] <- (1/(1 + swpLandfillDecay)) * CalcUSA$Input_DegradableStock_Landfills[year -
                                                                 (minyr - 1)]
        } else {
          CalcUSA$DegradableStock_Landfills[year - (minyr - 1)] <- (1/(1 + swpLandfillDecay)) * (CalcUSA$DegradableStock_Landfills[year -
                                                                  (minyr)] + CalcUSA$Input_DegradableStock_Landfills[year - (minyr - 1)])

        }
    }

    #Stock change degradable (landfills only)
    CalcUSA$DegradableStockChange_Landfills <- sapply(ys, function(year) {
        if (year == minyr) {
            return(CalcUSA$DegradableStock_Landfills[year - (minyr - 1)])
        }
        return(CalcUSA$DegradableStock_Landfills[year - (minyr - 1)] - CalcUSA$DegradableStock_Landfills[year - minyr])
    })

    #Input to permanent stock from SWP
    CalcUSA$Input_PermanentStock_SWP  <- swpSwdsNondegradable * CalcUSA$SWP_Landfills 

    #C input flow - paper products 
    CalcUSA$CarbonInputFlow_Paper <- paperToCarbon * usa$`Percent of Wood Pulp For Paper` * usa$`Paper+Paperboard Consumption`

    #Carbon stock in paper products 
    for (year in ys) {
        if (year == minyr) {
            CalcUSA$CarbonStock_Paper[year - (minyr - 1)] <- (exp(-log(2)/PRP10)) *
                (CalcUSA$CarbonInputFlow_Paper[year - (minyr - 1)])
        } else {
            CalcUSA$CarbonStock_Paper[year - (minyr - 1)] <- (exp(-log(2)/PRP10)) *
                (CalcUSA$CarbonInputFlow_Paper[year - (minyr - 1)] + CalcUSA$CarbonStock_Paper[year - minyr])
        }
    }

    #Carbon output from paper products 
    for (year in ys) {
        if (year == minyr) {
            CalcUSA$CarbonOutput_Paper[year - (minyr - 1)] <- CalcUSA$CarbonInputFlow_Paper[year - (minyr -
                1)] - CalcUSA$CarbonStock_Paper[year - (minyr - 1)]
        } else {
            CalcUSA$CarbonOutput_Paper[year - (minyr - 1)] <- CalcUSA$CarbonStock_Paper[year - (minyr)] +
                CalcUSA$CarbonInputFlow_Paper[year - (minyr - 1)] - CalcUSA$CarbonStock_Paper[year - (minyr -
                1)]
        }
    }


    CalcUSA$InputDegradableStock_Dumps_Paper_StockChangeApproach <- PRM57 * CalcUSA$CarbonOutput_Paper * wd_percent[ys - (minyr -
        1), "wd_percent"]

    #Degradable stock (dumps only) for paper products using the stock change approach 
    for (y in ys) {
        if (y == minyr) {
            CalcUSA$DegradableStock_Dumps_Paper_StockChangeApproach[y - (minyr - 1)] <- (1/(1 + paperDumpDecay)) * CalcUSA$InputDegradableStock_Dumps_Paper_StockChangeApproach[y - (minyr - 1)]
        } else {
            CalcUSA$DegradableStock_Dumps_Paper_StockChangeApproach[y - (minyr - 1)] <- (1/(1 + paperDumpDecay)) * (CalcUSA$DegradableStock_Dumps_Paper_StockChangeApproach[y -minyr] +
                                                                     CalcUSA$InputDegradableStock_Dumps_Paper_StockChangeApproach[y - (minyr - 1)])
        }
    }

    #Carbon input flow to landfills form paper products 
    CalcUSA$CarbonInputFlow_Landfills_Paper <- PRI96 * CalcUSA$CarbonOutput_Paper * plf_percent[ys - (minyr - 1), "plf_percent"]

    #Input to permanent stock from paper products 
    CalcUSA$Input_PermanentStock_Paper <- paperSwdsNondegradable * CalcUSA$CarbonInputFlow_Landfills_Paper

    for (y in ys) {
        if (y == minyr) {
            CalcUSA$Dumps_K[y - (minyr - 1)] <- CalcUSA$DegradableStock_Dumps_Paper_StockChangeApproach[y -
                (minyr - 1)]
        } else {
            CalcUSA$Dumps_K[y - (minyr - 1)] <- CalcUSA$DegradableStock_Dumps_Paper_StockChangeApproach[y -
                (minyr - 1)] - CalcUSA$DegradableStock_Dumps_Paper_StockChangeApproach[y - (minyr)]
        }
    }


    CalcUSA$AI <- PRM46 * CalcUSA$CarbonInputFlow_Landfills_Paper

    for (y in ys) {
        if (y == minyr) {
            CalcUSA$AJ[y - (minyr - 1)] <- (1/(1 + paperLandfillDecay)) * CalcUSA$AI[y -
                (minyr - 1)]
        } else {
            CalcUSA$AJ[y - (minyr - 1)] <- (1/(1 + paperLandfillDecay)) * (CalcUSA$AJ[y -
                minyr] + CalcUSA$AI[y - (minyr - 1)])
        }
    }

    CalcUSA$AL <- sapply(ys, function(y) {
        if (y < 1901) {
            return(CalcUSA$AJ[y - (minyr - 1)])
        }
        if (y > 1900) {
            return(CalcUSA$AJ[y - (minyr - 1)] - CalcUSA$AJ[y - minyr])
        }
    })

    CalcUSA$Total_swpstock_change_LD <- CalcUSA$DegradableStockChange_Landfills + CalcUSA$Input_PermanentStock_SWP  + CalcUSA$StockChange_Dumps_SWP_StockChangeApproach 

    CalcUSA$Total_paper_stock_change_LD <- CalcUSA$Input_PermanentStock_Paper + CalcUSA$AL + CalcUSA$Dumps_K

    CalcUSA$Var1b_STOCKCHANGE_TOTAL <- (CalcUSA$Total_swpstock_change_LD +
        CalcUSA$Total_paper_stock_change_LD)

    if (totalcarbonstockchange == TRUE) {
        return(CalcUSA$Var1b_STOCKCHANGE_TOTAL[years - (minyr - 1)])
    } else {
        CalcUSA[years - (minyr - 1), ]
    }
}

#' Calculate carbon in SWDS for the Production Method
#'
#' Column names with only letters come from Calc sheet, otherwise sheet is specified (need to change names)
#'
#' @param years years to calculate
#' @param totalcarbonstockchange If true, returns carbon stock change, or returns data
#' frame of intermediate calculations
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
#' @return Either a data frame of values from Calculation spreadsheet or total carbon output
calculatedumpcarbonproduction <- function(years, totalcarbonstockchange = FALSE,
                                          swpSwdsNondegradable = 0.77, paperSwdsNondegradable = 0.44,
                                          paperLandfillDecay = 0.0478032538317204, swpLandfillDecay = 0.0300063714528115,
                                          swpDumpDecay = 0.0420089200339361 , paperDumpDecay = 0.0840178400678722,
                                          woodToCarbon = 4.535925e-07, paperToCarbon = 3.9008955e-07) {
    ys <- minyr:max(years)

    environment(calcUSApaper) <- environment()
    usa <- calcUSApaper(ys)

    c <- calcplacediu(ys, approach = "Production")#.135 seconds

    swp <- as.numeric(swpcarbontotal(Yrs = ys))
    CalcUSA <- data.frame(Years = ys)

    PRM45 <- 1- swpSwdsNondegradable
    PRM46 <- 1 - paperSwdsNondegradable
    for (y in ys) {
        if (y == minyr) {
            CalcUSA$CA[1] <- exp(-log(2)/PRP10) * (usa$`Production Approach-C Input from Paper Products(Calc BU)`[y -
                (minyr - 1)])
        } else {
            CalcUSA$CA[y - (minyr - 1)] <- exp(-log(2)/PRP10) * (usa$`Production Approach-C Input from Paper Products(Calc BU)`[y -
                (minyr - 1)] + CalcUSA$CA[y - minyr])
        }
    }

    for (y in ys) {
        if (y == minyr) {
            CalcUSA$CB[y - (minyr - 1)] <- usa$`Production Approach-C Input from Paper Products(Calc BU)`[y -
                (minyr - 1)] - CalcUSA$CA[y - (minyr - 1)]
        } else {
            CalcUSA$CB[y - (minyr - 1)] <- CalcUSA$CA[y - minyr] + usa$`Production Approach-C Input from Paper Products(Calc BU)`[y -
                (minyr - 1)] - CalcUSA$CA[y - (minyr - 1)]
        }
    }

    CalcUSA$ParamResults_V <- plf_percent[ys - (minyr - 1), "plf_percent"]

    CalcUSA$ParamResults_P <- PRI96 * CalcUSA$ParamResults_V

    CalcUSA$CI <- CalcUSA$ParamResults_P * CalcUSA$CB

    CalcUSA$CS <- paperSwdsNondegradable * CalcUSA$CI

    CalcUSA$CU <- PRM46 * CalcUSA$CI

    for (year in ys) {
        if (year == minyr)
            CalcUSA$CV[1] <- (1/(1 + paperLandfillDecay)) * CalcUSA$CU[year - (minyr -
                1)] else CalcUSA$CV[year - (minyr - 1)] <- (1/(1 + paperLandfillDecay)) * (CalcUSA$CV[year -
            minyr] + CalcUSA$CU[year - (minyr - 1)])
    }

    CalcUSA$CX <- sapply(ys, function(y) {
        if (y == minyr) {
            return(0)
        } else {
            return(CalcUSA$CV[y - (minyr - 1)] - CalcUSA$CV[y - minyr])
        }
    })

    CalcUSA$SWPtoDumps <- wd_percent[ys - (minyr - 1), "wd_percent"]

    CalcUSA$Dumps_S <- PRM57 * CalcUSA$CB * CalcUSA$SWPtoDumps

    ### P+R 'O200' column, wlf_percent is data linked to another site,
    ### not sure how it's calculated.
    CalcUSA$ParamResults_O <- wlf_percent[ys - (minyr - 1), "wlf_percent"] * PRJ96

    for (year in ys) {
        if (year == minyr)
            CalcUSA$Dumps_T[1] <- (1/(1 + paperDumpDecay)) * CalcUSA$Dumps_S[year -
                (minyr - 1)] else CalcUSA$Dumps_T[year - (minyr - 1)] <- (1/(1 + paperDumpDecay)) *
            (CalcUSA$Dumps_T[year - minyr] + CalcUSA$Dumps_S[year -
                (minyr - 1)])
    }

    CalcUSA$Dumps_V <- sapply(ys, function(year) {
        if (year == minyr) {
            return(CalcUSA$Dumps_T[year - (minyr - 1)])
        } else {
            return(CalcUSA$Dumps_T[year - (minyr - 1)] - CalcUSA$Dumps_T[year -
                minyr])
        }
    })

    CalcUSA$TotalCarbonOutput <- sapply(ys, function(year) {
        if (year == minyr) {
            return(c[year - (minyr - 1)] - swpcarbontotal(Yrs = year,
                lumberpre = FALSE))
        } else {
            return((swp[year - minyr] + c[year - (minyr - 1)]) - (swp[year -
                (minyr - 1)]))
        }
    })

    CalcUSA$CM <- woodToCarbon * PRM45 * CalcUSA$ParamResults_O * CalcUSA$TotalCarbonOutput


    for (year in ys) {
        if (year == minyr)
            CalcUSA$CN[1] <- (1/(1 + swpLandfillDecay)) * CalcUSA$CM[year - (minyr -
                1)] else CalcUSA$CN[year - (minyr - 1)] <- (1/(1 + swpLandfillDecay)) * (CalcUSA$CN[year -
            minyr] + CalcUSA$CM[year - (minyr - 1)])
    }

    CalcUSA$Dumps_N <- PRM57 * CalcUSA$TotalCarbonOutput * woodToCarbon * CalcUSA$SWPtoDumps

    for (year in ys) {
        if (year == minyr)
            CalcUSA$Dumps_O[1] <- (1/(1 + swpDumpDecay)) * CalcUSA$Dumps_N[year -
                (minyr - 1)] else CalcUSA$Dumps_O[year - (minyr - 1)] <- (1/(1 + swpDumpDecay)) *
            (CalcUSA$Dumps_O[year - minyr] + CalcUSA$Dumps_N[year -
                (minyr - 1)])
    }

    CalcUSA$BX <- woodToCarbon * CalcUSA$TotalCarbonOutput

    CalcUSA$CH <- CalcUSA$ParamResults_O * CalcUSA$BX

    CalcUSA$CK <- swpSwdsNondegradable * CalcUSA$CH

    for (year in ys) {
        if (year == minyr)
            CalcUSA$CL[1] <- CalcUSA$CK[year - (minyr - 1)] else CalcUSA$CL[year - (minyr - 1)] <- CalcUSA$CL[year - minyr] +
            CalcUSA$CK[year - (minyr - 1)]
    }

    for (year in ys) {
        if (year == minyr)
            CalcUSA$CT[1] <- CalcUSA$CS[year - (minyr - 1)] else CalcUSA$CT[year - (minyr - 1)] <- CalcUSA$CT[year - minyr] +
            CalcUSA$CS[year - (minyr - 1)]
    }

    CalcUSA$C_SWP_StockChange_LFDumps <- sapply(ys, function(y) {
        if (y == 1900) {
            return(swpSwdsNondegradable * CalcUSA$ParamResults_O[y - (minyr -
                1)] * CalcUSA$TotalCarbonOutput[y - (minyr - 1)] * woodToCarbon +
                (CalcUSA$CM[y - (minyr - 1)]) + CalcUSA$Dumps_O[y -
                (minyr - 1)])
        } else {
            return(swpSwdsNondegradable * CalcUSA$ParamResults_O[y - (minyr -
                1)] * CalcUSA$TotalCarbonOutput[y - (minyr - 1)] * woodToCarbon +
                (CalcUSA$CN[y - (minyr - 1)] - CalcUSA$CN[y - minyr]) +
                (CalcUSA$Dumps_O[y - (minyr - 1)] - CalcUSA$Dumps_O[y -
                  minyr]))
        }
    })

    CalcUSA$C_PAPER_StockChange_LFDumps <- CalcUSA$CS + CalcUSA$CX + CalcUSA$Dumps_V

    CalcUSA$TotalC_StockChange_SWDSProduction <- CalcUSA$C_PAPER_StockChange_LFDumps +
        CalcUSA$C_SWP_StockChange_LFDumps


    if (totalcarbonstockchange == TRUE) {
        return(CalcUSA$TotalC_StockChange_SWDSProduction[years - (minyr -
            1)])
    }
    CalcUSA[years - (minyr - 1), ]
}
