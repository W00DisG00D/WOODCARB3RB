#' Calculates paper statistics
#'
#' Uses anonymous functions to calculate columns from `USA` sheet in WOODCARB spreadsheet.
#' @param years years to calculate data for
#' @param woodToCarbon Carbon conversion factor
#' @param paperToCarbon Carbon conversion factor
#' @return A data frame with necessary intermediate calculations to calculate carbon from paper.
#'
#' Corresponds with necessary columns from the `USA` sheet in the WOODCARB spreadsheet
calcUSApaper <- function(years = yrs,
                         woodToCarbon = 4.535925e-07, paperToCarbon = 3.9008955e-07) {

  USA <- data.frame(Years = years)
  ##api includes calculations from estimates/averages
  #from api_total_1973
  USA$`apiTotalWP_L` <- sapply(years, function(year) {
    if (year < 1957) {
      return( (apiFiber(year, 'Rags.Consumption') +
                apiFiber(year, 'Other.Consumption')) * (apiTotal(year, 'Woodpulp.Prod')/
                                                          apiTotal(year, 'Consump.Paper.Board')))
    }
    if (year > 1956) {
      #G, B, F
      return(apiFiber(year, 'Other.Consumption') * (apiTotal(year, 'Woodpulp.Prod') /
                                             apiTotal(year, 'Consump.Paper.Board')))
    }
  })
 #
  USA$`Recovered Fibre Pulp Exports Quantity` <- sapply(years, function(year) {
    if (year > 1997 && year < 2014) {
      return(usafp[[year-1997, "Exports.Quantity"]])
    }
    else{
      0
    }
  })

  USA$`Paper+Paperboard Production` <- 1000 * InceL5 * IncePaper[years - (minyr - 2), 'Paper.Board.Prod']

  USA$`Paper+Paperboard Exports` <- 1000 * InceL5 * IncePaper[years - (minyr - 2), 'Paper.Board.ApparentConsumption']

  USA$`Pulp for Paper Production` <- sapply(years, function(year) {
    if (year < 1965) {
      return( apiTotal(year, 'Woodpulp.Prod') +
               USA$`apiTotalWP_L`[year-(minyr-1)])
    }
    if (year < 2021) {
      return((h49(year, 'Woodpulp.Production') + h46(year, 'Consump.Other') * (h46(year, 'Prod.PaperBoard') /
                                                                 h46(year, 'Consump.Tot'))) * 1000)
    }
    if (year > 2020) {
      return(USA$`Pulp for Paper Production`[[USA$Years == 2002]]) #2002 value
    }
  })

  USA$`Pulp for Paper Imports` <- sapply(years, function(year) {
    if (year < 1965) {
      return(apiTotal(year, 'Woodpulp.Imports') +
               USA$`apiTotalWP_L`[year - (minyr - 1)] * (apiTotal(year, 'Woodpulp.Imports') /
                                                           apiTotal(year, 'Woodpulp.Prod')))
    }
    if (year < 2021) {
      return( (h49(year, 'Woodpulp.Imports') +
                 h46(year, 'Consump.Other') * (h49(year, 'Woodpulp.Imports.PercConsump') / 100)) * 1000)
    }
    if (year > 2020) {
      return(USA$`Pulp for Paper Imports`[[USA$Years == 2002]])#2002 value
    }
  })

  USA$`Pulp for Paper Exports` <- sapply(years, function(year) {
    if (year < 1965) {
      return(apiTotal(year, 'Woodpulp.Exports') +
               USA$`apiTotalWP_L`[year - (minyr - 1)] * (apiTotal(year, 'Woodpulp.Exports') /
                                                           apiTotal(year, 'Woodpulp.Prod')))
    }
    if (year < 2021) {
      return( (h49(year, 'Woodpulp.Exports') +
                 h46(year, 'Consump.Other') *
                 (h46(year, 'Prod.PaperBoard') / h46(year, 'Consump.Tot')) *
                 (h49(year, 'Woodpulp.Exports.PercProd')/100)) * 1000)
    }
    if (year > 2020) {
      return(USA$`Pulp for Paper Exports`[[USA$Years == 2002]])
    }
  })

  USA$`Other Fibre Pulp Production` <- sapply(years, function(year) {
    if (year < 1965) {
      return(USA$`apiTotalWP_L`[year - (minyr - 1)])
    }
    if (year < 2014) {
      return(h46(year, 'Consump.Other') * (h46(year, 'Prod.PaperBoard') /
                                             h46(year, 'Consump.Tot')) * 1000)
    }
    if (year < 2021) {
      return(1000 * h46(2007, 'Consump.Other') *
               (h46(2007, 'Prod.PaperBoard') / h46(2007, 'Consump.Tot')))#uses 2007 value
    }
    if (year > 2020) {
      return(1000 * h46(2002, 'Consump.Other') *
               (h46(2002, 'Prod.PaperBoard') /
                  h46(2002, 'Consump.Tot')))#uses value from 2002
    }
  })

  USA$`Other Fibre Pulp Imports` <- sapply(years, function(year) {
    if(year < 1965) {
      return(USA$`apiTotalWP_L`[year - (minyr - 1)] * (apiTotal(year, 'Woodpulp.Imports') /
                                                         apiTotal(year, 'Woodpulp.Prod')))
    }
    if (year < 2014) {
      return(1000 * h46(year, 'Consump.Other') * (h49(year, 'Woodpulp.Imports.PercConsump') / 100))
    }
    if (year < 2021) {
      return(1000 * h46(2007, 'Consump.Other') * (h49(2007, 'Woodpulp.Imports.PercConsump') / 100)) #uses 2007
    }
    if (year > 2020) {
      return(1000 * h46(2002, 'Consump.Other') * (h49(2002, 'Woodpulp.Imports.PercConsump') / 100))#uses 2002
    }
  })

  USA$`Other Fibre Pulp Exports` <- sapply(years, function(year) {
    if(year < 1965) {
      return(USA$`apiTotalWP_L`[year - (minyr - 1)] * (apiTotal(year, 'Woodpulp.Exports') / apiTotal(year, 'Woodpulp.Prod')))
    }
    if (year < 2014) {
      return(1000 * h46(year, 'Consump.Other') * (h46(year, 'Prod.PaperBoard') /
                                                    h46(year, 'Consump.Tot')) * (h49(year, 'Woodpulp.Exports.PercProd')/100))
    }
    if (year < 2021) {
      return(1000 * h46(2007, 'Consump.Other') * (h46(2007, 'Prod.PaperBoard') / h46(2007, 'Consump.Tot')) *
               (h49(2007, 'Woodpulp.Exports.PercProd') / 100)) #uses 2007 value
    }
    if (year > 2020) {
      return(1000 * h46(2002, 'Consump.Other') * (h46(2002, 'Prod.PaperBoard') / h46(2002, 'Consump.Tot')) *
               (h49(2002, 'Woodpulp.Exports.PercProd') /100)) #uses 2002 value
    }
  })
  #return(USA$`Other Fibre Pulp Production`)
  USA$`Wood Pulp for Paper Production` <- USA$`Pulp for Paper Production`- USA$`Other Fibre Pulp Production`

  USA$`Wood Pulp for Paper Imports` <- USA$`Pulp for Paper Imports` - USA$`Other Fibre Pulp Imports`

  USA$`Wood Pulp for Paper Exports` <- USA$`Pulp for Paper Exports` - USA$`Other Fibre Pulp Exports`

  USA$`Recovered Fibre Pulp Imports` <- sapply(years, function(year) {
      if (year < 1998) {
          return(0)
      }
      if (year < 2014) {
          return(0.90718 * usafp[[year-1997, "Exports.Quantity"]])#recov exports qty
      }
      if (year < 2021) {
          return(0.90718 * usafp[[2007-1997, "Exports.Quantity"]]) #2007 value
      }
  })

  USA$`Recovered Fibre Pulp Exports` <- sapply(years, function(year) {
    if (year < 1998) {
      return(0)
    }
    if (year < 2014) {
      return(0.90718 * USA[USA$Years == year, 'Recovered Fibre Pulp Exports Quantity'])#recov exports qty
    }
    if (year < 2021) {
      return(0.90718 * usafp[[2007-1997, "Exports.Quantity"]]) #2007 value
    }
  })

  USA$`Recovered Paper Imports` <- sapply(years, function(year) {
      if (year < 1965) {
          return(apiTotal(year, "WastePaper.Estimated.Imports"))
      }
      if (year < 2021) {
          return(h47(year, "RecPap.Imports")*1000)
      }
  })

  USA$`Recovered Paper Exports` <- sapply(years, function(year) {
    if (year < 1965) {
      return(apiTotal(year, 'WastePaper.Estimated.Exports'))##calculation
    }
    if (year < 2021) {
      return(1000 * h47(year, 'RecPap.Exports'))
    }
  })

  USA$`Total Roundwood Consumed For Paper` <- sapply(years, function(year) {
    if (year < 1950) {
      return( h3(year, 'PulpWood.Dom.Prod') *
                ( (InceV5 * u5(1950, 'PulpProducts.Imports')  + InceW5 * u6(1950, 'Ind.RW.Pulp.Prod')) /
                    (u5(1950, 'PulpProducts.Prod') + u6(1950, 'Ind.RW.Pulp.Prod')))) #1950 value
    }
    if (year < 1965) {
      #u6`T`
      return(InceV5 * u5(year, 'PulpProducts.Imports') + InceW5 * u6(year, 'Ind.RW.Pulp.Prod'))
    }
    if (year < 2021) {
      return( InceV5 * h6(year, 'Ind.RW.Pulp.Prod') +
               InceW5 * h7(year, 'Ind.RW.Pulp.Prod'))
    }
  })

  USA$`Imported Pulpwood Chips` <- sapply(years, function(year) {
    if (year < 1965) {
      return(0)
    }
    if (year < 1989) {
      return(h5(year, 'PulpwoodChip.Imports') *
               ((InceV5 * h6(1989, 'Ind.RW.Pulpchip.Imports') + InceW5 * h7(1989, 'Ind.RW.Pulpchip.Imports'))/
                  (h5(1989, 'PulpwoodChip.Imports'))) ) #1989 value
    }
    if (year < 2021) {
      return( InceV5 * h6(year, 'Ind.RW.Pulpchip.Imports') +
                InceW5 * h7(year, 'Ind.RW.Pulpchip.Imports'))
    }
  })

  USA$`Chip Exports (tons)` <- sapply(years, function(year) {
    if (year < 1965) {
      return(0)
    }
    if (year < 2021) {
      return(1000 * (InceV5 * h6(year, 'Ind.RW.Pulpchip.Exports') +
                            InceW5 * h7(year, 'Ind.RW.Pulpchip.Exports')))
    }
    if (year > 2020) {
      return(USA$`Chip Exports (tons)`[[USA$Years == 1999]]) #1999 value
    }
  })


  USA$`Paper+Paperbaord Imports` <-  1000 * InceL5 * IncePaper[years - (1899 - 1), 'Paper.Board.Imports']

  USA$`Paper+Paperboard Consumption` <- USA$`Paper+Paperboard Production` +
    USA$`Paper+Paperbaord Imports`-
    USA$`Paper+Paperboard Exports`


  USA$`Percent of Wood Pulp For Paper`<- ((USA$`Pulp for Paper Production`+USA$`Pulp for Paper Imports`-
                                             USA$`Pulp for Paper Exports`)-
                                            (USA$`Other Fibre Pulp Production`+USA$`Other Fibre Pulp Imports`-
                                               USA$`Other Fibre Pulp Exports`))/(USA$`Pulp for Paper Production`+USA$`Pulp for Paper Imports`-
                                                                                   USA$`Pulp for Paper Exports`)


  USA$CarbonInputFlowPaperStockChange <- paperToCarbon*USA$`Paper+Paperboard Consumption`*USA$`Percent of Wood Pulp For Paper`

  USA$`Nonwood Fiber Percent of Total Pulp Consumption` <-
    (USA$`Other Fibre Pulp Production`+USA$`Other Fibre Pulp Imports`
     -USA$`Other Fibre Pulp Exports`)/(USA$`Pulp for Paper Production`
                                       +USA$`Pulp for Paper Imports`-USA$`Pulp for Paper Exports`)

  USA$`Imported Woodpulp as a Percent of Total WoodPulp Consumption` <-
    USA$`Wood Pulp for Paper Imports`/(USA$`Wood Pulp for Paper Production` +
                                         USA$`Wood Pulp for Paper Imports`-USA$`Wood Pulp for Paper Exports`)

  USA$`Production Approach-C Input from Paper Products(Calc BU)` <-  a5 * paperToCarbon * ( (USA$`Paper+Paperboard Production` +
                                                                              USA$`Paper+Paperboard Exports` * (a5 - 1)) *
                                                                             (1 - USA$`Nonwood Fiber Percent of Total Pulp Consumption` * PRP62) *
                                                                             (1 - USA$`Imported Woodpulp as a Percent of Total WoodPulp Consumption` * PRP62) *
                                                                             ( (USA$`Total Roundwood Consumed For Paper` - USA$`Imported Pulpwood Chips` + USA$`Chip Exports (tons)`/1000) /
                                                                                USA$`Total Roundwood Consumed For Paper`) +
                                                                             (USA$`Recovered Fibre Pulp Exports` + USA$`Recovered Paper Exports` +
                                                                                USA$`Pulp for Paper Exports`))

  USA
}


