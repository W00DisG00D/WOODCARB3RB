#' Solidwood products statistics
#' Various production, imports and export data for different wood product types.
#' 21 columns in SWPcalcdata that are used to calculate production and stock change approach.
#' @param Years years to calculate
#'
#' @return Data frame of Roundwood and solidwood production, imports and exports
#' corresponds to columns in `SWPcalcdata` spreadsheet.
calculateswpdata <- function(Years = yrs) {
    swpcalcdata <- data.frame(Years = yrs)
    yrs <- swpcalcdata$Years
    pap <- calcUSApaper()

    swpcalcdata$`Other Products Production` <- 1000 * InceN5 * sapply(yrs, function(y) {
        if (y < 1950) {
            return(h3(y, 'Other.ApparentConsumption'))
        }
        if (y < 1965) {
            return(u4(y, 'OtherIndProducts.ProdandConsump'))
        }
        if (y < 2021) {
            return(h5(y, 'OtherIndProductsandConsump'))
        }
    })
    pOther <- function(y) {
        # other products exports, swpcalcP
        return(0)
    }
    swpcalcdata$`Other Products Exports` <- 0

    swpcalcdata$`Sawnwood Production` <- 1000 * sapply(yrs, function(y) {
        cavg <- (h8(1904, 'Prod.SW') - h8(1899, 'Prod.SW')) / 5
        davg <- (h8(1904, 'Prod.HW') - h8(1899, 'Prod.HW')) / 5
        if (y < 1904) {
            return((((h8(1899, 'Prod.SW') + ((y - 1899) * cavg)) * InceF5) +
                (h8(1899, 'Prod.HW') + (y - 1899) * davg) * InceG5))
        }
        if (y < 1950) {
            return((h8(y, 'Prod.SW') * InceF5 + h8(y, 'Prod.HW') * InceG5))
        }
        if (y < 1965) {
            return((u29(y, "Prod.Tot") * 1000 * (((u29(y, "Prod.SW")/u29(y, "Prod.Tot"))
                                                  * InceF5) +
                ((u29(y, "Prod.HW")/u29(y, "Prod.Tot")) * InceG5))))
        }
        if (y < 2021) {
            return((h28(y, "Prod.Tot") * 1000 * (((h28(y, "Prod.SW")/h28(y, "Prod.Tot"))
                                                  * InceF5) +
                ((h28(y, "Prod.HW")/h28(y, "Prod.Tot")) * InceG5))))
        }
    })

    swpcalcdata$`Sawnwood Imports` <- 1000 * sapply(yrs, function(year) {
        if (year < 1918) {
            return(InceF5 * h8(year, 'Imports.Tot'))
        }
        if (year < 1950) {
            return(InceF5 * (h8(year, 'Imports.SW') + h8(year, 'Imports.Mixed'))  +
                      InceG5 * h8(year, 'Imports.HW'))
        }
        if (year < 1965) {
            return(1000 * (InceF5 * u29(year, 'Imports.SW')
                           + InceG5 * u29(year, 'Imports.HW')))
        }
        if (year < 2021) {
            return(1000 * (InceF5 * h28(year, 'Imports.SW')
                           + InceG5 * h28(year, 'Imports.HW')))
        }
        # if(year< 2051){
        # return(((i1(year,4)*InceF5)+(i1(year,5)*InceG5))*1000) }
    })

    swpcalcdata$`Sawnwood Exports` <- 1000 * sapply(yrs, function(year) {
        if (year < 1911) {
            return(InceF5 * h8(year, 'Exports.Tot'))
        }
        if (year < 1950) {
            return(InceF5 * (h8(year, 'Exports.SW') + h8(year, 'Exports.Mixed')) +
                     InceG5 * h8(year, 'Exports.HW'))
        }
        if (year < 1965) {
            return(1000 * (InceF5 * u29(year, 'Exports.SW') +
                             InceG5 * u29(year, 'Exports.HW')))
        }
        if (year < 2021) {
            return(1000 * (InceF5 * h28(year, 'Exports.SW') +
                             InceG5 * h28(year, 'Exports.HW')))
        }
    })

    swpcalcdata$`Roundwood consumed for lumber and panels` <- 1000 * sapply(yrs,
        function(year) {
            if (year < 1950) {
                  return((0.8 * InceV5 + 0.2 * InceW5) * (h3(year, 'SawLogs.Dom.Prod') +
                                                            h3(year, 'VeneerLogs.Dom.Prod')))
            }
            if (year < 1965) {

                  return(InceV5 * (u5(year, 'Lumber.Prod') + u5(year, 'PlywoodVeneer.Prod'))  +
                            InceW5 * (u6(year, 'Ind.RW.Lum.Prod') + u6(year, 'Ind.RW.PlyandVen.Prod')))
            }
            if (year < 2021) {
                return(InceV5 * (h6(year, 'Ind.RW.Lum.Prod') + h6(year, 'Ind.RW.PlyandVen.Prod')) +
                          InceW5 * (h7(year, 'Ind.RW.Lum.Prod') + h7(year, 'Ind.RW.PlyandVen.Prod')))
            }
        })

    swpcalcdata$`Log Exports (tons)` <- 1000 * sapply(yrs, function(year) {
        if (year < 1965) {
            return(InceV5 * h3(year, 'LogChipExports.SW') +
                     InceW5 * h3(year, 'LogChipExports.HW'))
        }
        if (year < 2021) {
            return(InceV5 * h6(year, 'Ind.RW.Logs.Exports')  +
                     InceW5 * h7(year, 'Ind.RW.Logs.Exports'))
        }
    })

    #Exports - Other industrial roundwood
    swpcalcdata$`Log Exports (short tons)` <- 1000 * sapply(yrs, function(year) {
        if (year < 1965) {
            return(swpcalcdata[swpcalcdata$Years == year, "Log Exports (tons)"] / 1000)
        }
        if (year < 2021) {
            return((h6(year, "FuelWood.ProdAndConsumption")*InceS5+
                        h7(year, "Ind.RW.PlyandVen.Exports")*InceT5+
                        h6(year, "Ind.RW.Pulpchip.Exports")*InceS5+
                        h7(year, "Ind.RW.Pulpchip.Exports")*InceT5))
        }
    })

    #Imports - other industrial roundwood
    swpcalcdata$Log_Imports <- 1000 * sapply(yrs, function(year) {
        if (year < 1950) {
            return((h3(year, "LogChipExports.SW")*InceS5+
                        h3(year, "LogChipExports.HW")*InceT5))
        }
        if (year < 1965) {
            return(u5(year, "Logs.Imports")*InceS5+
                        u6(year, "Ind.RW.Logs.Imports")*InceT5)
        }
        if (year < 2021) {
            return(h6(year, "Ind.RW.Logs.Imports")*InceS5+
                        h7(year, "Ind.RW.Logs.Imports")*InceT5+
                        h6(year, "Ind.RW.Pulpchip.Imports")*InceS5+
                        h7(year, "Ind.RW.Pulpchip.Imports")*InceT5)
        }
    })

    swpcalcdata$TotalFibreFinish_Imports <- pap$`Wood Pulp for Paper Imports` + pap$`Recovered Paper Imports` + pap$`Recovered Fibre Pulp Imports`
    swpcalcdata$TotalFibreFinish_Exports <- pap$`Wood Pulp for Paper Exports` + pap$`Recovered Fibre Pulp Exports` + pap$`Recovered Fibre Pulp Exports`

    swpcalcdata$`Imported logs for lumber and panels (1000 tons)` <- 1000 * sapply(yrs,
        function(year) {
            if (year < 1950) {
                return(0)
            }
            if (year < 1965) {
                # u5$Y, u6$z
                return(InceV5 * u5(year, 'Logs.Imports') +
                        InceW5 * u6(year, 'Ind.RW.Logs.Imports'))
            }
            if (year < 2021) {
                return(InceV5 * h6(year, 'Ind.RW.Logs.Imports') +
                         InceW5 * h7(year, 'Ind.RW.Logs.Imports'))
            }
        })

    swpcalcdata$SP.Production <- 1000 * sapply(yrs, function(year) {
        if (year < 1950) {
            # softwood plywood million ft^2
            return(InceB5 * inc1(year, 'SW.Ply'))
        }
        if (year < 1965) {
            return(InceB5 * u36(year, 'Prod.SW'))
        }
        if (year < 1980) {
            return(InceB5 * h37(year, 'Prod.SW'))
        }
        if (year < 2021) {
            return(InceB5 * h37(year, 'Prod.SW') +
                     InceC5 * h38(year, 'Prod.OSP'))
        }
    })

    swpcalcdata$SP.Imports <- 1000 * sapply(yrs, function(year) {
        if (year < 1950) {
            return(0)
        }
        if (year < 1965) {
            return(InceB5 * u36(year, 'Imports.SW'))
        }
        if (year < 1980) {
            return(InceB5 * h37(year, 'Imports.SW'))
        }
        if (year < 2021) {
            return(InceB5 * h37(year, 'Imports.SW') + InceC5 * h38(year, 'Imports.OSP'))
        }
        # if(year<2051){
        # return(InceB5 * inc1(year, 'SW.Ply') + InceC5 * inc1(year, 'OSB.Wafer.board))
        #}
    })

    swpcalcdata$SP.Exports <- 1000 * sapply(yrs, function(year) {
        if (year < 1927) {
            return(0)
        }
        if (year < 1950) {
            return(InceB5 * h3t20(year, 'SW.Exports') / 1000)
        }
        if (year < 1965) {
            return(InceB5 * u36(year, 'Exports.SW'))
        }
        if (year < 1991) {
            return(InceB5 * h37(year, 'Exports.SW'))
        }
        if (year < 2021) {
            return(InceB5 * h37(year, 'Exports.SW') + InceC5 * h38(year, 'Exports.OSP'))
        }
    })

    swpcalcdata$NSP.Production <- 1000 * sapply(yrs, function(year) {
        if (year < 1950) {
            return(InceE5 * inc1(year, 'HW.Ply.Ven') + InceJ5 * inc1(year, 'Hardboard.Prod') +
                      InceO5 * inc1(year, 'InsulatingBoard'))
        }
        if (year < 1965) {
            return(InceE5 * u36(year, 'Prod.HW') + InceI5 * u52(year, 'Prod.Part.Board')
                    + InceJ5 * u54(year, 'Hardboard.Prod') + InceO5 * u53(year, 'InsulatingBoard.Prod'))
        }
        if (year < 2021) {
            return(InceE5 * h37(year, 'Prod.HW') + InceI5 * h53(year, 'Particle_MDF.Prod.ParticleBoard') +
                      InceJ5 * h56(year, 'Hardboard.Production') +
                     InceK5 * h53(year, 'Particle_MDF.Prod.MediumDensityFiberboard') + InceQ5 * h55(year, 'Insulboard.Production'))
        }
    })

    swpcalcdata$NSP.Imports <- 1000 * sapply(yrs, function(year) {
        if (year < 1927) {
            return(0)
        }
        if (year < 1935) {
            return(InceR5 * h3t21(year, 'Imports.Tot') / 1000)
        }
        if (year < 1950) {
            return((InceE5 * h3t20(year, 'HW.Imports.Tot') +
                       InceR5 * h3t21(year, 'Imports.Tot'))/1000)
        }
        if (year < 1954) {
            return(InceE5 * u36(year, 'Imports.HW'))
        }
        if (year < 1963) {
            return(InceE5 * u36(year, 'Imports.HW') + InceJ5 * u54(year, 'Hardboard.Import') +
                      InceO5 * u53(year, 'InsulatingBoard.Import'))
        }
        if (year < 1965) {
            return(InceE5 * u36(year, 'Imports.HW') + InceI5 * u52(year, 'Imports') +
                       InceJ5 * u54(year, 'Hardboard.Import') + InceO5 * u53(year, 'InsulatingBoard.Import'))
        }
        if (year < 2021) {
            return(InceE5 * h37(year, 'Imports.HW') + InceI5 * h53(year, 'Particle_MDF.Imports') +
                      InceJ5 * h56(year, 'Hardboard.Imports')  + InceQ5 * h55(year, 'Insulboard.Imports'))
        }
    })

    swpcalcdata$NSP.Exports <- 1000 * sapply(yrs, function(year) {
        if (year < 1916) {
            return(0)
        }
        if (year < 1925) {
            return((InceJ5 * u54(year, 'Hardboard.Exports')))
        }
        if (year < 1927) {
            return(InceJ5 * u54(year, 'Hardboard.Exports') +
                      InceO5 * u53(year, 'InsulatingBoard.Exports'))
        }
        if (year < 1935) {
            return((InceE5 * h3t20(year, 'HW.Exports') + InceR5 * h3t21(year, 'Exports.Tot')) / 1000 +
                 InceJ5 * u54(year, 'Hardboard.Exports')  + InceO5 * u53(year, 'InsulatingBoard.Exports'))
        }
        if (year < 1950) {
            return(InceE5 * (h3t20(year, 'HW.Exports') + h3t21(year, 'Exports.Tot')) / 1000 +
                      InceJ5 * u54(year, 'Hardboard.Exports') + InceO5 * u53(year, 'InsulatingBoard.Exports'))
        }
        if (year < 1965) {
            return(InceE5 * u36(year, 'Exports.HW') + InceJ5 * u54(year, 'Hardboard.Exports') +
                      InceO5 * u53(year, 'InsulatingBoard.Exports'))
        }
        if (year < 2021) {
            return(InceE5 * h37(year, 'Exports.HW') + InceI5 * h53(year, 'Particle_MDF.Exports') +
                     InceJ5 * h56(year, 'Hardboard.Exports') + InceQ5 * h55(year, 'Insulboard.Exports'))
        }
    })


    swpcalcdata$`Wood Panel Production` <- 1000 * sapply(yrs, function(year) {
        if (year < 1950) {
          return((inc1(year, "SW.Ply")*InceB5)
                 +(inc1(year, "HW.Ply.Ven")*InceE5)
                 +(inc1(year,  "Hardboard.Prod")*InceJ5)
                 +(inc1(year, "InsulatingBoard")*InceO5))
        }
        if (year < 1965) {
            return( (u36(year,  "Prod.SW")*InceB5)+
                        (u36(year, "Prod.HW")*InceE5)+
                        (u52(year, "Prod.Part.Board")*InceI5)+
                        (u54(year, "Hardboard.Prod")*InceJ5)+
                        (u53(year, "InsulatingBoard.Prod")*InceO5))
        }
        if (year < 1980) {
            return(h37(year, "Prod.SW")*InceB5 +
                       (h37(year, "Prod.HW") *InceE5) +
                       (h53(year, "Particle_MDF.Prod.ParticleBoard" )*InceI5) +
                       (h56(year, "Hardboard.Production")*InceJ5) +
                       (h53(year, "Particle_MDF.Prod.MediumDensityFiberboard")*InceK5) +
                       h55(year, "Insulboard.Production")*InceQ5)
        }
        if (year < 2021) {
            return((h37(year, "Prod.SW")*InceB5)+
                       (h38(year, "Prod.OSP")*InceC5)+
                       (h37(year, "Prod.HW")*InceE5)+
                       (h53(year, "Particle_MDF.Prod.ParticleBoard")*InceI5)+
                       (h56(year, "Hardboard.Production")*InceJ5)+
                       (h53(year, "Particle_MDF.Prod.MediumDensityFiberboard")*InceK5)+
                       h55(year, "Insulboard.Production")*InceQ5)
        }
    })

    swpcalcdata$`Wood Panel Imports` <- 1000 * sapply(yrs, function(year) {
        if (year < 1927) {
            return(0)
        }
        if (year < 1935) {
            return((h3t21(year, "Imports.Tot")/1000)*InceR5)
        }
        if (year < 1950) {
            return((h3t20(year, "HW.Imports.Tot")*InceE5 +
                        h3t21(year, "Imports.Tot")*InceR5)/1000)
        }
        if (year < 1954) {
            return((u36(year, "Imports.SW")*InceB5)+
                       (u36(year, "Imports.HW")*InceE5))
        }
        if (year < 1965) {
            return((u36(year, "Imports.SW")*InceB5)+
                       (u36(year, "Imports.HW")*InceE5)+
                       (u52(year, "Imports")*InceI5)+
                       (u54(year, "Hardboard.Import")*InceJ5)+
                       (u53(year, "InsulatingBoard.Import")*InceO5))
        }
        if (year < 1980) {
            return((h37(year, "Imports.SW")*InceB5)+
                       (h37(year, "Imports.HW")*InceE5)+
                       (h53(year, "Particle_MDF.Imports")*InceI5)+
                       (h56(year, "Hardboard.Imports")*InceJ5)+
                       h55(year, "Insulboard.Imports")*InceQ5)
        }
        if (year < 2021) {
            return((h37(year, "Imports.SW")*InceB5)+
                       (h38(year, "Imports.OSP")*InceC5)+
                       (h37(year, "Imports.HW")*InceE5)+
                       (h53(year, "Particle_MDF.Imports")*InceI5)+
                       (h56(year, "Hardboard.Imports")*InceJ5)+
                       h55(year, "Insulboard.Imports")*InceQ5)
        }
    })
    swpcalcdata$`Wood Panel Exports` <- 1000 * sapply(yrs, function(year) {
        if (year < 1916) {
            return(0)
        }
        if (year < 1925) {
            return(u54(year, "Hardboard.Exports")*InceJ5)
        }
        if (year < 1927) {
            return((u54(year, "Hardboard.Exports")*InceJ5)+
                        (u53(year, "InsulatingBoard.Exports")*InceO5))
        }
        if (year < 1935) {
            return((h3t20(year, "SW.Exports")/1000*InceB5)+
                       (h3t20(year, "HW.Exports")*InceE5+h3t21(year, "Exports.Tot")*InceR5)/1000+
                       (u54(year, "Hardboard.Exports")*InceJ5)+
                       (u53(year, "InsulatingBoard.Exports")*InceO5))
        }
        if (year < 1950) {
            return((h3t20(year, "SW.Exports")/1000*InceB5)+
                       ((h3t20(year, "HW.Exports")+h3t21(year, "Exports.Tot"))/1000*InceE5)+
                       (u54(year, "Hardboard.Exports")*InceJ5)+
                       (u53(year, "InsulatingBoard.Exports")*InceO5))
        }
        if (year < 1965) {
            return((u36(year, "Exports.SW")*InceB5)+
                        (u36(year, "Exports.HW")*InceE5)+
                        (u54(year, "Hardboard.Exports")*InceJ5)+
                        (u53(year, "InsulatingBoard.Exports")*InceO5))
        }
        if (year < 1991) {
            return((h37(year, "Exports.SW")*InceB5)+
                       (h37(year, "Exports.HW")*InceE5)+
                       (h53(year, "Particle_MDF.Exports")*InceI5)+
                       (h56(year, "Hardboard.Exports")*InceJ5)+
                       h55(year, "Insulboard.Exports")*InceQ5)
        }
        if (year < 2021) {
            return((h37(year, "Exports.SW")*InceB5)+
                        (h38(year, "Exports.OSP")*InceC5)+
                        (h37(year, "Exports.HW")*InceE5)+
                        (h53(year, "Particle_MDF.Exports")*InceI5)+
                        (h56(year, "Hardboard.Exports")*InceJ5)+
                        h55(year, "Insulboard.Exports")*InceQ5)
        }
    })

    swpcalcdata$`Other Products Production Special` <- swpcalcdata$`Other Products Production` -
        (1 - a5) * swpcalcdata$`Other Products Exports`

    swpcalcdata$`Sawnwood Prod Special` <- (swpcalcdata$`Sawnwood Production` -
        (1 - a5) * swpcalcdata$`Sawnwood Exports`) * ((swpcalcdata$`Roundwood consumed for lumber and panels` +
        swpcalcdata$`Log Exports (tons)` * a5 - swpcalcdata$`Imported logs for lumber and panels (1000 tons)` *
        PRP62)/swpcalcdata$`Roundwood consumed for lumber and panels`)

    swpcalcdata$`SP Prod Special` <- (swpcalcdata$SP.Production - (1 -
        a5) * swpcalcdata$SP.Exports) * ((swpcalcdata$`Roundwood consumed for lumber and panels` +
        a5 * swpcalcdata$`Log Exports (tons)` - swpcalcdata$`Imported logs for lumber and panels (1000 tons)` *
        PRP62)/swpcalcdata$`Roundwood consumed for lumber and panels`)

    swpcalcdata$`NSP Prod Special` <- (swpcalcdata$NSP.Production -
        (1 - a5) * swpcalcdata$NSP.Exports) * ((swpcalcdata$`Roundwood consumed for lumber and panels` +
        a5 * swpcalcdata$`Log Exports (tons)` - swpcalcdata$`Imported logs for lumber and panels (1000 tons)` *
        PRP62)/swpcalcdata$`Roundwood consumed for lumber and panels`)

    swpcalcdata$`Sawnwood Consumption` <- swpcalcdata$`Sawnwood Production` +
        swpcalcdata$`Sawnwood Imports` - swpcalcdata$`Sawnwood Exports`

    swpcalcdata$SP.Consumption <- swpcalcdata$SP.Production + swpcalcdata$SP.Imports -
        swpcalcdata$SP.Exports

    swpcalcdata$NSP.Consumption <- swpcalcdata$NSP.Production + swpcalcdata$NSP.Imports -
        swpcalcdata$NSP.Exports

    swpcalcdata[swpcalcdata$Years %in% Years, ]
}

#' Total carbon placed in use in od tons of wood fiber
#'
#' Corresponds to B3147 column in SW Calc P and SW Calc spreadsheet for Production and Stock Change
#' approach respectively.
#'
#' @param Years years to calculate carbon placed in use for
#' @param approach The approach type to use. Fractions dont change but inflow does
#' @param total Whether to return total placed in use or seperate it by end uses.
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
#' @return a data frame of carbon placed in use for various end uses
calcplacediu <- function(Years = 1900:2020, approach = c("Production",
    "Stock Change"), total = TRUE, fsp = fsp_1,
                                   fnsp = fnonsp,
                                   fsawn = fsw, swpdata) {
    approachtype <- match.arg(approach)
    if (!missing(swpdata))
    {
      swpcalcdata <- swpdata
    }
    else
    {
      swpcalcdata <- calculateswpdata()
    }

    placeIU <- data.frame(Years = Years)

    if (approachtype == "Production") {
        placeIU[, 2:13] <- swpcalcdata[["Sawnwood Prod Special"]][Years -
            (minyr - 1)] * fsawn[Years - (minyr - 1), -13] +
            swpcalcdata[["SP Prod Special"]][Years - (minyr - 1)] *
                fsp[Years - (minyr - 1), -13] + swpcalcdata[["NSP Prod Special"]][Years -
            (minyr - 1)] * fnsp[Years - (minyr - 1), -13]
    } else if (approachtype == "Stock Change") {
        placeIU[, 2:13] <- swpcalcdata[["Sawnwood Consumption"]][Years -
            (minyr - 1)] * fsawn[Years - (minyr - 1), -13] +
            swpcalcdata[["SP.Consumption"]][Years - (minyr - 1)] * fsp[Years -
                (minyr - 1), -13] + swpcalcdata[["NSP.Consumption"]][Years -
            (minyr - 1)] * fnsp[Years - (minyr - 1), -13]
    }

    placeIU$`Other Industrial Products Production` <- swpcalcdata$`Other Products Production Special`[Years -
        (minyr - 1)]
    if (total == FALSE) {
        placeIU
    } else {
         apply(placeIU[,-1], 1, sum)
    }
}
