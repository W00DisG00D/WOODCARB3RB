 #' Timber products: Production, imports, exports, and new supply of lumber in the United States, specified years, 1799-1958
#'
#' Dataset containing statistics for timber products.
#' All units are in Million Board Ft, unless otherwise specified. All vectors are numeric.
#'
#' @format A data frame with 58 observations on 23 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Prod.Tot}{Production Total}
#' \item{Prod.SW}{Production of Softwoods}
#' \item{Prod.HW}{Production of Hardwoods}
#' \item{Imports.Tot}{Total Imports}
#' \item{Imports.SW}{Imports for Softwood}
#' \item{Imports.HW}{Imports for Hardwoods}
#' \item{Imports.Mixed}{Imports for Mixed Woods}
#' \item{Imports.Mixed.PercOfTot}{Mixed Woods Percent of Total Imports}
#' \item{Imports.SW.PercOfTot}{Softwoods Percent of Total Imports}
#' \item{Imports.HW.PercOfTot}{Hardwoods Percent of Total Imports}
#' \item{Imports.Estimated.SW}{Estimated Softwood Imports}
#' \item{Imports.Estimated.HW}{Estimated Hardwood Imports}
#' \item{Exports.Tot}{Total Exports}
#' \item{Exports.SW}{Softwood Exports}
#' \item{Exports.HW}{Hardwood Exports}
#' \item{Exports.Mixed}{Mixed Woods Exports}
#' \item{Exports.Mixed.PercOfTot}{Mixed Woods Percent of Total Exports}
#' \item{Exports.SW.PercOfTot}{Softwoods Percent of Total Exports}
#' \item{Exports.HW.PercOfTot}{Hardwoods Percent of Total Exports}
#' \item{Exports.Estimated.SW}{Estimated Softwood Exports}
#' \item{Exports.Estimated.HW}{Estimated Hardwood Exports}
#' \item{NewSupply}{New Supply}
#' \item{PerCapita}{Per Capita Use, in Board ft.}
#' }
#' @source USFS Estimations and Bureau of Census Data
#' @keywords piet_all
"piet_all"



#' Estimated production, net imports, and apparent consumption of timber products in the US, 1900-1962
#'
#' Dataset containing estimated statistics for timber products. All vectors are numeric.
#' Most data is for products in the Industrial Roundwood Class. Each column except the first two and the last one
#' are in this category.
#'
#' Columns may not add to total because of rounding.
#'
#' @format A data frame with 85 observations on 38 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Dom.Prod.Tot}{Total Domestic Production}
#' \item{ApparentConsumption}{Apparent Consumption}
#' \item{Dom.Prod}{Domestic Production, Total Ind RW}
#' \item{Imports}{Net Imports, Total Ind RW}
#' \item{Prod.SW}{Softwood Production, Total Ind RW}
#' \item{Prod.HW}{Hardwood Production , Total Ind RW}
#' \item{LogChipImports.SW}{Softwood Log and Chip Imports, Total Ind RW}
#' \item{LogChipExports.SW}{Softwood Log and Chip Exports, Total Ind RW}
#' \item{LogChipImports.HW}{Hardwood Log and Chip Imports, Total Ind RW}
#' \item{LogChipExports.HW}{Hardwood Log and Chip Exports, Total Ind RW}
#' \item{SW.PctOfProd}{Softwoods as Percent of Production, Total Ind RW}
#' \item{Exports.SW.PctOfProd}{Softwood Exports as Percent of Production, Total Ind RW}
#' \item{Exports.HW.PctOfProd}{Hardwood Exports as Percent of Production, Total Ind RW}
#' \item{Imports.SW.PctOfConsump}{Softwood Imports as Percent of Production, Total Ind RW}
#' \item{Imports.HW.PctOfConsump}{Hardwood Imports as Percent of Production, Total Ind RW}
#' \item{SW.Lumber.PctOfTotLumbProd}{Softwood Lumber as Percent of Total Lumber Production, Total Ind RW}
#' \item{DV.Exports.Log.PctOfProd}{Dollar Value of Log Exports as Percent of Production, Total Ind RW}
#' \item{DV.Imports.Log.PctOfProd}{Dollar Value of Log Imports as Percent of Consumption, Total Ind RW}
#' \item{Estimated.Imports}{Estimated Imports, Total Ind RW}
#' \item{Estimated.Exports}{Estimated Exports, Total Ind RW}
#' \item{Adj.Imports}{Adjusted Imports, Total Ind RW}
#' \item{Adj.Exports}{Adjust Exports, Total Ind RW}
#' \item{Estimated.Imports(tons)}{Estimated Imports in tons, Total Ind RW}
#' \item{Estimated.Exports(tons)}{Estimated Exports in tons, Total Ind RW}
#' \item{Adj.Imports(tons)}{Adjusted Imports in tons, Total Ind RW}
#' \item{Adj.Exports(tons)}{Adjust Exports in tons, Total Ind RW}
#' \item{ApparentConsumption.Total}{Apparent Conusmption, Total Ind RW.}
#' \item{SawLogs.Dom.Prod}{Domestic Production of Saw Logs}
#' \item{SawLogs.NetImports}{Net Imports of Saw Logs.
#' Net imports of lumber converted to cubic feet roundwood.  Small quantities of imported saw logs (roundwood form) are included under domestic production.}
#' \item{SawLogs.ApparentConsumption}{Apparent Consumption of Saw Logs
#' Includes changes in stocks beginning in 1935.}
#' \item{VeneerLogs.Dom.Prod}{Domestic Production of Veneer Logs}
#' \item{VeneerLogs.NetImports}{Net Imports of Veneer Logs
#' Net imports of veneer logs represent the equivalent net imports of veneer and plywood converted to board feet log scale, and then to cubic feet round-wood.
#' The small volume of veneer logs imported (roundwood form) is included under domestic production.}
#' \item{VeneerLogs.ApparentConsumption}{Apparent Consumption of Veneer Logs.}
#' \item{PulpWood.Dom.Prod}{Domestic Production of Pulpwood}
#' \item{PulpWood.NetImports}{Net Imports of Pulpwood}
#' \item{PulpWood.ApparentConsumption}{Apparent Consumption of Pulpwood}
#' \item{Other.ApparentConsumption}{Apparent Consumption of Other Products}
#' \item{FuelWood.ApparentConsumption}{Apparent Consumption of Fuelwood}
#' }
#' @source US Department of Commerce and Agriculture and USFS Estimations
#' @keywords piet_irw
"piet_irw"


#' United States imports and exports of plywood, by softwoods and hardwoods, 1935-62 (with estimates of 1927-34)
#'
#' Dataset containing import and export statistics for plywood products.
#' All units are in Thousand Sq Ft. All vectors are numeric.
#'
#' @format A data frame with 36 observations on 8 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Imports.Tot}{Total Plywood Imports}
#' \item{SW.Imports}{Softwood Imports}
#' \item{HW.Imports.Tot}{Total Hardwood Imports}
#' \item{HW.Imports.Birch}{Birch Imports}
#' \item{HW.Imports.Other}{Other Hardwoods Imports}
#' \item{Exports.Tot}{Total Plywood Exports}
#' \item{SW.Exports}{Softwood Exports}
#' \item{HW.Exports}{Hardwood Exports}
#' }
#' @source US Department of Commerce, Bureau of the Census
"ie_pw"



#' Imports and exports of veneer, by kind, in the US 1935-1962 (estimates of 1927-34).
#'
#' Dataset containing import and export statistics for veneer products.
#' All units are in 1000 Sq. Ft. All vectors are numeric.
#'
#' @format A data frame with 36 observations on 6 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Imports.Tot}{Total Veneer Imports}
#' \item{Imports.BirchMaple}{Birch or Maple Veneer Imports}
#' \item{Imports.Other}{Other Products Imports}
#' \item{Exports.Tot}{Total Veneer Exports}
#' \item{Exports.Fancy.Face.Figured.Special}{Exports of fancy, face, figured and special Veneer Products}
#' \item{Exports.UtilityCommercialContainer}{Exports of utility, commerical and container Veneer Products}
#' }
#' @source US Department of Commerce, Bureau of the Census
#' @keywords ie_veneer
"ie_veneer"



#' Lumber production, imports, exports, and consumption, by softwoods and hardwoods, 1965-1999.
#'
#' Dataset containing statistics for lumber products.
#' All units are in Billion Board ft. unless otherwise specified. All vectors are numeric.
#'
#' @format A data frame with 56 observations of 15 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Prod.Tot}{Total Lumber Production}
#' \item{Prod.SW}{Softwood Lumber Production}
#' \item{Prod.HW}{Hardwood Lumber Production}
#' \item{Imports.Tot}{Total Lumber Imports}
#' \item{Imports.SW}{Softwood Lumber Imports}
#' \item{Imports.HW}{Hardwood Lumber Imports}
#' \item{Exports.Tot}{Total Lumber Exports}
#' \item{Exports.SW}{Softwood Lumber Exports}
#' \item{Exports.HW}{Hardwood Lumber Exports}
#' \item{Consump.Tot}{Total Lumber Consumption}
#' \item{Consump.SW}{Softwood Lumber Consumption}
#' \item{Consump.HW}{Hardwood Lumber Consumption}
#' \item{PerCapConsump.Tot}{Total Per Capita Consumption, in Board ft.}
#' \item{PerCapConsump.SW}{Softwood Per Capita Consumption, in Board ft.}
#' \item{PerCapConsump.HW}{Hardwood Per Capita Consumption, in Board ft.}
#' }
#' @source None Listed (need to find)
#' @keywords pietc_l2
"pietc_l2"



#' Plywood production, imports, exports, and consumption, by softwoods and hardwoods, 1965-1999
#'
#' Dataset containing statisics for plywood products(3/8-in. basis).
#' All units are in Million Square ft. unless otherwise specified.
#'
#' @format A data frame with 56 observations of 15 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Prod.Tot}{Total Plywood Production}
#' \item{Prod.SW}{Softwood Plywood Production}
#' \item{Prod.HW}{Hardwood Plywood Production}
#' \item{Imports.Tot}{Total Plywood Imports}
#' \item{Imports.SW}{Softwood Plywood Imports}
#' \item{Imports.HW}{Hardwood Plywood Imports}
#' \item{Exports.Tot}{Total Plywood Exports}
#' \item{Exports.SW}{Softwood Plywood Exports}
#' \item{Exports.HW}{Hardwood Plywood Exports}
#' \item{Consump.Tot}{Total Plywood Consumption}
#' \item{Consump.SW}{Softwood Plywood Consumption}
#' \item{Consump.HW}{Hardwood Plywood Consumption}
#' \item{PerCapConsump.Tot}{Total Per Capita Consumption, in Square ft.}
#' \item{PerCapConsump.SW}{Softwood Per Capita Consumption, in Square ft.}
#' \item{PerCapConsump.HW}{Hardwood Per Capita Consumption, in Square ft.}
#' }
#' @source
#' US Dept. of Commerce, Bureau of Census, American Plywood Association
#' The Engineered Wood Association, US Dept. of Agriculture, Foreign Agriculture Service
#' @keywords pietc_pw2
"pietc_pw2"



#' Production, imports, exports, and consumption of structural panel products, by type, 1980-1999
#'
#' Datasets containing statistics for various structural panel products.
#' All units are in Million Square ft.
#'
#' @format A data frame with 41 observations and 21 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Prod.Tot}{Total Production}
#' \item{Prod.SWPlywood}{Softwood Plywood Production}
#' \item{Prod.OSP}{Other Structural Panels Production}
#' \item{Imports.Tot}{Total Imports}
#' \item{Imports.SWPlywood}{Softwood Plywood Imports}
#' \item{Imports.OSP}{Other Structual Panels Imports}
#' \item{Exports.Tot}{Total Exports}
#' \item{Exports.SWPlywood}{Softwood Plywood Exports}
#' \item{Exports.OSP}{Other Structural Panels Exports}
#' \item{Consump.Tot}{Total Consumption}
#' \item{Consump.SWPlywood}{Softwood Plywood Consumption}
#' \item{Consump.OSP}{Other Structural Panels Consumption}
#' }
#' @source
#' American Plywood Association, US Dept. of Agriculture
#' Foreign Agricultural Service
#' @keywords pietc_sp
"pietc_sp"



#' Paper and board production and fibrous materials consumed in the manufacture of paper and board, 1965-1999
#'
#' Dataset containing consumption and production statistics for various board and paper products.
#' Units are specified below.
#'
#' @format A data frame with 56 observations of 19 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Prod.PaperBoard}{Prod.PaperBoard. Paper and Paperboard Production, in 1000 Tons}
#' \item{Consump.Tot}{Total Consumption of Fibrous Materials, in 1000 Tons}
#' \item{Consump.WoodPulp}{Consumption of Woodpulp, in 1000 Tons}
#' \item{Consump.RecPaper}{Consumption of Recovered Paper, in 1000 Tons}
#' \item{Consump.Other}{Consumption of Other Fibrous Materials, in 1000 Tons}
#' \item{Consump.PerTonProd.Tot}{Total Consumption of Fibrous Material per ton of Paper and Board Produced, in Tons}
#' \item{Consump.PerTonProd.WoodPulp}{Consumption of Wood pulp per ton of Paper and Board Produced, in Tons}
#' \item{Consump.PerTonProd.RecPaper}{Consumption of Recovered Paper per ton of Paper and Board Produced, in Tons}
#' \item{Consump.PerTonProd.Other}{Consumption of Other Fibrous Materials per ton of Paper and Board Produced, in Tons}
#' \item{RagsOther.RecPaperUtiRatePerc}{Rags and Other: Recovered Paper Utilization Rate Percent}
#' \item{RagsOther.Prod.Estimated}{Rags and Other: Estimated Production, in Thousand Tons}
#' \item{RagsOther.Imports.Estimated}{Rags and Other: Estimated Imports, in Thousand Tons}
#' \item{RagsOther.Exports.Estimated}{Rags and Other: Estimated Exports, in Thousand Tons}
#' }
#' @source American Paper Institute, American Forest & Paper Association: Statistics of Paper, Paperboard & Wood Pulp
"pietc_pbfm"



#' Paper and board new supply and recyclable paper consumption, exports, imports and total recovered, 1965-1999
#'
#' Dataset containing consumption and production statistics for various board and paper products.
#' All units are in thousand tons unless otherwise specified.
#' Recovery rate is the ratio of total recovered paper collected to new supply of paper and paperboard.
#'
#' @format A data frame with 56 observations of 19 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{PaperBoardNewSupply}{New supply of Paper and Board}
#' \item{RecPap.ConsumedatPaperBoardMills}{Recyclable Paper Consumed at Paper and Board Mills}
#' \item{RecPap.MoldedPulpInsulationandOther}{Recyclable Paper for Molded pulp, insulation and other uses}
#' \item{RecPap.Exports}{Recyclable Paper Exports}
#' \item{RecPap.Imports}{Recyclable Paper Imports}
#' \item{RecPap.TotRecovered}{Total Recyclable Paper Recovered}
#' \item{RecPap.RecoveryRate}{Recyclable Paper Recovery Rate as a percent}
#' \item{NoName}{Recylable Paper Totals for certain years}
#' \item{Ratio.ExportstoProduction}{Ratio of Exports to Production}
#' \item{Ratio.ImportstoProduction}{Ratio of Imports to Production}
#' }
#' @source 1972-Present, American Forest and Paper Association, 1999 Annual Statistical Summary Recovered
#' Paper Utilization, p.81 (6). 1965-1971, American Forest and Paper Assocation (2,4,5).
"pietc_pbrp"



#' Wood pulp production, imports, exports, and consumption, 1965-1999
#'
#' Dataset containing woodpulp statistics.
#' All units are in thousand pounds unless otherwise specified.
#' Consumption = Production + Imports - Exports
#'
#' @format A data frame with 56 observations of 15 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Woodpulp.Production}{Wood pulp Production}
#' \item{Woodpulp.Imports}{Wood pulp Imports}
#' \item{Woodpulp.Imports.PercConsump}{Imports as Percent of Consumption}
#' \item{Woodpulp.Exports}{Wood pulp Exports}
#' \item{Woodpulp.Exports.PercProd}{Exports as Percent of Production}
#' \item{Woodpulp.Consump.Tot}{Total Consumption}
#' \item{Woodpulp.Consump.PerCap(pounds)}{Per Capita Consumption in pounds}
#' }
#' @source
#' U.S. Department of Commerce, Bureau of the Census (52,56,57); American Forest
#' and Paper Association (2,4); United Nations, Food and Agriculture Organization (29).
#' 1993-1997 revised to include dissolving and special alpha pulps, which is consistent with earlier years.
#' American Paper Institute; American Forest & Paper Association: Statistics of Paper, Paperboard & Wood Pulp;
#'  1977, 1982, 1990, 1995, 1998, 1999 annual Statistics reports (4).
#'  1965-1967, 1970, 1973, 1977-1978, 1986-1988 & 1997 revised.
#'  1966, 1968, 1972, 1975, 1985-1987 & 1996-1997 revised.
"pietc_wp2"



#' Production, imports, exports, and consumption of timber products, by major product, 1965-1999
#'
#' Dataset containing statistics for various timber products.
#' Prior to 1989, pulpwood chips are not included in total production.
#' All units are in million cubic ft.
#'
#' @format A data frame with 56 observations of 24 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{AllProducts.Prod}{Production for all timber products}
#' \item{AllProducts.Consump}{Consumption for all timber products}
#' \item{Prod.Tot}{Total Production for Industrial Roundwood}
#' \item{Imports.Tot}{Total Imports for Industrial Roundwood}
#' \item{Exports.Tot}{Total Exports for Industrial Roundwood}
#' \item{Consump.Tot}{Total Consumption for Industrial Roundwood}
#' \item{Lumber.Prod}{Production for Lumber Products}
#' \item{Lumber.Imports}{Imports for Lumber Products}
#' \item{Lumber.Exports}{Exports for Lumber Products}
#' \item{Lumber.Consump}{Consumption for Lumber Products}
#' \item{PlywoodVeneer.Prod}{Production for Plywood and Veneer Products}
#' \item{PlywoodVeneer.Imports}{Imports for Plywood and Veneer Products}
#' \item{PlywoodVeneer.Exports}{Exports for Plywood and Veneer Products}
#' \item{PlywoodVeneer.Consump}{Consumption for Plywood and Veneer Products}
#' \item{Pulpwood.Prod}{Production for Pulpwood Products}
#' \item{Pulpwood.Imports}{Imports for Pulpwood Products}
#' \item{Pulpwood.Exports}{Exports for Pulpwood Products}
#' \item{Pulpwood.Consump}{Consumption for Pulpwood Products}
#' \item{OtherIndProductsandConsump}{Other Industrial Products, production. and consumption}
#' \item{Logs.Imports}{Log Imports}
#' \item{Logs.Exports}{Log Exports}
#' \item{PulpwoodChip.Imports}{Pulpwood chip Imports}
#' \item{PulpwoodChip.Exports}{Pulpwood chip Exports}
#' \item{FuelwoodProdandConsump}{Fuelwood production and consumption}
#' }
#' @source
#' U.S. Department of Agriculture, Forest Service(16, 69); Data may not add to totals because of rounding; Data have been revised.
"pietc_t2"

#' Particleboard and medium-density fiberboard production, imports,
#'
#' Dataset containing various statistics for particleboard and medium-density fiberboard.
#' All units are in Million Square ft unless otherwise specified.
#'
#' @format A data frame with 56 observations of 7 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Particle_MDF.Prod.Tot}{Total Particleboard and Medium-Density Fiberboard Production}
#' \item{Particle_MDF.Prod.ParticleBoard}{Particleboard Production}
#' \item{Particle_MDF.Prod.MediumDensityFiberboard}{Medium-Density Fiberboard Production}
#' \item{Particle_MDF.Imports}{Particleboard and Medium-Density Fiberboard Imports}
#' \item{Particle_MDF.Exports}{Particleboard and Medium-Density Fiberboard Exports}
#' \item{Particle_MDF.Consump.Tot}{Total Particleboard and Medium-Density Fiberboard Consumption}
#' \item{Particle_MDF.Consump.PerCap(Sq ft)}{Per Capita Consumption of particleboard and medium-density fiberboard, in Square ft.}
#' }
#' @source
#' National Particleboard Association (20); U.S. International Trade Commission (69);
#' U.S. Department of Agriculture (32).  Data may not add to totals because of rounding.
"pietc_parbfb2"

#' Lumber production, imports, exports, and consumption, by softwoods and hardwoods, 1950-87
#'
#' Dataset containing various statistics for lumber products.
#' All units are in Billion Board Ft unless otherwise specified.
#' Year 1987 includes small volumes of mixed species (not classified as softwoods or hardwoods).
#' Data may not add to totals because of rounding. Estimates of softwood lumber production for certain years in the
#' late 1970's and early 1980's by the Bureau of the Census and some major softwood industry associations currently vary by as much as
#' 5 percent. Cooperative studies are underway to resolve these differences. Revisions made by Census as a result of these studies
#' will be reported in future issues of U.S. timber production, trade, consumption, and price statistics.
#'
#' @format A data frame with 38 observations of 15 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Prod.Tot}{Total Lumber Production}
#' \item{Prod.SW}{Softwood Lumber Production}
#' \item{Prod.HW}{Hardwood Lumber Prodcution}
#' \item{Imports.Tot}{Total Lumber Imports}
#' \item{Imports.SW}{Softwood Lumber Imports}
#' \item{Imports.HW}{Hardwood Lumber Imports}
#' \item{Exports.Tot}{Total Lumber Exports}
#' \item{Exports.SW}{Softwood Lumber Exports}
#' \item{Exports.HW}{Hardwood Lumber Exports}
#' \item{Consump.Tot}{Total Lumber Consumption}
#' \item{Consump.SW}{Softwood Lumber Consumption}
#' \item{Consump.HW}{Hardwood Lumber Consumption}
#' \item{PerCapitaConsump.Tot}{Total Lumber Consumption Per Capita, in Board ft.}
#' \item{PerCapitaConsump.SW}{Softwood Lumber Consumption Per Capita, in Board ft.}
#' \item{PerCapitaConsump.HW}{Hardwood Lumber Consumption Per Capita, in Board ft.}
#' }
#' @source
#' U.S. Department of Commerce. Bureau of the Census (43.51.52)
#' U.S. Department of Agriculture, Forest Service; National Forest Products Association (16).
#' Data for selected years 1899.1949 in "The demand and price situation for forest products", 1964, table 7 (31)
"pietc_l"



#' Plywood production, imports, exports, and consumption, by softwoods and hardwoods, 1950-87
#'
#' Dataset containing various statistics for plywood products. (3/8-inch basis)
#' All units are in Million Square ft unless otherwise specified.
#' Year 1987 includes small volumes of mixed species (not classified as softwoods or hardwoods).
#'
#' @format A data frame with 38 observations of 15 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Prod.Tot}{Total Plywood Production}
#' \item{Prod.SW}{Softwood Plywood Production}
#' \item{Prod.HW}{Hardwood Plywood Production}
#' \item{Imports.Tot}{Total Plywood Imports}
#' \item{Imports.SW}{Softwood Plywood Imports}
#' \item{Imports.HW}{Hardwood Plywood Imports}
#' \item{Exports.Tot}{Total Plywood Exports}
#' \item{Exports.SW}{Softwood Plywood Exports}
#' \item{Exports.HW}{Hardwood Plywood Exports}
#' \item{Consump.Tot}{Total Plywood Consumption}
#' \item{Consump.SW}{Softwood Plywood Consumption}
#' \item{Consump.HW}{Hardwood Plywood Consumption}
#' \item{PerCapitaConsump.Tot}{Total Plywood Consumption Per Capita, in Square ft.}
#' \item{PerCapitaConsump.SW}{Softwood Plywood Consumption Per Capita, in Square ft.}
#' \item{PerCapitaConsump.HW}{Hardwood Plywood Consumption Per Capita, in Square ft.}
#' }
#' U.S. Department of Commerce, Bureauot Census (40,50.51,52)
#' American Plywood Association (5).
"pietc_pw"



#' Production, imports, exports. and consumption of timber products, by major product, 1950-87
#'
#' Dataset containing statistics for various timber products (hardwood and softwood).
#' All units are Million cubic ft (roundwood equivalent) unless otherwise specified.
#'
#' @format A data frame with 38 observations of 24 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{AllProducts.Prod}{Production for all timber products}
#' \item{AllProducts.Consump}{Consumption for all timber products}
#' \item{Tot.Prod}{Total Production for major timber products}
#' \item{Tot.Imports}{Total Imports for major timber products}
#' \item{Tot.Exports}{Total Exports for major timber products}
#' \item{Tot.Consump}{Total Consumption for major timber products}
#' \item{Lumber.Prod}{Lumber Products Prodcution}
#' \item{Lumber.Imports}{Lumber Products Imports}
#' \item{Lumber.Exports}{Lumber Products Exports}
#' \item{Lumber.Consump}{Lumber Products Consumption}
#' \item{PlywoodVeneer.Prod}{Plywood and Veneer Products Production}
#' \item{PlywoodVeneer.Imports}{Plywood and Veneer Products Imports}
#' \item{PlywoodVeneer.Exports}{Plywood and Veneer Products Exports}
#' \item{PlywoodVeneer.Consump}{Plywood and Veneer Products Consumption}
#' \item{PulpProducts.Prod}{Pulp Products Production}
#' \item{PulpProducts.Imports}{Pulp Products Imports}
#' \item{PulpProducts.Exports}{Pulp Products Exports}
#' \item{PulpProducts.Consump}{Pulp Products Consumption}
#' \item{OtherIndProducts.ProdandConsump}{Production and Consumption for other industrial products}
#' \item{Logs.Imports}{Log Imports}
#' \item{Logs.Exports}{Log Exports}
#' \item{PulpwoodChip.Exports}{Pulpwood-chip Exports}
#' \item{Fuelwood.ProdandConsump}{Production and Consumption for fuelwood products}
#' \item{LogChipExports.PercofProduction}{Log and Chip exports as a percent of production}
#' }
#' @source
#' Based on data published by the U.S. Departments of Commerce and Agriculture.
#' Data for 1900-49 in "The demand and price situation for forest products 1964", table 2 (31),
"pietc_t"



#' Production, imports, exports, and consumption of softwood timber products, by major product, 1950-87
#'
#' Dataset containing statistics for softwood timber products.
#' All units are in Million cubic ft(roundwood equivalent). All vectors
#' are numeric.
#'
#' @format A data frame with 38 observations of 24 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{AllProducts.Prod}{Production for all timber products}
#' \item{AllProducts.Consump}{Consumption for all timber products}
#' \item{Tot.Prod}{Total Production for major timber products}
#' \item{Tot.Imports}{Total Imports for major timber products}
#' \item{Tot.Exports}{Total Exports for major timber products}
#' \item{Tot.Consump}{Total Consumption for major timber products}
#' \item{Lumber.Prod}{Lumber Products Prodcution}
#' \item{Lumber.Imports}{Lumber Products Imports}
#' \item{Lumber.Exports}{Lumber Products Exports}
#' \item{Lumber.Consump}{Lumber Products Consumption}
#' \item{PlywoodVeneer.Prod}{Plywood and Veneer Products Production}
#' \item{PlywoodVeneer.Imports}{Plywood and Veneer Products Imports}
#' \item{PlywoodVeneer.Exports}{Plywood and Veneer Products Exports}
#' \item{PlywoodVeneer.Consump}{Plywood and Veneer Products Consumption}
#' \item{PulpProducts.Prod}{Pulp Products Production}
#' \item{PulpProducts.Imports}{Pulp Products Imports}
#' \item{PulpProducts.Exports}{Pulp Products Exports}
#' \item{PulpProducts.Consump}{Pulp Products Consumption}
#' \item{OtherIndProducts.ProdandConsump}{Production and Consumption for other industrial products}
#' \item{Logs.Imports}{Log Imports}
#' \item{Logs.Exports}{Log Exports}
#' \item{PulpwoodChip.Exports}{Pulpwood-chip Exports}
#' \item{Fuelwood.ProdandConsump}{Production and Consumption for fuelwood products}
#' \item{LogChipExports.PercofProduction}{Log and Chip exports as a percent of production}
#' }
#' Based on data published by the U.S. Departments of Commerce and Agriculture.
#' Data for 1900-49 in The demand and price situation for forest products 1964, table 2 (31),
"pietc_swt"



#' Production, imports, exports and consumption of insulating board, 1950-1987
#' Also includes historical production and exports data from 1925-1950.
#'
#' Dataset contains statistics for insulating board.
#' All units are in million square ft unless otherwise specified. All vectors are numeric.
#'
#' @format A data frame with 72 observations of 5 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Hardboard.Prod}{Hardboard Production}
#' \item{Hardboard.Import}{Hardboard Imports}
#' \item{Hardboard.Exports}{Hardboard Exports}
#' \item{Hardboard.Consump.Tot}{Total Hardboard Consumption}
#' \item{Hardboard.Consump.PerCapita}{Hardboard Consumption Per Capita}
#' }
#' @source
#' U.S. Department of Commerce, Bureau of the Census (48,51, 52);
#' American Paper Institute (3, 4); American Hardboard Association.
#' Data from Ince,2000, Industrial Wood Productivity in the United States, 1900-1998.
"pietc_hb"



#' Production, imports, exports, and consumption of hardwood timber products, by major product, 1950-87
#'
#' Dataset containing statistics for hardwood timber products.
#' All units are in million cubic ft.
#'
#' @format A data frame with 48 observations of 22 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{AllProduction.Prod}{Total production of all products}
#' \item{AllProduct.Consump}{Total consumption of all products}
#' \item{Ind.RW.Tot.Prod}{Total production of all industrial roundwood products}
#' \item{Ind.RW.Tot.Imports}{Total imports of all industrial roundwood products}
#' \item{Ind.RW.Tot.Exports}{Total exports of all industrial roundwood products}
#' \item{Ind.RW.Tot.Consump}{Total consumption of all industrial roundwood products}
#' \item{Ind.RW.Lum.Prod}{Production of lumber products}
#' \item{Ind.RW.Lum.Imports}{Imports of lumber products}
#' \item{Ind.RW.Lum.Exports}{Exports of lumber products}
#' \item{Ind.RW.Lum.Consump}{Consumption of lumber products}
#' \item{Ind.RW.PlyandVen.Prod}{Production of plywood and veneer products}
#' \item{Ind.RW.PlyandVen.Imports}{Imports of plywood and veneer products}
#' \item{Ind.RW.PlyandVen.Exports}{Exports of plywood and veneer products}
#' \item{Ind.RW.PlyandVen.Consump}{Consumption of plywood and veneer products}
#' \item{Ind.RW.Pulp.Prod}{Production of pulp products}
#' \item{Ind.RW.Pulp.Imports}{Imports of pulp products}
#' \item{Ind.RW.Pulp.Exports}{Exports of pulp products}
#' \item{Ind.RW.Pulp.Consump}{Consumption of pulp products}
#' \item{Ind.RW.OtherIndustrial.ProdAndConsump}{Production and consumption of Other industrial roundwood products}
#' \item{Ind.RW.Logs.Imports}{Imports of logs}
#' \item{Ind.RW.Logs.Exports}{Exports of logs}
#' \item{FuelWood.ProdAndConsumption}{Production and Consumption of Fuelwood}
#' \item{UnNamed1}{Unnamed}
#' }
#' @source Based on data published by the U.S. Departments of Commerce and Agriculture.
"pietc_hwt"



#' U.S. Paper & Paperboard, Production, Imports, Exports, Apparent Consumption, 1900 - 2050.
#'
#' Production & consumption after 1958 excludes wet machine board & insulating board; trade & consumption exclude converted 'products'.
#' Production & consumption include building paper but not insulating board or hardboard.
#' Data set containing statistics for paper and paperboard in the U.S.
#' Data are in thousands of short tons per year.
#'
#' @format A data frame with 48 observations of 22 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Paper.Board.Prod}{Paper and board production}
#' \item{Paper.Board.Imports}{Paper and board imports}
#' \item{Paper.Board.ApparentConsumption}{Paper and board apparent consumption}
#' \item{Population}{United states population in thousands}
#' \item{Paper.Board.ConsumptionPerCapita}{Paper and board consumption per capita, in pounds}
#' }
#' @source Historical Paper & Paperboard - American Forest & Paper Association, AF&PA (American Paper Institute for earlier years)
#' Projections - RPA timber assessment, PNW-GTR-560 (see http://www.fs.fed.us/pnw/pubs/gtr560/)
"IncePaper"

#' U.S. Annual industrial wood product in conventional units of measure.
#'
#' "Lumber made at pallet plants" is a Forest Service estimate of lumber cut from roundwood at pallet plants.
#' Other lumber (e.g., purchased lumber) used by pallet makers is accounted for under hardwood and softwood lumber production.
#' Paper and paperboard production includes "Total Paper", "Total Paperboard", and "Building Paper" production.
#' Total production of pulp, paper and paperboard includes market pulp produced for export.
#' Fuelwood estimates are shown for comparison only and are not included in industrial wood products.
#'
#' Units are in Million feet squared 3/8in.
#'
#' @format A data frame with 151 observations of  25 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{SW.Ply}{Softwood Plywood units: million ft^2, 3/8 in.}
#' \item{OSB.Wafer.board}{OSB/ Wafer board units: million ft^2, 3/8 in.}
#' \item{Lam.Ven.Lumb}{Laminated Veneer lumber units: million ft^3}
#' \item{HW.Ply.Ven}{Hardwood, Plywood and veneer units: million ft^2, 3/8 in.}
#' \item{SW.Lumb}{Softwood lumber units: million bd. ft.}
#' \item{HW.Lumb}{Hardwood lumber units: million bd. ft.}
#' \item{Lumb.Pallet.Plant}{Lumber made at Pallet plants units: million bd. ft.}
#' \item{PartBoard.Prod}{Particleboard production units: million ft^2, 3/4 in.}
#' \item{Hardboard.Prod}{Hardboard production units: million ft^2, 1/8 in.}
#' \item{MDF.Prod}{MDF production units: million ft^2, 3/4 in.}
#' \item{Pulp.Paper.Board}{Pulp, paper and board production units: Thousand tons}
#' \item{Other.Products}{Other industrial products units: million ft^3}
#' \item{InsulatingBoard}{Insulating board units: million ft^2, 1/2 in.}
#' \item{Fuelwood}{Fuelwood units: million ft^3}
#' \item{Fuelwood.Tons}{Fuelwood units: Thousand short tons}
#' \item{Log.Chip.Exports}{Log and Chip exports units: Thousand short tons}
#' \item{RW.Dom.Prod}{Roundwood Equivalent of Domestic Product Output units: Thousand short tons}
#' \item{Estimated.Tot.Dom.Timber}{Estimated Total Domestic Timber Harvest units: Thousand short tons}
#' }
#' @source
#' Softwood plywood, OSB, and LVL production data (1960-1998) from APA--The Engineered Wood Association (formerly American Plywood Assoc.); see APA Economics Report E-65 (1999).
#'
#' Hardwood plywood production data from Dept. of Commerce (to 1988); 1989-1990 data from Hardwood Plywood & Veneer Association;
#' later estimates based on trends in value of shipments (Dept. of Commerce); hardwood veneer based on Census of Manufactures data and trend in value of shipments.
#'
#' Lumber production 1900-1946 based on H.B. Steer (USDA Forest Service, 1948, Table 1, "Estimated Production")
#'
#' Lumber production 1947-1976 based on Commerce Department data and Forest Service estimates.  Lumber production 1976-1998 from American Forest & Paper Association (AF&PA) Statistical Roundup report (1996 hardwood estimated by Forest Service; 1997-1998 hardwood estimate from Miller Freeman).
#'
#' [Note that Commerce Dept. reported hardwood lumber production is understood to underestimate actual production as reflected in Forest Service estimates since 1900.]
#'
#' Particleboard and MDF production data from Composite Panel Association (based on production data 1959-1977, and 1995-1997; otherwise based on shipments; 1998 data are estimated).
#'
#' Hardboard production based on shipments data from American Hardboard Association (1964-1997) and as reported by Miller Freeman (1998); 1998 figure is estimated.
#'
#' Pulp, paper, and paperboard production data are from AF&PA, formerly API (Statistics of Paper, Paperboard & Woodpulp).
#'
#' Miscellaneous wood product production data through 1988 were obtained from U.S. Timber Production, Trade, Consumption, and Price Statistics reports (USDA Forest Service).
#' Miscellaneous wood product production for 1996 based on timber product output tables in (draft) 1997 RPA Inventory Data Tables; intervening and subsequent data are extrapolated.
#'
#' Insulating board production data are derived using earlier data from Commerce Department MA26A reports (in square feet of product output) to convert tonnage reported in AF&PA and earlier API reports (Statistics of Paper, Paperboard & Woodpulp).
#'
#' Fuelwood data are Forest Service estimates (Skog and others, FPL).
"p_wfw"



#' Insulating board production, imports, exports, and consumption, 1965-1999.
#'
#' All units in thousand tons except when specified otherwise.
#'
#' @format A data frame with 56 observations of  5 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Insulboard.Production}{Insulating board production}
#' \item{Insulboard.Imports}{Insulating board imports}
#' \item{Insulboard.Exports}{Insulating board exports}
#' \item{Insulboard.Total.Consumption}{Insulating board total}
#' \item{Insulboard.PerCapita.Consumption}{Per Capita Consumption in Pounds}
#' }
#' @source USFS Estimations and Bureau of Census Data
"pietc_hw2"



#' Hardboard production, imports, exports and consumption, 1965-1999.
#'
#' All units are in million square feet except when specified otherwise.
#'
#' @format A data frame with 56 observations on 5 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Hardboard.Production}{Hardboard production}
#' \item{Hardboard.Imports}{Hardboard imports}
#' \item{Hardboard.Exports}{Hardboard exports}
#' \item{Hardboard.Total.Consumption}{Total Hardboard Consumption}
#' \item{Hardboard.PerCapita.Consumption}{Per Capita Hardboard Consumption in Square feet}
#' }
#' @source USFS Estimations and Bureau of Census Data
"pietc_ib2"



#' Production, imports, exports, and consumption of softwood products, by major product, 1965-1999.
#'
#' All units are in million cubic feet except when specified otherwise.
#'
#' @format A data frame with 56 observations on 34 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{AllProduction.Prod}{All products production}
#' \item{AllProduct.Consump}{All products consumption}
#' \item{Ind.RW.Tot.Prod}{Industrial roundwood uses total production}
#' \item{Ind.RW.Tot.Imports}{Industrial roundwood uses total imports}
#' \item{Ind.RW.Tot.Exports}{Industrial roundwood uses total exports}
#' \item{Ind.RW.Tot.Consump}{Industrial roundwood uses total consumption}
#' \item{Ind.RW.Lum.Prod}{Industrial roundwood uses lumber production}
#' \item{Ind.RW.Lum.Imports}{Industrial roundwood uses lumber imports}
#' \item{Ind.RW.Lum.Exports}{Industrial roundwood uses lumber exports}
#' \item{Ind.RW.Lum.Consump}{Industrial roundwood uses lumber consumption}
#' \item{Ind.RW.PlyandVen.Prod}{Industrial roundwood uses plywood and veneer production}
#' \item{Ind.RW.PlyandVen.Imports}{Industrial roundwood uses plywood and veneer imports}
#' \item{Ind.RW.PlyandVen.Exports}{Industrial roundwood uses plywood and veneer exports}
#' \item{Ind.RW.PlyandVen.Consump}{Industrial roundwood uses plywood and veneer consumption}
#' \item{Ind.RW.Pulp.Prod}{Industrial roundwood uses pulp-based products production}
#' \item{Ind.RW.Pulp.Imports}{Industrial roundwood uses pulp-based products imports}
#' \item{Ind.RW.Pulp.Exports}{Industrial roundwood uses pulp-based products exports}
#' \item{Ind.RW.Pulp.Consump}{Industrial roundwood uses pulp-based products consumption}
#' \item{Ind.RW.OtherIndustrial.ProdAndConsump}{Other industrial product production and consumption}
#' \item{Ind.RW.Logs.Imports}{Industrial roundwood uses log imports}
#' \item{Ind.RW.Logs.Exports}{Industrial roundwood uses log exports}
#' \item{Ind.RW.Pulpchip.Imports}{Industrial roundwood uses pulpwood chip imports}
#' \item{Ind.RW.Pulpchip.Exports}{Industrial roundwood uses pulpwood chip exports}
#' \item{FuelWood.ProdAndConsumption}{Fuelwood production and consumption}
#' }
#' @source USFS Estimations and Bureau of Census Data
"pietc_swt2"



#' Production, imports, exports, and consumption of hardwood products, by major product, 1965-1999.
#'
#' All units are in million cubic feet except when specified otherwise.
#'
#' @format A data frame with 56 observations on 31 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{AllProduction.Prod}{All products production}
#' \item{AllProduct.Consump}{All products consumption}
#' \item{Ind.RW.Tot.Prod}{Industrial roundwood uses total production}
#' \item{Ind.RW.Tot.Imports}{Industrial roundwood uses total imports}
#' \item{Ind.RW.Tot.Exports}{Industrial roundwood uses total exports}
#' \item{Ind.RW.Tot.Consump}{Industrial roundwood uses total consumption}
#' \item{Ind.RW.Lum.Prod}{Industrial roundwood uses lumber production}
#' \item{Ind.RW.Lum.Imports}{Industrial roundwood uses lumber imports}
#' \item{Ind.RW.Lum.Exports}{Industrial roundwood uses lumber exports}
#' \item{Ind.RW.Lum.Consump}{Industrial roundwood uses lumber consumption}
#' \item{Ind.RW.PlyandVen.Prod}{Industrial roundwood uses plywood and veneer production}
#' \item{Ind.RW.PlyandVen.Imports}{Industrial roundwood uses plywood and veneer imports}
#' \item{Ind.RW.PlyandVen.Exports}{Industrial roundwood uses plywood and veneer exports}
#' \item{Ind.RW.PlyandVen.Consump}{Industrial roundwood uses plywood and veneer consumption}
#' \item{Ind.RW.Pulp.Prod}{Industrial roundwood uses pulp-based products production}
#' \item{Ind.RW.Pulp.Imports}{Industrial roundwood uses pulp-based products imports}
#' \item{Ind.RW.Pulp.Exports}{Industrial roundwood uses pulp-based products exports}
#' \item{Ind.RW.Pulp.Consump}{Industrial roundwood uses pulp-based products consumption}
#' \item{Ind.RW.OtherIndustrial.ProdAndConsump}{Other industrial product production and consumption}
#' \item{Ind.RW.Logs.Imports}{Industrial roundwood uses log imports}
#' \item{Ind.RW.Logs.Exports}{Industrial roundwood uses log exports}
#' \item{Ind.RW.Pulpchip.Imports}{Industrial roundwood uses pulpwood chip imports}
#' \item{Ind.RW.Pulpchip.Exports}{Industrial roundwood uses pulpwood chip exports}
#' \item{FuelWood.ProdAndConsumption}{Fuelwood production and consumption}
#' }
#' @source USFS Estimations and Bureau of Census Data
"pietc_hwt2"



#' Particleboard and medium density fiberboard production, imports, exports, and consumption, 1950-87
#'
#' All units in millions square feet except when specified otherwise.
#'
#' @format A data frame with 38 observations of  7 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Prod.tot}{Production Total}
#' \item{Prod.Part.Board}{Production of Partical Board}
#' \item{Prod.Med.Fiberboard}{Medium Density Fiberboard}
#' \item{Imports}{Particleboard and medium density fiberboard imports}
#' \item{Exports}{Particleboard and medium density fiberboard exports}
#' \item{Consump.Tot}{Consumption Total}
#' \item{Consump.PerCapita}{Consumption Per Capita, square feet}
#' }
#' @source U.S. Department of Commerce, Bureau of the Census
"pietc_parbfb"


#' Insulating board production, imports, exports, and consumption, 1950-87
#'
#' All units in millions square feet except when specified otherwise
#'
#' @format A data frame with 63 observations of  6 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{InsulatingBoard.Prod}{Insulating Board Production}
#' \item{InsulatingBoard.Import}{Insulating Board Imports}
#' \item{InsulatingBoard.Exports}{Inulating Board Exports}
#' \item{InsulatingBoard.Consump.Tot}{Insulating Board consumption total}
#' \item{InsulatingBoard.Consump.PerCapita}{Insulating Board consumption per capita, in square feet}
#' }
#' @source U.S. Department of Commerce, Bureau of the Census
"pietc_ib"


#' Half lives for each end use.
#'
#' Half lives for each of the 13 end uses.
#'
#' @format A data frame with 151 observations of 13 variables:
#' \describe{
#' \item{House.SingFam}{Half lives for Single Family Homes}
#' \item{House.Multifam}{Half lives for Multi Family Homes}
#' \item{House.MobHom}{Half lives for Mobile Homes}
#' \item{Res.Upkeep}{Half lives for Total Residential Upkeep and Improvement}
#' \item{New.Nonres.AllRR}{Half lives for All Railroads}
#' \item{New.Nonres.Rties}{Half lives for Railroad Ties}
#' \item{New.Nonres.Rcar.Repair}{Half lives for Railcar Repair}
#' \item{Manu.HouseFurniture}{Half lives for Household Furniture}
#' \item{Manu.CommFurniture}{Half lives for Commercial Furniture}
#' \item{Manu.OtherProducts}{Half lives for Manufacturing and Other Products}
#' \item{Shipping.Tot}{Half lives for Shipping}
#' \item{Other.Uses.Tot}{Half lives for Other Uses}
#' \item{Other.Industrial.Tot}{Half lives for Uses For Other Industrial Products}
#' }
#' @source USFS data
"hl"



#' Consumption of Fibrous Materials in Paper and Board Manufacture in the United States
#' from 1900 to 1974.
#'
#' Dataset containing estimated and reported statistics. All units are
#' in tons, air dry weight.
#'
#' @format A data frame with 75 observations of 5 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Wood.Pulp.Consumption}{Consumption of Wood Pulp}
#' \item{Waste.Paper.Consumption}{Consumption of Waste Paper}
#' \item{Rags.Consumption}{Consumption of Rags}
#' \item{Other.Consumption}{Consumption of Other Fibrous Materials}
#' \item{Total.Consumption}{Total Consumption of Fibrous Materials}
#' }
#' @source Estimated using best available data and historical patterns. Some data
#' is original but no source in spreadsheet.
"c_pb"



#' Summary of Total Wood Pulp Statistics in the United States
#' from 1899 to 1972 with necessary statistics from 1869, 1879, 1889.
#'
#' New Supply is Production plus Imports minus Exports
#' All units are in tons of 2000 pounds, air dry weight.
#' All vectors are numeric.
#'
#' @format A data frame with 77 observations of 11 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Woodpulp.Prod}{Production of Wood Pulp}
#' \item{Woodpulp.Imports}{Imports of Wood Pulp}
#' \item{Woodpulp.Exports}{Exports of Wood Pulp}
#' \item{Woodpulp.NewSupply}{New Supply of Wood Pulp}
#' \item{Consump.Paper.Board}{Consumption in Paper & Board}
#' \item{WastePaper.Estimated.Prod}{Estimated Production of Waste Paper}
#' \item{WastePaper.Estimated.Imports}{Estimated Imports of Waste Paper}
#' \item{WastePaper.Estimated.Exports}{Estimated Exports of Waste Paper}
#' \item{Rags.Estimated.Prod}{Estimated Production of Rags and Other}
#' \item{Rags.Estimated.Imports}{Estimated Imports of Rags and Other}
#' \item{Rags.Estimated.Exports}{Estimated Exports of Rags and Other}
#' }
#' @source 1869 -  1962 U.  S.  Bureau of the Census
#' 1963 - Pulp Producers Division, A.  P.  I.
#' Imports,  Exports,  Consumption -  Bureau of the Census
"twp"



#' Lumber wood in single and multifamily houses build 1800 to 1900
#'
#' @format A vector with 151 observations of 1 variable:
#' \describe{
#' \item{Years}{Years}
#' \item{Carbon.Lumberwood}{Carbon from lumber wood in single and multifamily houses in od tons wood fiber}
#' }
#' @source 'ftp://ftp2.fs.fed.us/incoming/fpl/Skog/[Woodcarb II special lumber July 5 2007 Base Case.xls]SW Calc'!$V3878
"lp1900"



#' Fractions of Sawnwood production used in various end uses
#'
#' All units are percentages and all vectors are numeric.
#'
#' @format A data frame with 151 observations of 13 variables:
#' \describe{
#' \item{House.SingFam}{Fraction of sawnwood production in Single Family Homes}
#' \item{House.Multifam}{Fraction of sawnwood production in Multi Family Homes}
#' \item{House.MobHom}{Fraction of sawnwood production in Mobile Homes}
#' \item{Res.Upkeep}{Fraction of sawnwood production in Total Residential Upkeep and Improvement}
#' \item{New.Nonres.AllRR}{Fraction of sawnwood production in All Railroads}
#' \item{New.Nonres.Rties}{Fraction of sawnwood production in Railroad Ties}
#' \item{New.Nonres.Rcar.Repair}{Fraction of sawnwood production in Railcar Repair}
#' \item{Manu.HouseFurniture}{Fraction of sawnwood production in Household Furniture}
#' \item{Manu.CommFurniture}{Fraction of sawnwood production in Commercial Furniture}
#' \item{Manu.OtherProducts}{Fraction of sawnwood production in Manufacturing and Other Products}
#' \item{Shipping.Tot}{Fraction of sawnwood production in Shipping}
#' \item{Other.Uses.Tot}{Fraction of sawnwood production in Other Uses}
#' \item{Other.Industrial.Tot}{Fraction of sawnwood production in Uses For Other Industrial Products}
#' }
#' @source Check spreadsheet - values entered in
"fsw"



#' Fractions of Structural Panel production used in various end uses
#'
#' All units are percentages and all vectors are numeric.
#'
#' @format A data frame with 151 observations of 16 variables:
#' \describe{
#' \item{House.SingFam}{Fraction of structural panel production in Single Family Homes}
#' \item{House.Multifam}{Fraction of structural panel production in Multi Family Homes}
#' \item{House.MobHom}{Fraction of structural panel production in Mobile Homes}
#' \item{Res.Upkeep}{Fraction of structural panel production in Total Residential Upkeep and Improvement}
#' \item{New.Nonres.AllRR}{Fraction of structural panel production in All Railroads}
#' \item{New.Nonres.Rties}{Fraction of structural panel production in Railroad Ties}
#' \item{New.Nonres.Rcar.Repair}{Fraction of structural panel production in Railcar Repair}
#' \item{Manu.HouseFurniture}{Fraction of structural panel production in Household Furniture}
#' \item{Manu.CommFurniture}{Fraction of structural panel production in Commercial Furniture}
#' \item{Manu.OtherProducts}{Fraction of structural panel production in Manufacturing and Other Products}
#' \item{Shipping.Tot}{Fraction of structural panel production in Shipping}
#' \item{Other.Uses.Tot}{Fraction of structural panel production in Other Uses}
#' \item{Other.Industrial.Tot}{Fraction of structural panel production in Uses For Other Industrial Products}
#' }
#' @source Check spreadsheet - values entered in
"fsp_1"


#' Fractions of Non-Structural Panel production used in various end uses
#'
#' All units are percentages and all vectors are numeric.
#'
#' @format A data frame with 151 observations of 16 variables:
#' \describe{
#' \item{House.SingFam}{Fraction of non-structural panel production in Single Family Homes}
#' \item{House.Multifam}{Fraction of non-structural panel production in Multi Family Homes}
#' \item{House.MobHom}{Fraction of non-structural panel production in Mobile Homes}
#' \item{Res.Upkeep}{Fraction of non-structural panel production in Total Residential Upkeep and Improvement}
#' \item{New.Nonres.AllRR}{Fraction of non-structural panel production in All Railroads}
#' \item{New.Nonres.Rties}{Fraction of non-structural panel production in Railroad Ties}
#' \item{New.Nonres.Rcar.Repair}{Fraction of non-structural panel production in Railcar Repair}
#' \item{Manu.HouseFurniture}{Fraction of non-structural panel production in Household Furniture}
#' \item{Manu.CommFurniture}{Fraction of non-structural panel production in Commercial Furniture}
#' \item{Manu.OtherProducts}{Fraction of non-structural panel production in Manufacturing and Other Products}
#' \item{Shipping.Tot}{Fraction of non-structural panel production in Shipping}
#' \item{Other.Uses.Tot}{Fraction of non-structural panel production in Other Uses}
#' \item{Other.Industrial.Tot}{Fraction of non-structural panel production in Uses For Other Industrial Products}
#' }
#' @source Check spreadsheet - values entered in
"fnonsp"



#' Fraction of wood lost when placed in use from various end uses.
#'
#' All units are percentages and all vectors are numeric.
#'
#' @format A data frame with 151 observations of 16 variables:
#' \describe{
#' \item{House.SingFam}{Fraction of wood lost in single family homes}
#' \item{House.Multifam}{Fraction of wood lost in multi family homes}
#' \item{House.MobHom}{Fraction of wood lost in mobile homes}
#' \item{Res.Upkeep}{Fraction of wood lost in total residential upkeep and improvement}
#' \item{New.Nonres.AllRR}{Fraction of wood lost in all railroads}
#' \item{New.Nonres.Rties}{Fraction of wood lost in railroad ties}
#' \item{New.Nonres.Rcar.Repair}{Fraction of wood lost in railcar repair}
#' \item{Manu.HouseFurniture}{Fraction of wood lost in household furniture}
#' \item{Manu.CommFurniture}{Fraction of wood lost in commercial furniture}
#' \item{Manu.OtherProducts}{Fraction of wood lost in manufacturing and other products}
#' \item{Shipping.Tot}{Fraction of wood lost in shipping}
#' \item{Other.Uses.Tot}{Fraction of wood lost in other uses}
#' \item{Other.Industrial.Tot}{Fraction of wood lost in other industrial products}
#' }
#' @source Check spreadsheet - values entered in
"liu"



#' Percent of paper that goes to landfills.
#'
#' Units are percentages and the vector is numeric.
#'
#' @format A data frame with 151 observations of 1 variable:
#' \describe{
#' \item{Years}{Years}
#' \item{plf_percent}{Percent of paper that goes to landfills}
#' }
#' @source Linked site, check spreadsheet
"plf_percent"



#' Recovered Fibre Pulp in the United States from 1998 to 2013
#'
#'
#' All units are in metric tons and all vectors are numeric.
#'
#' @format A data frame with 16 observations of 3 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Prod.Quantity}{Production Quantity of Recovered Fibre Pulp}
#' \item{Imports.Quantity}{Imports Quantity of Recovered Fibre Pulp}
#' \item{Exports.Quantity}{Exports Quantity of Recovered Fibre Pulp}
#' }
#'
#' @source from AF&PA Paper, paperboard & wood pulp, A Monthly statistical summary
"usafp"



#' Percent of wood to dumps
#'
#' Gives percentage of all wood that goes to dumps.
#'
#' @format A data frome with 151 observations of 1 variable:
#'
#' \describe{
#' \item{Years}{Years}
#' \item{wd_percent}{Percentage of wood that goes to dumps}
#' }
#'
#' @source Linked spreadsheet (check this)
"wd_percent"



#' Percentage of wood to landfills.
#' Corresponds to colum U20 in Param + Results Sheet.
#' Gives percentage of all wood that goes to landfills.
#'
#' @format A data frame with 151 observations of 1 variable:
#' \describe{
#' \item{Years}{Years}
#' \item{wlf_percent}{Percentage of wood going to landfills}
#' }
#' @source Linked spreadsheet (check this)
"wlf_percent"



#' Total production, productivity, recovered paper utilization, and per capita production
#'
#' Dataset containing industrial wood and roundwood statistics,
#' productivity, recovered paper utilization rate and per capita
#' indudustrial wood production.
#'
#' @format A data frame with 151 observations of 15 variables:
#' \describe{
#' \item{Years}{Years}
#' \item{Total.Industrial.Wood.Product.Production}{Industrial wood production in thousand tons}
#' \item{Roundwood.Equivalents.of.Production.Hardwoods}{Hardwood production in million ft<sup>3</sup>}
#' \item{Roundwood.Equivalents.of.Production.Softwoods}{Softwood production in million ft<sup>3</sup>}
#' \item{Roundwood.Equivalents.of.Production.Totals.millionftcubed}{Production totals in million ft<sup>3</sup>}
#' \item{Roundwood.Equivalents.of.Production.Totals.thousand.short.tons}{Production totals in thousand short tons}
#' \item{Roundwood.Equivalents.of.Production.Totals.thousand.metric.tons}{Production totals in thousand metric tons}
#' \item{Industrial.Wood.Productivity.lbs.ftsquared}{Industrial wood product output per unit of roundwood input in (lbs/ft3)}
#' \item{Industrial.Wood.Productivity}{Industrial wood product output per unit of roundwood input in tons/tons}
#' \item{Recovered.Paper.Utilization.Rate(AF&PA)}{Recovered paper utilization rate}
#' \item{U.S.Population}{United States population in millions}
#' \item{Per.Capita.Industrial.Wood.Product.Production}{Per capita industrial wood product production, lbs/capita}
#' \item{HW.Agrifiber}{Hardwood Agrifiber (units?)}
#' \item{HW.Roundwood}{Hardwood Roundwood (units?)}
#' \item{U.S.Timber.Harvest}{United States Timber Harvest}
#' \item{Misc}{This is unnamed column in Ince_Table3}
#' }
#' @source Ince tables
"pt_pu"











