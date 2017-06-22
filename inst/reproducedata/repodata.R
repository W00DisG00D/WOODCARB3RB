library(devtools)
library(repmis)
library(xlsx)

#convertData <- read.xlsx("./inst/extdata/CopyOfData/conversionData.xlsx", 1, header = TRUE)
#rownames(convertData) <- c("Assumptions", "Moisture Content", "Non-wood Content")
#use_data(convertData, overwrite = TRUE)

piet_all <- read.xlsx("./inst/extdata/CopyOfData/piet_all.xlsx",1,header=FALSE,
                       startRow=15,endRow=72,colIndex=1:24)
colnames(piet_all) <- c("Years", "Prod.Tot",
                        "Prod.SW",
                        "Prod.HW",
                        "Imports.Tot", "Imports.SW", "Imports.HW", "Imports.Mixed",
                        "Imports.Mixed.PercOfTot",
                        "Imports.SW.PercOfTot", "Imports.HW.PercOfTot", "Imports.Estimated.SW",
                        "Imports.Estimated.HW", "Exports.Tot", "Exports.SW",
                        "Exports.HW", "Exports.Mixed", "Exports.Mixed.PercOfTot",
                        "Exports.SW.PercOfTot", "Exports.HW.PercOfTot", "Exports.Estimated.SW",
                        "Exports.Estimated.HW",
                        "NewSupply","PerCapita")
use_data(piet_all, overwrite = TRUE)

piet_irw <- read.xlsx("./inst/extdata/CopyOfData/piet_irw.xlsx", 1, header = FALSE,
                      startRow=6)
colnames(piet_irw) <- c("Years","Dom.Prod.Tot", "ApparentConsumption", "IndRW.Dom.Prod",
                        "IndRW.Imports","IndRW.Prod.SW", "IndRW.Prod.HW", "IndRW.LogChipImports.SW",
                        "IndRW.LogChipExports.SW", "IndRW.LogChipImports.HW",
                        "IndRW.LogChipExports.HW", "IndRW.SW.PctOfProd", "IndRW.Exports.SW.PctOfProd",
                        "IndRW.Exports.HW.PctOfProd", "IndRW.Imports.SW.PctOfConsump",
                        "IndRW.Imports.HW.PctOfConsump", "IndRW.SW.Lumber.PctOfTotLumbProd",
                        "IndRW.DV.Exports.Log.PctOfProd", "IndRW.DV.Imports.Log.PctOfProd",
                        "IndRW.Estimated.Imports", "IndRW.Estimated.Exports", "IndRW.Adj.Imports",
                        "IndRW.Adj.Exports", "IndRW.Estimated.Imports(tons)",
                        "IndRW.Estimated.Exports(tons)", "IndRW.Adj.Imports(tons)",
                        "IndRW.Adj.Exports(tons)", "IndRW.ApparentConsumption.Total",
                        "IndRW.SawLogs.Dom.Prod", "IndRW.SawLogs.NetImports", "IndRW.SawLogs.ApparentConsumption",
                        "IndRW.VeneerLogs.Dom.Prod", "IndRW.VeneerLogs.NetImports", "IndRW.VeneerLogs.ApparentConsumption",
                        "IndRW.PulpWood.Dom.Prod", "IndRW.PulpWood.NetImports", "IndRW.PulpWood.ApparentConsumption",
                        "IndRW.Other.ApparentConsumption", "FuelWood.ApparentConsumption")
colnames(piet_irw) <- gsub("IndRW.", "", colnames(piet_irw))
use_data(piet_irw, overwrite = TRUE)

ie_pw <- read.xlsx("./inst/extdata/CopyOfData/ie_pw.xlsx",1,header=FALSE,
                         startRow = 6)
colnames(ie_pw) <- c("Years", "Imports.Tot", "SW.Imports",
                           "HW.Imports.Tot", "HW.Imports.Birch", "HW.Imports.Other",
                           "Exports.Tot", "SW.Exports", "HW.Exports")
use_data(ie_pw, overwrite = TRUE)

ie_veneer <- read.xlsx("./inst/extdata/CopyOfData/ie_veneer.xlsx",1,header=FALSE,
                         startRow = 5,endRow = 41)
colnames(ie_veneer) <- c("Years", "Imports.Tot", "Imports.BirchMaple", "Imports.Other",
                           "Exports.Tot", "Exports.Fancy.Face.Figured.Special",
                           "Exports.UtilityCommercialContainer")
use_data(ie_veneer, overwrite = TRUE)

pietc_l2 <- read.xlsx("./inst/extdata/CopyOfData/pietc_l2.xls",1,header=FALSE,
                      startRow = 10)
colnames(pietc_l2) <- c("Years", "Prod.Tot", "Prod.SW","Prod.HW",
                        "Imports.Tot", "Imports.SW", "Imports.HW",
                        "Exports.Tot", "Exports.SW", "Exports.HW",
                        "Consump.Tot", "Consump.SW", "Consump.HW",
                        "PerCapConsump.Tot", "PerCapConsump.SW", "PerCapConsump.HW")
use_data(pietc_l2, overwrite = TRUE)

pietc_pw2 <- read.xlsx("./inst/extdata/CopyOfData/pietc_pw2.xlsx",1,header=FALSE,
                      startRow = 10)
colnames(pietc_pw2) <- c("Years","Prod.Tot", "Prod.SW","Prod.HW",
                        "Imports.Tot", "Imports.SW", "Imports.HW",
                        "Exports.Tot", "Exports.SW", "Exports.HW",
                        "Consump.Tot", "Consump.SW", "Consump.HW",
                        "PerCapConsump.Tot", "PerCapConsump.SW", "PerCapConsump.HW")
use_data(pietc_pw2, overwrite = TRUE)

pietc_sp <- read.xlsx("./inst/extdata/CopyOfData/pietc_sp.xlsx",1,header=FALSE,
                      startRow = 8)
colnames(pietc_sp) <- c("Years", "Prod.Tot", "Prod.SWPlywood","Prod.OSP",
                        "Imports.Tot", "Imports.SWPlywood", "Imports.OSP",
                        "Exports.Tot", "Exports.SWPlywood", "Exports.OSP",
                        "Consump.Tot", "Consump.SWPlywood", "Consump.OSP")
use_data(pietc_sp, overwrite = TRUE)

pietc_pbfm <- read.xlsx("./inst/extdata/CopyOfData/pietc_pbfm.xlsx",1,header=FALSE,
                      startRow = 8)
colnames(pietc_pbfm) <- c("Years", "Prod.PaperBoard",
                        "Consump.Tot", "Consump.WoodPulp", "Consump.RecPaper", "Consump.Other",
                        "Consump.PerTonProd.Tot","Consump.PerTonProd.WoodPulp", "Consump.PerTonProd.RecPaper", "Consump.PerTonProd.Other",
                        "RagsOther.RecPaperUtiRatePerc",
                        "RagsOther.Prod.Estimated", "RagsOther.Imports.Estimated", "RagsOther.Exports.Estimated")
use_data(pietc_pbfm, overwrite = TRUE)

pietc_pbrp <- read.xlsx("./inst/extdata/CopyOfData/pietc_pbrp.xlsx",1,header=FALSE,
                      startRow = 9)
colnames(pietc_pbrp) <- c("Years", "PaperBoardNewSupply",
                        "RecPap.ConsumedatPaperBoardMills", "RecPap.MoldedPulpInsulationandOther",
                        "RecPap.Exports", "RecPap.Imports", "RecPap.TotRecovered",
                        "RecPap.RecoveryRate", "NoName", "Ratio.ExportstoProduction",
                        "Ratio.ImportstoProduction")
use_data(pietc_pbrp, overwrite = TRUE)

pietc_wp2 <- read.xlsx("./inst/extdata/CopyOfData/pietc_wp2.xlsx",1,header=FALSE,
                      startRow = 8)
colnames(pietc_wp2) <- c("Years", paste("Woodpulp.",c("Production",
                        "Imports", "Imports.PercConsump",
                        "Exports", "Exports.PercProd",
                        "Consump.Tot", "Consump.PerCap(pounds)"), sep=""))
use_data(pietc_wp2, overwrite = TRUE)

pietc_t2 <- read.xlsx("./inst/extdata/CopyOfData/pietc_t2.xlsx",1,header=FALSE,
                     startRow = 10)
colnames(pietc_t2) <- c("Years", "AllProducts.Prod", "AllProducts.Consump",
                       "Prod.Tot", "Imports.Tot", "Exports.Tot", "Consump.Tot",
                       "Lumber.Prod", "Lumber.Imports", "Lumber.Exports", "Lumber.Consump",
                       "PlywoodVeneer.Prod", "PlywoodVeneer.Imports", "PlywoodVeneer.Exports", "PlywoodVeneer.Consump",
                       "Pulpwood.Prod", "Pulpwood.Imports", "Pulpwood.Exports", "Pulpwood.Consump",
                       "OtherIndProductsandConsump",
                       "Logs.Imports", "Logs.Exports",
                       "PulpwoodChip.Imports", "PulpwoodChip.Exports",
                       "FuelwoodProdandConsump")
use_data(pietc_t2, overwrite = TRUE)

pietc_parbfb2 <- read.xlsx("./inst/extdata/CopyOfData/pietc_parbfb2.xlsx",1,header=FALSE,
                      startRow=10)
colnames(pietc_parbfb2) <- c("Years", paste("Particle_MDF.", c("Prod.Tot", "Prod.ParticleBoard", "Prod.MediumDensityFiberboard",
                        "Imports", "Exports",
                        "Consump.Tot", "Consump.PerCap(Sq ft)"), sep=""))
use_data(pietc_parbfb2, overwrite = TRUE)

pietc_l <- read.xlsx("./inst/extdata/CopyOfData/pietc_l.xlsx",1,header=FALSE,
                      startRow = 11, endRow = 39, colIndex = c(1:4, 6:8, 10:12, 14:16, 18:20))
colnames(pietc_l) <- c("Years", "Prod.Tot", "Prod.SW", "Prod.HW",
                        "Imports.Tot", "Imports.SW", "Imports.HW",
                        "Exports.Tot", "Exports.SW", "Exports.HW",
                        "Consump.Tot", "Consump.SW", "Consump.HW",
                        "PerCapitaConsump.Tot", "PerCapitaConsump.SW", "PerCapitaConsump.HW")
use_data(pietc_l, overwrite = TRUE)

pietc_pw <- read.xlsx("./inst/extdata/CopyOfData/pietc_pw.xlsx",1,header=FALSE,
                      startRow=11, endRow = 49, colIndex = c(1:4, 6:8, 10:12, 14:16, 18:20))
colnames(pietc_pw) <- c("Years", "Prod.Tot", "Prod.SW", "Prod.HW",
                        "Imports.Tot", "Imports.SW", "Imports.HW",
                        "Exports.Tot", "Exports.SW", "Exports.HW",
                        "Consump.Tot", "Consump.SW", "Consump.HW",
                        "PerCapitaConsump.Tot", "PerCapitaConsump.SW", "PerCapitaConsump.HW")
use_data(pietc_pw, overwrite = TRUE)

pietc_t <- read.xlsx("./inst/extdata/CopyOfData/pietc_t.xlsx",1,header=FALSE,
                     startRow=11, endRow = 49, colIndex = c(1:3, 5:8, 10:13, 15:18, 20:23, 25:30))
colnames(pietc_t) <- c("Years", "AllProducts.Prod", "AllProducts.Consump",
                       "Tot.Prod", "Tot.Imports", "Tot.Exports", "Tot.Consump",
                       "Lumber.Prod", "Lumber.Imports", "Lumber.Exports", "Lumber.Consump",
                       "PlywoodVeneer.Prod", "PlywoodVeneer.Imports", "PlywoodVeneer.Exports", "PlywoodVeneer.Consump",
                       "PulpProducts.Prod", "PulpProducts.Imports", "PulpProducts.Exports", "PulpProducts.Consump",
                       "OtherIndProducts.ProdandConsump", "Logs.Imports", "Logs.Exports", "PulpwoodChip.Exports",
                       "Fuelwood.ProdandConsump", "LogChipExports.PercofProduction")
use_data(pietc_t, overwrite = TRUE)

pietc_swt <- read.xlsx("./inst/extdata/CopyOfData/pietc_swt.xlsx",1,header=FALSE,
                     startRow=11, endRow = 49, colIndex = c(1:3, 5:8, 10:13, 15:18, 20:29))
colnames(pietc_swt) <- c("Years","AllProducts.Prod", "AllProducts.Consump",
                       "Tot.Prod", "Tot.Imports", "Tot.Exports", "Tot.Consump",
                       "Lumber.Prod", "Lumber.Imports", "Lumber.Exports", "Lumber.Consump",
                       "PlywoodVeneer.Prod", "PlywoodVeneer.Imports", "PlywoodVeneer.Exports", "PlywoodVeneer.Consump",
                       "PulpProducts.Prod", "PulpProducts.Imports", "PulpProducts.Exports", "PulpProducts.Consump",
                       "OtherIndProducts.ProdandConsump", "Logs.Imports", "Logs.Exports", "PulpwoodChip.Exports",
                       "Fuelwood.ProdandConsump", "LogChipExports.PercofProduction")
use_data(pietc_swt, overwrite = TRUE)

######LOSS when placed IU based on EU and yr
pietc_liu <- read.xlsx("./inst/extdata/CopyOfData/lossWhenPlacedIU.xlsx",1,header=FALSE,
                    colIndex = c(1,2,3,5,6,7,8,10,11,12,14,15,16,17)+1,
                    startRow = 9)
colnames(pietc_liu)<-c("House.SingFam","House.Multifam","House.MobHom",
                    "Res.Upkeep","New.Nonres.AllRR","New.Nonres.Rties","New.Nonres.Rcar.Repair",
                    "Manu.HouseFurniture","Manu.CommFurniture","Manu.OtherProducts",
                    "Shipping.Tot","Other.Uses.Tot", "Other.Industrial.Tot")
#pietc_liu <- pietc_liu[,1]
use_data(pietc_liu, overwrite = TRUE)

lp1900 <- read.xlsx("./inst/extdata/CopyOfData/lp1900.xlsx",1,header=FALSE,
                           startRow = 2)
colnames(lp1900)<-c("Years", "Carbon.Lumberwood")
use_data(lp1900, overwrite = TRUE)

wlf_percent <- read.xlsx("./inst/extdata/CopyOfData/wlf_percent.xlsx", 1, header=F,
                             startRow=2)
colnames(wlf_percent) <-c("Years", "wlf_percent")
use_data(wlf_percent, overwrite = TRUE)

wd_percent <- read.xlsx("./inst/extdata/CopyOfData/wd_percent.xlsx", 1, header=F,
                         startRow=2)
colnames(wd_percent) <-c("Years", "wd_percent")
use_data(wd_percent, overwrite = TRUE)

plf_percent <- read.xlsx("./inst/extdata/CopyOfData/plf_percent.xlsx",1,header=F,
                              startRow=2)
colnames(plf_percent) <-c("Years", "plf_percent")
use_data(plf_percent, overwrite = TRUE)
########
#imports1file<-read.xlsx("./inst/extdata/CopyOfData/imports1.xlsx",1,header=FALSE)
#colnames(imports1file) <-c("SW.PLY","OSB.Waferboard","HW.PLY.veneer",
                           #"SW.lumber","HW.lumber","Partical.board","Hardboard","MDF","PPandBoard",
                           #"insulatingboard", "","year","HardPly","Partboard","hardboard","insulboard")
#use_data(imports1file, overwrite = TRUE)

########halflives for various end uses for yrs 1900-2050
hl <- read.xlsx("./inst/extdata/CopyOfData/hl.xlsx", 1,header=FALSE,
                       startRow = 11,colIndex = c(1:3, 5:8,10:12,14:16)+1 )
colnames(hl)<-c("House.SingFam","House.Multifam","House.MobHom",
                       "Res.Upkeep","New.Nonres.AllRR","New.Nonres.Rties","New.Nonres.Rcar.Repair",
                       "Manu.HouseFurniture","Manu.CommFurniture","Manu.OtherProducts",
                       "Shipping.Tot","Other.Uses.Tot", "Other.Industrial.Tot")
use_data(hl, overwrite = TRUE)

########PAPER
IncePaper <- read.xlsx("./inst/extdata/CopyOfData/Ince_Paper.xlsx",1,header=FALSE,
                       startRow = 9)
colnames(IncePaper) <- c("Years", "Paper.Board.Prod","Paper.Board.Imports","Paper.Board.ApparentConsumption",
                         "Population", "Paper.Board.ConsumptionPerCapita")
use_data(IncePaper, overwrite = TRUE)

c_pb <- read.xlsx("./inst/extdata/CopyOfData/c_pb.xlsx",1,header=FALSE,
                          startRow = 6, colIndex = c(1,2,4,6,8,10))
colnames(c_pb) <- c("Years", "Wood.Pulp.Consumption","Waste.Paper.Consumption", "Rags.Consumption",
                            "Other.Consumption","Total.Consumption")
use_data(c_pb, overwrite = TRUE)

twp <- read.xlsx("./inst/extdata/CopyOfData/twp.xlsx",1,header=FALSE,
                              startRow = 8, endRow = 84, colIndex = c(1:6, 8:10, 12:14))
colnames(twp)<-c("Years", "Woodpulp.Prod","Woodpulp.Imports","Woodpulp.Exports",
                              "Woodpulp.NewSupply", "Consump.Paper.Board",
                              "WastePaper.Estimated.Prod","WastePaper.Estimated.Imports",
                              "WastePaper.Estimated.Exports",
                              "Rags.Estimated.Prod",
                              "Rags.Estimated.Imports",
                              "Rags.Estimated.Exports")
use_data(twp, overwrite = TRUE)
#####################
usafp <- read.xlsx("./inst/extdata/CopyOfData/usafp.xlsx",1,header=FALSE,
                          startRow = 6, colIndex=c(2:5))
colnames(usafp)<-c("Years", "Prod.Quantity","Imports.Quantity","Exports.Quantity")
use_data(usafp, overwrite = TRUE)

p_wfw <- read.xlsx("./inst/extdata/CopyOfData/p_wfw.xlsx",1,header=FALSE,
                   startRow = 4,endRow=154)
colnames(p_wfw)<-c("Years", "SW.Ply","OSB.Wafer.board","Lam.Ven.Lumb", "HW.Ply.Ven",
                   "SW.Lumb","HW.Lumb","Lumb.Pallet.Plant",
                   "PartBoard.Prod","Hardboard.Prod",
                   "MDF.Prod","Pulp.Paper.Board","Other.Products",
                   "InsulatingBoard","Fuelwood","Fuelwood.Tons","Log.Chip.Exports",
                   "RW.Dom.Prod","Estimated.Tot.Dom.Timber")
use_data(p_wfw, overwrite = TRUE)


pietc_hw2 <- read.xlsx("./inst/extdata/CopyOfData/pietc_hw2.xlsx",1,header=FALSE,
                      startRow = 7)
colnames(pietc_hw2) <- c("Years", paste("Insulboard.", c("Production","Imports","Exports",
                      "Total.Consumption","PerCapita.Consumption"), sep = ""))
use_data(pietc_hw2, overwrite=TRUE)

pietc_ib2 <- read.xlsx("./inst/extdata/CopyOfData/pietc_ib2.xlsx",1,header=FALSE,
                      startRow = 9)
colnames(pietc_ib2)<- c("Years", paste("Hardboard.", c("Production","Imports","Exports",
                            "Total.Consumption","PerCapita.Consumption"), sep=""))
use_data(pietc_ib2, overwrite=TRUE)

pietc_swt2 <- read.xlsx("./inst/extdata/CopyOfData/pietc_swt2.xlsx",1,header=FALSE,
                startRow = 10, endRow = 65, colIndex = 1:26)

#pietc_swt2[is.na(pietc_swt2)] <- 0
colnames(pietc_swt2) <- c("Years", "AllProduction.Prod","AllProduct.Consump",
                     "Ind.RW.Tot.Prod","Ind.RW.Tot.Imports","Ind.RW.Tot.Exports","Ind.RW.Tot.Consump",
                     "Ind.RW.Lum.Prod","Ind.RW.Lum.Imports","Ind.RW.Lum.Exports","Ind.RW.Lum.Consump",
                     "Ind.RW.PlyandVen.Prod","Ind.RW.PlyandVen.Imports","Ind.RW.PlyandVen.Exports","Ind.RW.PlyandVen.Consump",
                     "Ind.RW.Pulp.Prod","Ind.RW.Pulp.Imports","Ind.RW.Pulp.Exports","Ind.RW.Pulp.Consump",
                     "Ind.RW.OtherIndustrial.ProdAndConsump",
                     "Ind.RW.Logs.Imports","Ind.RW.Logs.Exports",
                     "Ind.RW.Pulpchip.Imports","Ind.RW.Pulpchip.Exports",
                     "FuelWood.ProdAndConsumption")
use_data(pietc_swt2, overwrite=TRUE)

pietc_hwt2<-read.xlsx("./inst/extdata/CopyOfData/pietc_hwt2.xlsx",1,header=FALSE,
                   startRow = 10, colIndex = 1:25)
pietc_hwt2[is.na(pietc_hwt2)] <- 0
colnames(pietc_hwt2)<-c("Years", "AllProduction.Prod","AllProduct.Consump",
                      "Ind.RW.Tot.Prod","Ind.RW.Tot.Imports","Ind.RW.Tot.Exports","Ind.RW.Tot.Consump",
                      "Ind.RW.Lum.Prod","Ind.RW.Lum.Imports","Ind.RW.Lum.Exports","Ind.RW.Lum.Consump",
                      "Ind.RW.PlyandVen.Prod","Ind.RW.PlyandVen.Imports","Ind.RW.PlyandVen.Exports","Ind.RW.PlyandVen.Consump",
                      "Ind.RW.Pulp.Prod","Ind.RW.Pulp.Imports","Ind.RW.Pulp.Exports","Ind.RW.Pulp.Consump",
                      "Ind.RW.OtherIndustrial.ProdAndConsump",
                      "Ind.RW.Logs.Imports","Ind.RW.Logs.Exports",
                      "Ind.RW.Pulpchip.Imports","Ind.RW.Pulpchip.Exports",
                      "FuelWood.ProdAndConsumption")
use_data(pietc_hwt2, overwrite=TRUE)

pietc_parbfb<-read.xlsx("./inst/extdata/CopyOfData/pietc_parbfb.xlsx",1,header=FALSE,
                    startRow = 11, endRow = 48)
colnames(pietc_parbfb) <- c("Years", "Prod.tot","Prod.Part.Board","Prod.Med.Fiberboard",
                      "Imports","Exports","Consump.Tot","Consump.PerCapita")
use_data(pietc_parbfb, overwrite=TRUE)


pietc_ib<-read.xlsx("./inst/extdata/CopyOfData/pietc_ib.xlsx",1,header=FALSE,
                    startRow = 8, endRow = 70)
colnames(pietc_ib) <- c("Years", "InsulatingBoard.Prod","InsulatingBoard.Import",
                      "InsulatingBoard.Exports",
                      "InsulatingBoard.Consump.Tot",
                      "InsulatingBoard.Consump.PerCapita")
use_data(pietc_ib, overwrite=TRUE)

pietc_hb<-read.xlsx("./inst/extdata/CopyOfData/pietc_hb.xlsx",1,header=FALSE,
                    startRow = 7, endRow = 78)
colnames(pietc_hb)<-c("Years", "Hardboard.Prod","Hardboard.Import",
                      "Hardboard.Exports",
                      "Hardboard.Consump.Tot",
                      "Hardboard.Consump.PerCapita")
use_data(pietc_hb, overwrite=TRUE)

pietc_hwt <- read.xlsx("./inst/extdata/CopyOfData/pietc_hwt.xlsx",1,header=FALSE,
                   startRow = 11, endRow = 48, colIndex = c(1:3, 5:8, 10:13, 15:18, 20:24, 26:30))
colnames(pietc_hwt)<-c("Years", "AllProduction.Prod","AllProduct.Consump",
                     "Ind.RW.Tot.Prod","Ind.RW.Tot.Imports","Ind.RW.Tot.Exports","Ind.RW.Tot.Consump",
                     "Ind.RW.Lum.Prod","Ind.RW.Lum.Imports","Ind.RW.Lum.Exports","Ind.RW.Lum.Consump",
                     "Ind.RW.PlyandVen.Prod","Ind.RW.PlyandVen.Imports","Ind.RW.PlyandVen.Exports","Ind.RW.PlyandVen.Consump",
                     "Ind.RW.Pulp.Prod","Ind.RW.Pulp.Imports","Ind.RW.Pulp.Exports","Ind.RW.Pulp.Consump",
                     "Ind.RW.OtherIndustrial.ProdAndConsump",

                     "Ind.RW.Logs.Imports","Ind.RW.Logs.Exports",
                     "FuelWood.ProdAndConsumption","UnNamed1")
use_data(pietc_hwt, overwrite=TRUE)


fsw<-read.xlsx("./inst/extdata/CopyOfData/fsw.xlsx",1,header=FALSE,
                        startRow = 12, colIndex = 1:17)
fsw <- fsw[-c(1, 5, 10, 14)]
colnames(fsw)<-c("House.SingFam","House.Multifam","House.MobHom",
                          "Res.Upkeep","New.Nonres.AllRR","New.Nonres.Rties","New.Nonres.Rcar.Repair",
                          "Manu.HouseFurniture","Manu.CommFurniture","Manu.OtherProducts",
                          "Shipping.Tot","Other.Uses.Tot", "Other.Industrial.Tot")
#fsw[is.na(fsw)] <- 0

use_data(fsw, overwrite=TRUE)


fnonsp <- read.xlsx("./inst/extdata/CopyOfData/fnonsp.xlsx", 1,
                              header = FALSE, startRow = 11, colIndex = 1:17)
fnonsp <- fnonsp[,2:17]
colnames(fnonsp)<-c("House.SingFam","House.Multifam","House.MobHom","House.Tot",
                              "Res.Upkeep","New.Nonres.AllRR","New.Nonres.Rties","New.Nonres.Rcar.Repair",
                              "New.Nonres.tot",
                              "Manu.HouseFurniture","Manu.CommFurniture","Manu.OtherProducts",
                              "Manu.Tot",
                              "Shipping.Tot","Other.Uses.Tot", "Other.Industrial.Tot")
fnonsp <- fnonsp[,-c(4, 9, 13)]
use_data(fnonsp, overwrite = TRUE)


fsp_1 <- read.xlsx("./inst/extdata/CopyOfData/fsp_1.xlsx", 1,
                           header = FALSE, startRow = 10, colIndex = 1:17)
fsp_1 <- fsp_1[,2:17]
colnames(fsp_1)<-c("House.SingFam","House.Multifam","House.MobHom","House.Tot",
                           "Res.Upkeep","New.Nonres.AllRR","New.Nonres.Rties","New.Nonres.Rcar.Repair",
                           "New.Nonres.tot",
                           "Manu.HouseFurniture","Manu.CommFurniture","Manu.OtherProducts",
                           "Manu.Tot",
                           "Shipping.Tot","Other.Uses.Tot", "Other.Industrial.Tot")
fsp_1 <- fsp_1[,-c(4, 9, 13)]
use_data(fsp_1, overwrite = TRUE)

pt_pu <- read.xlsx("./inst/extdata/CopyOfData/pt_pu.xlsx", 1, header = FALSE, colClasses = "character")
pt_pu <- pt_pu[,1:16]
for(i in 1:ncol(pt_pu)){
  pt_pu[,i] <- as.character(pt_pu[,i])

}
pt_pu[(pt_pu == "N.A.") == T] <- NA
for(i in 1:ncol(pt_pu)){
  pt_pu[,i] <- suppressWarnings(as.numeric(pt_pu[,i]))

}
colnames(pt_pu) <- c("Years", "Total.Industrial.Wood.Product.Production", paste("Roundwood.Equivalents.of.Production.",c("Hardwoods", "Softwoods",paste("Totals",c(".millionftcubed", ".thousand.short.tons",".thousand.metric.tons"),sep="")),sep=""),
                          "Industrial.Wood.Productivity.lbs.ftsquared", "Industrial.Wood.Productivity",
                          "Recovered.Paper.Utilization.Rate(AF&PA)",
                          "U.S.Population","Per.Capita.Industrial.Wood.Product.Production",
                          "HW.Agrifiber", "HW.Roundwood",
                          "U.S.Timber.Harvest", "Misc")
use_data(pt_pu, overwrite = TRUE)
