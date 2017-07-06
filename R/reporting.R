#' Sectoral background data for land-use, land-use change and forestry.
#' Computes IPCC tables for reporting.
#' Corresponds to Table 4.G s1 and Table 4.G s2
#'
#' @param year years to calculate values for.
#' @param table table to calculate
#' If table is s1 then returns table corresponding to HWP in use from domestic harvest.
#' If table is s2 then returns HWP activity data which includes data for different types of
#' wood and paper products.
#' @return HWP Activity Data table that includes relevant data for reporting.
#' @export
#'
#' @examples
#' reportTable4G(table = "s2")
reportTable4G <- function(year = 2013, table=c("all", "s1", "s2")){
    
    #read in necessary data
    usa <- calculateswpdata(year)
    usapaper <- calcUSApaper(year)
    usadumps <- calculatedumpcarbonproduction(year)

    table = match.arg(table)
    if (table == "s1") {
        s22 <- data.frame(Years=year)
        s22$`Sawnwood_Production` <- usa$`Sawnwood Production`*PRO17*10e6
        s22$`Sawnwood_Imports` <- usa$`Sawnwood Imports`*PRO17*10e6
        s22$`Sawnwood_Exports` <- usa$`Sawnwood Exports`*PRO17*10e6

        s22$Woodpanels_Production <- usa$`Wood Panel Production` * PRO17 * 10e6
        s22$Woodpanels_Imports <- usa$`Wood Panel Imports` * PRO17 * 10e6
        s22$Woodpanels_Exports <- usa$`Wood Panel Exports` * PRO17 * 10e6

        s22$PaperandPaperboard_Production <- usapaper$`Paper+Paperboard Production` * PRO18 * 10e6
        s22$PaperandPaperboard_Imports <- usapaper$`Paper+Paperbaord Imports` * PRO18 * 10e6
        s22$PaperandPaperboard_Exports <- usapaper$`Paper+Paperboard Exports` * PRO18 * 10e6

        s22$OtherIndustrialRoundwood_Production <- usa$`Other Products Production` * PRO17 * 10e6
        s22$LogandChip_Imports <- usa$Log_Imports * PRO17 * 10e6
        s22$LogandChip_Exports <- usa$`Log Exports (short tons)` * PRO17 * 10e6

        s22$FiberforPulp_Imports <- usa$TotalFibreFinish_Imports * PRO18 * 10e6
        s22$FiberforPulp_Exports <- usa$TotalFibreFinish_Exports * PRO18 * 10e6
        return(s22)
    }

    #Calculating table s2 
    if (table == "s2")
    {
      TotalHWP_DomesticHarvest <- data.frame(Years = year)
      GHGsourceAndSinkCategories <- data.frame(Years = c(year, year))
      
      totalSWhwpgain <- -1*(usadumps$BX + swp_carbon_stockchange(year)) * 1000
      totalSWhwploss <- usadumps$BX*1000
      TotalHWP_DomesticHarvest$Gains <- totalSWhwpgain
      TotalHWP_DomesticHarvest$Losses <- totalSWhwploss
      rownames(TotalHWP_DomesticHarvest) <- c("Solidwood from domestic harvest")
      
      hwpSWDSgain <- -1 * (usadumps$CH + usadumps$Dumps_N)*1000
      paperSWDSgain <- -1 * (usadumps$CI + usadumps$Dumps_S) * 1000
      hwpSWDSloss <- (PRM50 * usadumps$CN + PRM60 * usadumps$Dumps_O) * 1000
      paperSWDSloss <- (PRM51 * usadumps$CV + PRM61 * usadumps$Dumps_T)*1000
      
      
      GHGsourceAndSinkCategories$Gains <- c(hwpSWDSgain, paperSWDSgain)
      GHGsourceAndSinkCategories$Losses <- c(hwpSWDSloss, paperSWDSloss)
      GHGsourceAndSinkCategories$HalfLives <- c(PRP50, PRP51)
      rownames(GHGsourceAndSinkCategories) <- c("Solidwood in SWDS", "Paper in SWDS")
      
      return(list(TotalHWP_DomesticHarvest, GHGsourceAndSinkCategories))
    }
    
    if (table == "all")
    {
        s1 = reportTable4G(table = "s1") 
        s2 = reportTable4G(table = "s2") 
        return(list(s1, s2))
    }
}
