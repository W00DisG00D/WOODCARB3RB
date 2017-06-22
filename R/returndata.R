##This removes "no visible binding" error from R CMD check
#solution from: http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
names <- c("piet_all","piet_irw","ie_pw",
           "ie_veneer","pietc_l2","pietc_pw2",
           "pietc_sp","pietc_pbfm","pietc_pbrp",
           "pietc_wp2","pietc_t2","pietc_parbfb2",
           "pietc_l","pietc_pw","pietc_t",
           "pietc_swt","liu","lp1900",
           "wlf_percent","wd_percent","plf_percent",
           "hl","IncePaper","c_pb",
           "twp","usafp","p_wfw",
           "pietc_hw2","pietc_ib2","pietc_swt2",
           "pietc_hwt2", "pietc_parbfb","pietc_ib",
           "pietc_hb", "pietc_hwt","fsw",
           "fnonsp", "fsp_1", "pt_pu",
           "woodToCarbon", "paperToCarbon", "swpSwdsNondegradable",
           "paperSwdsNondegradable", "paperLandfillDecay",
           "paperDumpDecay", "swpLandfillDecay", "swpDumpDecay",
           "paperHL")

for (i in seq(names)){
  utils::globalVariables(names[i])
}


#' Get import, export, production data for all product types.
#' Aggregates all intermediate calculations necessary for final calculations.
#' Returns data and intermediate calculations corresponding
#' to sheets in Woodcarb2. Includes relevant SWP and paper production and
#' trade time series for calculating final carbon contributions for
#' various approaches.
#'
#' @param years years to return data for
#'
#' @return data frame of intermediate calculations corresponding to those in
#' Woodcarb2.
#' @export
returnData<- function(years = 1950:2015){

    usa <- calculateswpdata(years)
    usapaper <- calcUSApaper(years)
    usadumps <- calculatedumpcarbonproduction(years)
    usadumps2 <- calculatedumpcarbonstockchange(years)
    return(Reduce(function(x, y) merge(x, y, all=TRUE), list(usa, usapaper, usadumps, usadumps2)))
}



###############################################################
##Functions used to access and return data for convenience in functions requiring
##several different formulas for one vector.
h8 <- function(y,c){
  return(piet_all[y-1898,c])
}

h3 <- function(y,c){
  return(piet_irw[y-1899,c])
}

h3t20 <- function(y,c){
  return(ie_pw[y-1926,c])
}

h3t21 <- function(y,c){
  return(ie_veneer[y-1926,c])
}

u4 <- function(y,c){
  return(pietc_t[y-1949,c])
}

u5 <- function(y,c){
  return(pietc_swt[y-1949,c])
}

u6 <- function(y,c){
  return(pietc_hwt[y-1949,c])
}

u29 <- function(y,c){
  return(pietc_l[y-1949,c])
}

u36 <- function(y,c){
  return(pietc_pw[y-1949,c])
}

u52 <- function(y,c){
  return(pietc_parbfb[y-1949,c])
}

u53 <- function(y,c){
  return(pietc_ib[y-1924,c])
}

u54 <- function(y,c){
  return(pietc_hb[y-1915,c])
}

h5 <- function(y,c){
  return(pietc_t2[y-1964,c])
}

h6 <- function(y,c){
  return(pietc_swt2[(y-1964),c])
}

h7 <- function(y,c){
  return(pietc_hwt2[y-1964,c])
}

h28 <- function(y,c){
  return(pietc_l2[y-1964,c])
}

h37 <- function(y,c){
  return(pietc_pw2[y-1964,c])
}

h38 <- function(y,c){
  return(pietc_sp[y-1979,c])
}

h46 <- function(y,c){
  return(pietc_pbfm[y-1964,c])
}

h47 <- function(y,c){
  return(pietc_pbrp[y-1964,c])
}

h49 <- function(y,c){
  return(pietc_wp2[y-1964,c])
}

h53 <- function(y,c){
  return(pietc_parbfb2[y-1964,c])
}

h55 <- function(y,c){
  return(pietc_hw2[y-1964,c])
}

h56 <- function(y,c){
  return(pietc_ib2[y-1964,c])
}

inc1 <- function(y,c){
  return(p_wfw[y-1899,c])
}

getIncePap <- function(y,c){
  return(IncePaper[y-1898,c])
}

apiFiber <- function(y,c){
  return(c_pb[y-1899,c])
}

apiTotal <- function(y,c){
  return(twp[y-1895,c])
}

FibPulp_USA<-function(y,c){
  return(usafp[y-1997,c])
}

Ince3 <- function(y,c){
  return(pt_pu[y-1899,c])
}


