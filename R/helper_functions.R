## Helper functions

#' list_op
#' @param l1 list object 1
#' @param l2 list_object 2
#' @param l3 list_object 3
#' @param func string describing elementwise operation
#' @return outcome of elementwise operation
#' @export
list_op <- function(l1, l2, l3 = NULL, func) {

  # THIS FUNCTION...

  if (func == "+") {
    res <- lapply(seq_along(l1), FUN=function(x) unlist(unname(l1[x])) + unlist(unname(l2[x])))
  } else if (func == "-") {
    res <- lapply(seq_along(l1), FUN=function(x) unlist(unname(l1[x])) - unlist(unname(l2[x])))
  } else if (func == "*") {
    res <- lapply(seq_along(l1), FUN=function(x) unlist(unname(l1[x])) * unlist(unname(l2[x])))
  } else if (func == "*0.5") {
    res <- lapply(seq_along(l1), FUN=function(x) 0.5 * unlist(unname(l1[x])) * unlist(unname(l2[x])))
  } else if (func == "max") {
    res <- lapply(seq_along(l1), FUN=function(x) apply(cbind(unlist(unname(l1[x])), unlist(unname(l2[x]))), MAR=1, FUN=max))
  } else if (func == "c") {
    res <- lapply(seq_along(l1), FUN=function(x) c(unlist(unname(l1[x])), unlist(unname(l2[x]))))
  } else if (func == "/") {
    res <- lapply(seq_along(l1), FUN=function(x) unlist(unname(l1[x])) / unlist(unname(l2[x])))
  }

  if (!is.null(l3)) {
    if (func == "+") {
      res <- lapply(seq_along(res), FUN=function(x) unlist(unname(res[x])) + unlist(unname(l3[x])))
    } else if (func == "-") {
      res <- lapply(seq_along(res), FUN=function(x) unlist(unname(res[x])) - unlist(unname(l3[x])))
    } else if (func == "*") {
      res <- lapply(seq_along(res), FUN=function(x) unlist(unname(res[x])) * unlist(unname(l3[x])))
    } else if (func == "*0.5") {
      res <- lapply(seq_along(res), FUN=function(x) unlist(unname(res[x])) * unlist(unname(l3[x])))
    } else if (func == "max") {
      res <- lapply(seq_along(res), FUN=function(x) apply(cbind(unlist(unname(res[x])), unlist(unname(l3[x]))), MAR=1, FUN=max))
    } else if (func == "c") {
      res <- lapply(seq_along(res), FUN=function(x) c(unlist(unname(res[x])), unlist(unname(l3[x]))))
    }
  }

  names(res) <- names(l1)

  return(res)
}

#' rest_dyn_mod flexible convergent function for modelling restoration dynmaics
#' @param t sequence of years for restoration phase
#' @param n shape parameter (user input)
#' @param ymin value of y at t=0 (drained rate)
#' @param ymax value of y at t=max(t) (restored rate)
#' @param convThresh proportion of ymax at which convergence assumed to occur (since y attains ymax at t=Inf)
#' @return rate sequence
#' @export
rest_dyn_mod <- function(t,n,ymin,ymax,convThresh=0.99) {

  # Bounded flexible function implemented in absence of mechanistic model for peatland restoration

  aa <- -log(1-convThresh)/(max(t)^n) # allows control over value of x at convergence point
  rate <- ymax + (ymin - ymax) * exp(-aa*(t^n))

  return(rate)
}

#' getDf1
#' @param list_ob output of intermediate calculations in nested list format (Area, Exp/Min/Max)
#' @return dataframe
#' @export
getDf1 <- function(list_ob) {
  list_ob_df <- bind_rows(lapply(seq_along(list_ob), FUN = function(x) {
    res <- lapply(seq_along(list_ob[[x]]), FUN = function(y) {
      df <- list_ob[[x]][[y]]
      df$source <- colnames(df)[2]
      colnames(df)[2] <- "value"
      df <- df[,c(1,3,2)]
      df$Area <- names(list_ob)[x]
      df$Est <- names(list_ob[[x]])[y]
      return(df)
    })
    return(res)
  }))

  list_ob_df <- list_ob_df[complete.cases(list_ob_df),]

  return(list_ob_df)
}

#' getDf2
#' @param list_ob output of intermediate calculations in nested list format (Area, Exp/Min/Max)
#' @return dataframe
#' @export
getDf2 <- function(list_ob) {
  list_ob_df <- bind_rows(lapply(seq_along(list_ob), FUN = function(x) {
    res <- lapply(seq_along(list_ob[[x]]), FUN = function(y) {
      df <- list_ob[[x]][[y]] %>%
        pivot_longer(cols = starts_with(c("L_")),
                     names_to = "source")
      df$Area <- names(list_ob)[x]
      df$Est <- names(list_ob[[x]])[y]
      return(df)
    })
    return(res)
  }))

  list_ob_df <- list_ob_df[complete.cases(list_ob_df),]

  return(list_ob_df)
}

#' getDf3
#' @param list_ob output of intermediate calculations in nested list format (Area, Exp/Min/Max)
#' @return dataframe
#' @export
getDf3 <- function(list_ob) {
  # For e.g. L_forest (multi-source output)
  list_ob_df <- lapply(seq_along(list_ob), FUN = function(x) {
    res <- lapply(seq_along(list_ob[[x]]), FUN = function(y) {
      res2 <- lapply(seq_along(list_ob[[x]][[y]]), FUN = function(z) {
        df <- as.data.frame(unname(list_ob[[x]][[y]][z])) %>%
          pivot_longer(cols = starts_with(c("L_", "S_")),
                       names_to = "source")
        df$Area <- names(list_ob)[x]
        df$Est <- names(list_ob[[x]][[y]])[z]
        return(df)
      })
      return(res2)
    })
    return(res)
  })

  list_ob_df <- bind_rows(unlist(list_ob_df, recursive = F))
  list_ob_df <- list_ob_df %>%
    mutate(value = ifelse(value==0, NA, value))
  list_ob_df <- list_ob_df[complete.cases(list_ob_df),]

  return(list_ob_df)
}

#' getCarbonDf
#' @param S_forest Forest sequestration
#' @param L_forest_soils Emissions from soils under forestry
#' @param L_AqC_forest_soils Aquatic carbon losses from forest soils
#' @param L_forest Forest product losses
#' @param L_peatland Emissions from peatland
#' @param L_AqC_peatland Aquatic carbon losses from oeatland
#' @return Total peatland restoration carbon accounting dataframe
#' @export
getCarbonDf <- function(S_forest,
                        L_forest_soils,
                        L_AqC_forest_soils,
                        L_forest,
                        L_peatland,
                        L_AqC_peatland) {

  # Get forest sequestration (units CO2)
  CO2_C <- 3.667 # Molecular weight ratio C to CO2
  S_forest_CO2 <- lapply(seq_along(S_forest), FUN = function(x) {
    res <- lapply(seq_along(S_forest[[x]]), FUN = function(y) {
      S <- S_forest[[x]][[y]]
      S <- S %>%
        select(t, NPP) %>%
        rename(S_forest = NPP) %>%
        mutate(S_forest = S_forest * CO2_C)
      return(S)
    })
    names(res) <- names(S_forest[[x]])
    return(res)
  })

  names(S_forest_CO2) <- names(S_forest)

  S_forest_df <- getDf1(S_forest_CO2)
  S_forest_df$model <- "Tree_growth"
  S_forest_df$treatment <- "CF"

  # Get forest soils emissions
  L_forest_soils_df <- bind_rows(lapply(L_forest_soils, FUN = bind_rows)) %>%
    select(t, source, value, Area, Est)
  L_forest_soils_df$model <- "Forest_soils"
  L_forest_soils_df$treatment <- "CF"

  # Get forest aquatic carbon loss
  L_AqC_forest_soils_df <- getDf1(L_AqC_forest_soils)
  L_AqC_forest_soils_df$model <- "Forest_AqC_loss"
  L_AqC_forest_soils_df$treatment <- "CF"

  # Get silvicuture/restoration emissions
  L_forest_df <- getDf3(L_forest)
  L_forest_df$model <- "Management"
  L_forest_df$treatment <- "PR"

  # Get peatland emissions
  L_peatland_df <- getDf2(L_peatland)
  L_peatland_df$model <- "Peatland"
  L_peatland_df$treatment <- "PR"

  # Get forest aquatic carbon loss
  L_AqC_peatland_df <- getDf1(L_AqC_peatland)
  L_AqC_peatland_df$model <- "Peatland_AqC_loss"
  L_AqC_peatland_df$treatment <- "PR"

  res <- bind_rows(S_forest_df,
                   L_forest_soils_df,
                   L_AqC_forest_soils_df,
                   L_forest_df,
                   L_peatland_df,
                   L_AqC_peatland_df)

  res <- res %>%
    filter(!is.na(value) & value != 0) %>%
    mutate(value = ifelse(grepl("L_", source), -value, value)) %>%
    select(model,treatment,t,Area,Est,source,value)

  return(res)
}

if (0){
  x <- seq(0,1,length.out=101)
  xy <- data.frame(x=rep(x,3),
                   y=c(rest_dyn_mod(x,n=1,0,1,convThresh=0.99),
                       rest_dyn_mod(x,n=2.25,0,1,convThresh=0.99),
                       rest_dyn_mod(x,n=5,0,1,convThresh=0.99)),
                   n=rep(c(1,2.25,5), each=101))

  pAsym <- ggplot(xy, aes(x=x, y=y, col=factor(n))) +
    geom_line() +
    theme_bw() +
    labs(col="n")

  png("../Figures/asymptotic_function.png",
      width=10.5, height=7.5, units="cm", res=300)
  pAsym
  dev.off()
}
