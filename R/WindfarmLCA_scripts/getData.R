#' getData
#' @param path path to UI
#' @return Complete Input Data
#' @export
getData <- function(path) {

  df_struct <- read.csv("Templates/CCWoP_vars.csv") %>%
    mutate(Input.name=str_replace(Input.name, "\\ {2,}", "\\ ")) %>% # remove non-standard formatting
    mutate(Input.name=str_replace(Input.name, "\\ +$", ""))# remove non-standard formatting

  core.dat <- getCoreInputData(path, df_struct)
  input.dat <- getForestryInputData(path, df_struct)
  construct.dat <- getConstructionInputData(path, df_struct)

  return(dat=list(core.dat=core.dat,
                  input.dat=input.dat,
                  construct.dat=construct.dat))

}

#' getCoreInputData
#' @param path path to UI
#' @param df_struct dataframe summarising the data structure to output
#' @return Core Input Data
#' @export
getCoreInputData <- function(path, df_struct) {

  df <- read_excel(path,
                   sheet = "Core input data",
                   range = "B9:G123",
                   col_names = c("Input.data", "Exp", "rem1", "Min", "rem2", "Max"))

  df <- df %>%
    select(!grep("rem", colnames(df))) %>%
    filter(!is.na(Input.data)) %>%
    mutate(Input.data=str_replace(Input.data, "\\ {2,}", "\\ ")) %>% # remove non-standard formatting
    mutate(Input.data=str_replace(Input.data, "\\ +$", "")) %>% # remove non-standard formatting
    mutate(across(c(Exp, Min, Max), ~ ifelse(.=="Yes", 2, .))) %>%
    mutate(across(c(Exp, Min, Max), ~ ifelse(.=="No", 1, .))) %>%
    mutate(across(c(Exp, Min, Max), ~ as.numeric(.))) %>%
    filter(!(is.na(Exp) & is.na(Min) & is.na(Max))) # if all inputs are NA, remove

  df <- left_join(df,
                  df_struct %>%
                    filter(Dataset=="core") %>%
                    select(-Dataset) %>%
                    rename(Input.data=Input.name),
                  by="Input.data") %>%
    filter(!is.na(Variable.name))

  core.dat <- vector(mode = "list", length = length(unique(df$Subset.name)))
  names(core.dat) <- unique(df$Subset.name)

  for (i in 1:length(unique(df$Subset.name))) {

    df.subset <- df %>%
      filter(Subset.name==unique(df$Subset.name)[i])

    x <- lapply(split((df.subset %>% select(-c(Input.data, Subset.name, Variable.name))), 1:nrow(df.subset)), unlist)
    names(x) <- df.subset$Variable.name

    core.dat[[i]] <- x
  }

  return(core.dat)
}

#' getForestryInputData
#' @param path path to UI
#' @param df_struct dataframe summarising the data structure to output
#' @return Forestry Input Data
#' @export
getForestryInputData <- function(path, df_struct) {
  df <- read_excel(path,
                   sheet = "Forestry input data",
                   range = "C12:H260",
                   col_names = c("Input.data", "Exp", "rem1", "Min", "rem2", "Max"))

  suppressWarnings(
    df <- df %>%
      select(!grep("rem", colnames(df))) %>%
      filter(!is.na(Input.data)) %>%
      mutate(Input.data=str_replace(Input.data, "\\ {2,}", "\\ ")) %>% # remove non-standard formatting
      mutate(Input.data=str_replace(Input.data, "\\ +$", "")) %>% # remove non-standard formatting
      mutate(across(c(Exp, Min, Max), ~ ifelse(.=="Yes", 1, .))) %>%
      mutate(across(c(Exp, Min, Max), ~ ifelse(.=="No", 1, .))) %>%
      mutate(across(c(Exp, Min, Max), ~ as.numeric(.)))
  )

  ## Manipulate strings to account for repetition in variable names in UI
  df <- left_join(df %>%
                    mutate(AREA=NA) %>%
                    mutate(AREA=ifelse(grepl("AREA", Input.data), Input.data, AREA)) %>%
                    fill(AREA, .direction = "down") %>%
                    mutate(AREA=ifelse(is.na(AREA), "", AREA)) %>%
                    mutate(Input.data=paste(Input.data, AREA, sep="..")) %>%
                    select(-AREA),
                  df_struct %>%
                    filter(Dataset=="forestry") %>%
                    select(-Dataset) %>%
                    rename(Input.data=Input.name) %>%
                    mutate(AREA=NA) %>%
                    mutate(AREA=ifelse(grepl("AREA", Input.data), Input.data, AREA)) %>%
                    fill(AREA, .direction = "down") %>%
                    mutate(AREA=ifelse(is.na(AREA), "", AREA)) %>%
                    mutate(Input.data=paste(Input.data, AREA, sep="..")) %>%
                    select(-AREA),
                  by="Input.data") %>%
    mutate(Input.data=str_remove(Input.data, "\\.\\..*")) %>%
    filter(!(is.na(Exp) & is.na(Min) & is.na(Max))) %>% # if all inputs are NA, remove
    group_by(Subset.name) %>%
    mutate(Subset.name=ifelse(n() <= 11 & length(grep("Area", Subset.name))>0, NA, Subset.name)) %>% # remove 'singletons': these are empty areas with non NA pow_curve/soil_type/rotation/restoration_interventions due to drop down menu
    ungroup() %>%
    filter(!is.na(Subset.name)) %>%
    filter(!is.na(Variable.name))

  input.dat <- vector(mode = "list", length = length(unique(df$Subset.name)))
  names(input.dat) <- unique(df$Subset.name)

  for (i in 1:length(unique(df$Subset.name))) {

    df.subset <- df %>%
      filter(Subset.name==unique(df$Subset.name)[i])

    x <- lapply(split((df.subset %>% select(-c(Input.data, Subset.name, Variable.name))), 1:nrow(df.subset)), unlist)
    names(x) <- df.subset$Variable.name

    input.dat[[i]] <- x
  }

  return(input.dat)
}

#' getConstructionInputData
#' @param path path to UI
#' @param df_struct dataframe summarising the data structure to output
#' @return Construction Input Data
#' @export
getConstructionInputData <- function(path, df_struct) {
  df <- read_excel(path,
                   sheet = "Construction input data",
                   range = "C10:H113",
                   col_names = c("Input.data", "Exp", "rem1", "Min", "rem2", "Max"))

  df <- df %>%
    select(!grep("rem", colnames(df))) %>%
    filter(!is.na(Input.data)) %>%
    mutate(Input.data=str_replace(Input.data, "\\ {2,}", "\\ ")) %>% # remove non-standard formatting
    mutate(Input.data=str_replace(Input.data, "\\ +$", "")) %>% # remove non-standard formatting
    mutate(across(c(Exp, Min, Max), ~ ifelse(.=="Yes", 1, .))) %>%
    mutate(across(c(Exp, Min, Max), ~ ifelse(.=="No", 1, .))) %>%
    mutate(across(c(Exp, Min, Max), ~ as.numeric(.)))

  df %>%
    mutate(AREA=NA,
           MATERIAL=NA) %>%
    mutate(AREA=ifelse(grepl("AREA", Input.data), Input.data, AREA),
           MATERIAL=ifelse(grepl("(AREA \\d|Number of turbines in this area|Turbine foundations|Hardstanding|Piling|Volume of Concrete)", Input.data), Input.data, MATERIAL)) %>%
    fill(AREA, .direction = "down") %>%
    fill(MATERIAL, .direction = "down") %>%
    mutate(AREA=ifelse(is.na(AREA), "", AREA),
           MATERIAL=ifelse(is.na(MATERIAL), "", MATERIAL)) %>%
    mutate(Input.data=paste(Input.data, AREA, sep="..")) %>%
    mutate(Input.data=paste(Input.data, MATERIAL, sep="..")) %>%
    select(-c(AREA, MATERIAL))

  df_struct %>%
    filter(Dataset=="construct") %>%
    select(-Dataset) %>%
    rename(Input.data=Input.name) %>%
    mutate(AREA=NA,
           MATERIAL=NA) %>%
    mutate(AREA=ifelse(grepl("AREA", Input.data), Input.data, AREA),
           MATERIAL=ifelse(grepl("(AREA \\d|Number of turbines in this area|Turbine foundations|Hardstanding|Piling|Volume of Concrete)", Input.data), Input.data, MATERIAL)) %>%
    fill(AREA, .direction = "down") %>%
    fill(MATERIAL, .direction = "down") %>%
    mutate(AREA=ifelse(is.na(AREA), "", AREA),
           MATERIAL=ifelse(is.na(MATERIAL), "", MATERIAL)) %>%
    mutate(Input.data=paste(Input.data, AREA, sep="..")) %>%
    mutate(Input.data=paste(Input.data, MATERIAL, sep="..")) %>%
    select(-c(AREA, MATERIAL))

  ## Manipulate strings to account for repetition in variable names in UI
  df <- left_join(df %>%
                    mutate(AREA=NA,
                           MATERIAL=NA) %>%
                    mutate(AREA=ifelse(grepl("AREA", Input.data), Input.data, AREA),
                           MATERIAL=ifelse(grepl("(AREA \\d|Number of turbines in this area|Turbine foundations|Hardstanding|Piling|Volume of Concrete)", Input.data), Input.data, MATERIAL)) %>%
                    fill(AREA, .direction = "down") %>%
                    fill(MATERIAL, .direction = "down") %>%
                    mutate(AREA=ifelse(is.na(AREA), "", AREA),
                           MATERIAL=ifelse(is.na(MATERIAL), "", MATERIAL)) %>%
                    mutate(Input.data=paste(Input.data, AREA, sep="..")) %>%
                    mutate(Input.data=paste(Input.data, MATERIAL, sep="..")) %>%
                    select(-c(AREA, MATERIAL)),
                  df_struct %>%
                    filter(Dataset=="construct") %>%
                    select(-Dataset) %>%
                    rename(Input.data=Input.name) %>%
                    mutate(AREA=NA,
                           MATERIAL=NA) %>%
                    mutate(AREA=ifelse(grepl("AREA", Input.data), Input.data, AREA),
                           MATERIAL=ifelse(grepl("(AREA \\d|Number of turbines in this area|Turbine foundations|Hardstanding|Piling|Volume of Concrete)", Input.data), Input.data, MATERIAL)) %>%
                    fill(AREA, .direction = "down") %>%
                    fill(MATERIAL, .direction = "down") %>%
                    mutate(AREA=ifelse(is.na(AREA), "", AREA),
                           MATERIAL=ifelse(is.na(MATERIAL), "", MATERIAL)) %>%
                    mutate(Input.data=paste(Input.data, AREA, sep="..")) %>%
                    mutate(Input.data=paste(Input.data, MATERIAL, sep="..")) %>%
                    select(-c(AREA, MATERIAL)),
                  by="Input.data") %>%
    mutate(Input.data=str_remove(Input.data, "\\.\\..*")) %>%
    filter(!(is.na(Exp) & is.na(Min) & is.na(Max))) %>% # if all inputs are NA, remove
    filter(!is.na(Variable.name))

  construct.dat <- vector(mode = "list", length = length(unique(df$Subset.name)))
  names(construct.dat) <- unique(df$Subset.name)

  for (i in 1:length(unique(df$Subset.name))) {

    df.subset <- df %>%
      filter(Subset.name==unique(df$Subset.name)[i])

    x <- lapply(split((df.subset %>% select(-c(Input.data, Subset.name, Variable.name))), 1:nrow(df.subset)), unlist)
    names(x) <- df.subset$Variable.name

    construct.dat[[i]] <- x
  }

  return(construct.dat)
}

#' getGrowthYieldData
#' @return Growth and yield table
#' @export
getGrowthYieldData <- function() {
  gy <- suppressMessages(read_xlsx("Templates/Growth_and_yield.xlsx",
                  sheet = "Sheet1",
                  range = "A1:AV737",
                  progress = F))

  gy <- gy %>%
    rename(Spp = "Tree Spp",
           H = "Top ht m",
           V = "Vol m³/ha",
           V_Biofuel = "Volume fuel (m3/ha)",
           V_wpF = "Volume short-lived products (m3/ha)",
           V_wpM = "Volume medium-lived products (m3/ha)",
           V_wpS = "Volume long-lived products (m3/ha)",
           B_Biofuel = "Biomass fuel (odt/ha)",
           B_wpF = "Biomass short-lived products (odt/ha)",
           B_wpM = "Biomass medium-lived products (odt/ha)",
           B_wpS = "Biomass long-lived products (odt/ha)",
           B_wpO = "Woody Residues (offcuts) (odt/ha)",
           B_s = "Stem biomass (odt/ha)",
           B_c = "Crown biomass (odt/ha)",
           B_r = "Root biomass (odt/ha)",
           B_b = "Branch biomass (odt/ha)",
           B_f = "Foliage biomass (odt/ha)",
           B_m = "Woody Residues (mulched) (odt/ha)",
           V_a = "Total above ground tree volume (m3/ha)",
           B_ag = "Total above ground tree biomass (odt/ha)",
           B_ag_gr = "Total above ground tree biomass (t green/ha)") %>%
    select(Spp, YC, Age, H, V, V_Biofuel, V_wpF, V_wpM, V_wpS, B_Biofuel, B_wpF, B_wpM, B_wpS, B_wpO, B_s, B_c, B_r, B_b, B_f, B_m, V_a, B_ag, B_ag_gr) %>%
    mutate(Spp = stringr::str_replace(Spp, "\\ ", "_")) %>%
    mutate(rho_r = B_r / (B_s + B_c + B_r), # prop root to total biomass
           rho_s = B_s / (B_s + B_c + B_r), # prop stem to total biomass
           rho_c = B_c / (B_s + B_c + B_r), # prop canopy to total biomass
           rho_b = B_b / (B_s + B_c + B_r), # prop branch to total biomass
           rho_f = B_f / (B_s + B_c + B_r), # prop foliage to total biomass
           rho_m = B_m / (B_s + B_c + B_r), # prop mulch (branh + stem) to total biomass
           rho_ag = B_ag / (B_s + B_c + B_r), # prop above ground to total biomass
           rho_Biofuel = B_Biofuel / B_ag, # prop biofuel to total above ground biomass
           rho_wpF = B_wpF / B_ag, # prop fast decay prod to total above ground biomass
           rho_wpM = B_wpM / B_ag, # prop medium decay prod to total above ground biomass
           rho_wpS = B_wpS / B_ag, # prop slow decay prod to total above ground biomass
           rho_wpO = B_wpO / B_ag,) # prop in situ offcuts to total above ground biomass

  return(gy)
}

#' getYC
#' @param input.dat UI forestry data
#' @param growthYield.dat Growth and yield table
#' @return Yield class estimated from UI average height/age
#' @export
getYC <- function(input.dat,
                  growthYield.dat) {

  # THIS FUNCTION...
  YC <- map(input.dat[grep("Area", names(input.dat))], .f = "YC")
  h_tree <- map(input.dat[grep("Area", names(input.dat))], .f = "h_tree")
  t_stand <- map(input.dat[grep("Area", names(input.dat))], .f = "t_stand")
  Spp <- map(input.dat[grep("Area", names(input.dat))], .f = "species")
  species <- c("Scots_pine", "Sitka_spruce")

  point_to_segment_distance <- function(P, A, B) {
    # Helper function: compute distance from point P (A,H) to line segments joining points in GY curve
    AP <- P - A
    AB <- B - A

    t <- sum(AP * AB) / sum(AB * AB)
    t <- max(0, min(1, t))   # clamp to [0, 1]

    closest <- A + t * AB
    return(sqrt(sum((P - closest)^2)))
  }

  point_to_curve_distance <- function(P, curve) {
    # Helper function: compute distance from point P (A,H) to GY curve
    n <- nrow(curve)

    distances <- numeric(n - 1)
    for (i in 1:(n - 1)) {
      A <- curve[i, ]
      B <- curve[i + 1, ]
      distances[i] <- point_to_segment_distance(P, A, B)
    }

    return(min(distances))
  }

  YC <- lapply(seq_along(YC), FUN = function(x) {
    if (is.null(YC[[x]])) { # estimate YC from avg. height/age inputs
      Spp_a <- species[Spp[[x]][1]]
      YC_avail_a <- unique((growthYield.dat %>%
                              filter(Spp == Spp_a))$YC)

      YC_a <- sapply(seq_along(h_tree[[x]]), FUN = function(y) {
        res <- sapply(seq_along(YC_avail_a), FUN = function(z) {
          curve <- growthYield.dat %>%
            filter(Spp == Spp_a, YC == YC_avail_a[z]) %>%
            select(Age, H) %>%
            as.matrix()
          d_YC <- point_to_curve_distance(P = c(t_stand[[x]][y], h_tree[[x]][y]),
                                          curve = curve)
        })

        YC_a_est <- YC_avail_a[which.min(res)]
        return(YC_a_est)
      })

      names(YC_a) <- c("Exp", "Min", "Max")

      if (0) {
        ggplot(growthYield.dat %>% filter(Spp==Spp_a),
               aes(x=Age, y=H, col=factor(YC))) +
          geom_line() +
          geom_point(data = data.frame(Age = t_stand[[x]],
                                       H = h_tree[[x]],
                                       YC = YC_a))
      }

      return(YC_a)
    } else {
      return(YC[[x]])
    }
  })

  names(YC) <- names(Spp)

  return(YC)
}

