#' getData
#' @param path path to UI
#' @return Complete Input Data
#' @export
getData <- function(path) {

  df_struct <- read.csv("Templates/CCWoP_vars.csv") %>%
    mutate(Input.name=str_replace(Input.name, "\\ {2,}", "\\ ")) %>% # remove non-standard formatting
    mutate(Input.name=str_replace(Input.name, "\\ +$", ""))# remove non-standard formatting

  core.dat <- getCoreInputData(path, df_struct)
  forestry.dat <- getForestryInputData(path, df_struct)
  construct.dat <- getConstructionInputData(path, df_struct)

  return(dat=list(core.dat=core.dat,
                  forestry.dat=forestry.dat,
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
                   range = "C12:H119",
                   col_names = c("Input.data", "Exp", "rem1", "Min", "rem2", "Max"))

  df <- df %>%
    select(!grep("rem", colnames(df))) %>%
    filter(!is.na(Input.data)) %>%
    mutate(Input.data=str_replace(Input.data, "\\ {2,}", "\\ ")) %>% # remove non-standard formatting
    mutate(Input.data=str_replace(Input.data, "\\ +$", "")) %>% # remove non-standard formatting
    mutate(across(c(Exp, Min, Max), ~ ifelse(.=="Yes", 1, .))) %>%
    mutate(across(c(Exp, Min, Max), ~ ifelse(.=="No", 1, .))) %>%
    mutate(across(c(Exp, Min, Max), ~ as.numeric(.)))

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
    filter(n() > 1) %>% # remove 'singletons': these are empty areas with non NA 'power curve' due to drop down menu
    ungroup() %>%
    filter(!is.na(Variable.name))

  forestry.dat <- vector(mode = "list", length = length(unique(df$Subset.name)))
  names(forestry.dat) <- unique(df$Subset.name)

  for (i in 1:length(unique(df$Subset.name))) {

    df.subset <- df %>%
      filter(Subset.name==unique(df$Subset.name)[i])

    x <- lapply(split((df.subset %>% select(-c(Input.data, Subset.name, Variable.name))), 1:nrow(df.subset)), unlist)
    names(x) <- df.subset$Variable.name

    forestry.dat[[i]] <- x
  }

  return(forestry.dat)
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
