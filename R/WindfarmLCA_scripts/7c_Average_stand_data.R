## 7c. Average stand data

#' growth_yield_tab
#' @param t stand age
#' @param soil_type soil type (deep peat or peaty gley)
#' @param species 1: scots pine; 2: sitka spruce
#' @return C_seq_soil
#' @export
growth_yield_tab <- function(t, soil_type, species = 2) {

  # THIS FUNCTION...

  # Only growth and yield tables for Sitka currently available
  if (species == 2) { # Sitka

    if (soil_type == 1) { # Peaty Gley: assume yield class 14

      avg_height <- (-0.0002 * t^3) + (0.0194 * t^2) + (0.023 * t)
      max_height <- (-0.0002 * t^3) + (0.0156 * t^2) + (0.1209 * t) + 1

      volume <- (-0.0087 * t^3) + (0.8817 * t^2) - (10.198 * t)
      volume[volume<0] <- 0

      spacing <- 1.3659 * exp(0.0123 * t)

    } else { # Deep peat: assume yield class 12
      avg_height <- (-0.0002 * t^3) + (0.0187 * t^2) - (0.0232 * t)
      max_height <- (-0.0002 * t^3) + (0.0151 * t^2) + (0.0726 * t) + 1

      volume <- (-0.0072 * t^3) + (0.7569 * t^2) - (9.1643 * t)
      volume[volume<0] <- 0

      spacing <- 1.3842 * exp(0.0108 * t)
    }

  } else {
    stop("Only growth and yield tables for Sitka currently available!")
  }

  return(list(avg_height = avg_height,
              max_height = max_height,
              volume = volume,
              spacing = spacing))
}
