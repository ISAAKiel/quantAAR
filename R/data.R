#' A fictional burial site
#'
#' A dataset containing graves (objects) and stereotypical grave attributes (variables)
#'
#' @format A data frame with 50 rows and 18 variables:
#' \itemize{
#'   \item sex_male: anthropologically male (0,1)
#'   \item sex_female: anthropologically female (0,1)
#'   \item pos_crouched: buried in a crouched position (0,1)
#'   \item pos_extended: buried in an extended position (0,1)
#'   \item orient_N-S: N-S-oriented (0,1)
#'   \item orient_W-E: W-E-oriented (0,1)
#'   \item axe_1: axe type 1 (1--10)
#'   \item axe_2: axe type 2 (1--10)
#'   \item adze_1: adze type 2 (1--10)
#'   \item adze_2: adze type 2 (1--10)
#'   \item pottery_1: pottery type 1 (1--10)
#'   \item pottery_2: pottery type 2 (1--10)
#'   \item pottery_3: pottery type 3 (1--10)
#'   \item pottery_4: pottery type 4 (1--10)
#'   \item goldring: goldring (1--10)
#'   \item goldbead: goldbead type 2 (1--10)
#'   \item fibula_1: fibula type 1 (1--10)
#'   \item fibula_2: fibula type 2 (1--10)
#' }
#' @name bs1
NULL

#' A burial site from the Unetice culture in western Slovakia
#'
#' A dataset containing graves (objects) and grave attributes
#' (variables) of a burial site from the Unetice culture in western Slovakia.
#'
#' @format A data frame with 62 rows and 34 variables:
#' \itemize{
#'   \item grave_nr: number of the grave in Tocik 1979
#'   \item burial_depth_in_cm: numeric
#'   \item burial_length_in_cm: numeric
#'   \item burial_width_in_cm: numeric
#'   \item orientation: factor variable of grave orientation
#'   \item sex: factor variable of the anthropological determination
#'   \item posture: factor variable of the posture of the skeleton
#'   \item ceramics_general: count of ceramic items that could not be classified further
#'   \item pots: count
#'   \item bowls: count
#'   \item cups: count
#'   \item jugs: count
#'   \item pearls_bronze: count
#'   \item pearls_amber: count
#'   \item spiral_bead_bronze: count
#'   \item locket_shell: count
#'   \item temple_rings_big: count
#'   \item temple_rings_small: count
#'   \item ring_gold: count
#'   \item ring_bronze: count
#'   \item burl_ring: count
#'   \item arm_bracelet: count
#'   \item arm_spiral: count
#'   \item roll.top_pin: count
#'   \item eylet_pin: count
#'   \item dagger: count
#'   \item misc_shells: count
#'   \item misc_stones: count
#'   \item misc_jaspis: count
#'   \item misc_bones: count
#'   \item misc_animal_teeth: count
#'   \item misc_bronze: count
#'   \item misc_clay_tuyere: count
#'   \item notes: character variable of additional notes
#' }
#'
#' @source {Tocik, Anton (1979) Vycapy-Opatovce : a dalsie pohrebiska z
#'  starsej doby bronzovej na Juhozapadnom Slovensku = Vycapy-Oopatovce :
#'  und weitere altbronzezeitliche Graberfelder in der Sudwestslowakei. Nitra.}
#' @name matuskovo
NULL