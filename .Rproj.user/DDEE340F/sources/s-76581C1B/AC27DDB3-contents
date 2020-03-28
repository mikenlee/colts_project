#----
str_punct <- function(x, preserve = c(",", "'", "-", "\\."),
                      space = c(",", ";", "\\.", "?", "!")) {
  
  # punctation
  p = c("!", "\\", "#", "$", "%", "&", "'", "\\(", "\\)", "\\*", "\\+", ",",
        "-", "\\.", "/", ":", ";", "<", "=", ">", "?", "@", "\\[" , "\\\\",
        "\\]", "\\^", "_", "`", "\\{", "\\|", "\\}", "~", "\\.")
  
  # single punctation marks
  for (i in p[ !p %in% preserve ]) {
    x = gsub(paste0("(\\s*", i, "\\s*)+"), "", x)
  }
  
  # add missing spaces for some reserved marks
  for (i in space[ space %in% preserve ]) {
    x = gsub(paste0("(\\s*", i, "\\s*)+"), paste0(i, " "), x)
  }
  
  # no space before other preserved characters
  for(i in preserve[ !preserve %in% space ]) {
    x = gsub(paste0("\\s*", i, "+\\s*"), i, x)
  }
  
  # no start/end punctuation
  x = gsub("^(\\s|[[:punct:]])+|(\\s|[[:punct:]])+$", "", x)
  
  x
  
}


#----
#' Remove prefixes or suffixes
#' @param sep character separator, defaults to ","
#' @param side where to look, "left" (default) or "right"
#' @param greedy defaults to TRUE
#' @examples 
#' ex = c("a", "a, b", "a, b, c", "a, b, c, d")
#' str_filter(ex, side = "left"  , greedy = TRUE)
#' str_filter(ex, side = "right" , greedy = TRUE)
#' str_filter(ex, side = "left"  , greedy = FALSE)
#' str_filter(ex, side = "right" , greedy = FALSE)
str_filter <- function(x, sep = ",", side = "left", greedy = TRUE) {
  gsub(switch(side,
              left  = c(ifelse(greedy, "", "?"), ")", sep, "\\s*"),
              right = c(ifelse(greedy, "?", ""), ")\\s*", sep)) %>%
         c("(.*", ., "(.*)") %>%
         paste0(collapse = ""),
       switch(side, left = "\\2", right = "\\1"), x)
}

#----
#function to display duplicated rows

#dat %>% filter (isdup (l))

isdup <- function (x) duplicated (x) | duplicated (x, fromLast = TRUE)

#----
#function to collapse rows for tables .. used for formattables

# iris %>%
#   group_by(Species) %>%
#   slice(1:2) %>%
#   select(Species, everything()) %>%
#   collapse_rows_df(Species) %>%
#   formattable()

collapse_rows_df <- function(df, variable){
  
  group_var <- enquo(variable)
  
  df %>%
    group_by(!! group_var) %>%
    mutate(groupRow = 1:n()) %>%
    ungroup() %>%
    mutate(!!quo_name(group_var) := ifelse(groupRow == 1, as.character(!! group_var), "")) %>%
    select(-c(groupRow))
}
