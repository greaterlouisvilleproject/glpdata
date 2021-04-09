library(glptools)
load_glp_packages()
library(stringr)
library(censusapi)

abs_var <- c("GEO_ID", "YEAR", "NAICS2017", "NAICS2017_LABEL", "SEX", "SEX_LABEL",
             "ETH_GROUP", "ETH_GROUP_LABEL", "RACE_GROUP", "RACE_GROUP_LABEL",
             "EMP",   "FIRMPDEMP",   "RCPPDEMP",   "PAYANN",
             "EMP_S", "FIRMPDEMP_S", "RCPPDEMP_S", "PAYANN_S")

for(f in FIPS_df_two_stl$FIPS) {
  temp <- censusapi::getCensus("abscs",
                               vintage = 2017,
                               vars = abs_var,
                               regionin = paste0("state:", str_sub(f, 1, 2)),
                               region = paste0("county:", str_sub(f, 3, 5)),
                               key = Sys.getenv("CENSUS_API_KEY"))

  business_owner <- assign_row_join(business_owner, temp)
}

business_owner %<>%
  transmute(
    FIPS = paste0(state, county),
    year = YEAR,
    sex = recode(SEX_LABEL,
                 Total = "total", Male = "male",
                 Female = "female", `Equally male/female` = "split",
                 .default = NA_character_),
    race = case_when(
      ETH_GROUP_LABEL == "Hispanic" ~ "hispanic",
      RACE_GROUP_LABEL == "Nonminority" ~ "white",
      RACE_GROUP_LABEL == "Black or African American" ~ "black",
      RACE_GROUP_LABEL == "Total" & ETH_GROUP_LABEL == "Total" ~ "total"),
    NAICS2017 = if_else(NAICS2017_LABEL == "Total for all sectors", "total", NAICS2017_LABEL),
    employees_num = EMP,
    firms_num = FIRMPDEMP,
    payroll_num = PAYANN,
    revenue_num = RCPPDEMP,
    emp_se = EMP_S,
    firms_se = FIRMPDEMP_S,
    payroll_se = PAYANN_S,
    revenue_se = RCPPDEMP_S) %>%
  filter(!is.na(race))

categories <- c("sex", "race", "NAICS2017")

for (c in categories) {
  temp <- business_owner %>%
    filter_at(setdiff(categories, c), ~ . == "total")

  output <- assign_row_join(output, temp)

}

business_owner %<>%
  mutate(across(c(year, employees_num:revenue_se), ~as.numeric(.)))

test <- business_owner %>%
  stl_merge(employees_num:revenue_num, method = "sum", other_grouping_vars = "NAICS2017")

test1 <- test %>%
  per_capita_adj(employees_num:revenue_num, other_grouping_vars = "NAICS2017", scale = 100)

#test <- business_owner %>%
#  stl_merge(employees_num:revenue_num, method = "sum", other_grouping_vars = "NAICS2017")


test5 <- glpdata::population_county %>%
  filter(year == 2017, sex == "total", race %in% c("black", "total")) %>%
  select(-core_county) %>%
  pivot_wider(names_from = race, values_from = population) %>%
  pull_peers(add_info = T) %>%
  filter(current == 1, FIPS != "29189", FIPS != "29510") %>%
  mutate(black_pct = black / total * 100)

glp_load_packages(T)


test1 <- test %>%
  per_capita_adj(employees_num:revenue_num, other_grouping_vars = "NAICS2017", scale = 100) %>%
  filter(sex == "total", race %in% c("black", "total"), NAICS2017 == "total") %>%
  select(-year) %>%
  pull_peers(add_info = T) %>%
  select(FIPS, race, firms_num, payroll_num, revenue_num, employees_num) %>%
  pivot_wider(names_from = race,
              values_from = c(firms_num, payroll_num, revenue_num, employees_num)) %>%
  mutate(
    black_pct_firm    = firms_num_black/firms_num_total*100,
    black_pct_payroll = payroll_num_black/payroll_num_total*100,
    black_pct_revenue = revenue_num_black/revenue_num_total*100,
    black_pct_emp     = employees_num_black/employees_num_total*100) %>%
  pull_peers(add_info = T) %>%
  filter(current ==1)

write_csv(test1, "data_update.csv")

black_own <- test1 %>%
  filter(sex == "total", race %in% c("black", "total"), NAICS2017 == "total") %>%
  select(-year) %>%
  pull_peers(add_info = T) %>%
  select(-NAICS2017)

white_own <- test1 %>%
  filter(sex == "total", race == "white", NAICS2017 == "total") %>%
  select(-year) %>%
  pull_peers(add_info = T) %>%
  select(-NAICS2017)

write_csv(black_own, "black_own.csv")
write_csv(white_own, "white_own.csv")

png("test.png", 3000, 2400, res = 200)
ranking(black_own,
        firms_num_per_100,
        plot_title = "Black-owned businesses per 100 Black residents")
dev.off()


ranking <- ranking <- function(df, var, plot_title = "",
                               year = NULL, sex = "total", race = "total", peers = "Current",
                               order = "Descending",
                               y_title = "Percent", caption_text = "", subtitle_text = "",
                               bar_label = TRUE, sigfig = 3, accuracy = 0.1,
                               label_function, alternate_text = NULL,
                               ranking_colors = TRUE,
                               font_family = "Verdana",
                               text_size = 20,
                               axis_size = 60){

  # Copy variable var to a new column for use with the '$' operator
  var <- dplyr:::tbl_at_vars(df, vars(!!enquo(var)))
  df$var <- df[[var]]

  # Filter to sex, race, and year
  if ("sex" %in% names(df)) df <- df[df$sex == sex,]
  if ("race" %in% names(df)) df <- df[df$race == race,]
  if("year" %in% names(df)) {
    if (is.null(year)) year <- max(years_in_df(df, var))
    df <- df[df$year %in% year,]

    if (length(year) > 1) {
      df %<>%
        group_by_at(df %cols_in% c("MSA", "FIPS")) %>%
        summarise(var = mean(var, na.rm = TRUE)) %>%
        ungroup()
    }
  }

  # Add peer data if not already present
  if (df_type(df) %in% c("FIPS", "MSA") & "current" %not_in% names(df)) df %<>% pull_peers()

  # Filter to peer parameter
  if (peers %in% c("current", "Current"))   df %<>% filter(current == 1)
  if (peers %in% c("baseline", "Baseline")) df %<>% filter(baseline == 1)

  # Sort according to order parameter
  if (order %in% c("descending", "Descending")) df %<>% arrange(desc(var))
  if (order %in% c("ascending", "Ascending"))   df %<>% arrange(var)

  df %<>% filter(!is.na(var))

  # Create numbered city labels for left side of graph
  df %<>%
    mutate(
      rank = row_number(),
      names = paste0(rank, ". ", city))

  # Set bar colors
  if (ranking_colors) {

    color_values <- c("#96ca4f", "#ffd600", "#db2834")
    color_names <- c("green", "yellow", "red")
    if (order %in% c("descending", "Descending")) {color_names  = rev(color_names)}

    breaks <- classInt::classIntervals(na.omit(df$var), 3, style = "jenks")
    df$color <- NA
    df$color[df$var <= breaks$brks[2]] <- color_names[1]
    df$color[df$var > breaks$brks[2] & df$var <= breaks$brks[3]] <- color_names[2]
    df$color[df$var > breaks$brks[3]] <- color_names[3]

  } else {
    df$color <- "blue"
    color_values <- "#f58021"
    color_names <- "blue"
  }

  if (order %in% c("descending", "Descending")) color_values = rev(color_values)

  # Create numeric labels
  if (!missing(label_function)) {
    label_text <- df$var %>% signif(sigfig) %>% label_function()
  } else if (y_title == "Dollars") {
    if(mean(df$var, na.rm = TRUE) > 10000) {
      label_text <- df$var %>% signif(sigfig) %>% scales::dollar(accuracy = accuracy, scale = .001, suffix = "k")
    } else {
      label_text <- df$var %>% signif(sigfig) %>% scales::dollar(accuracy = .01)
    }
  } else if (stringr::str_detect(y_title, "Percent")) {
    label_text <- df$var %>% signif(sigfig) %>% scales::percent(accuracy = accuracy, scale = 1, suffix = "%")
  } else {
    label_text <- df$var %>% signif(sigfig) %>% scales::comma(accuracy = accuracy)
  }

  # Set text format, highlight and italicise Louisville text, highlight Louisville bar
  df$textcolor <- "#000000"
  df$textcolor[df$city == "Louisville"] <- "#00a9b7"

  df$linecolor <- "#ffffff"
  df$linecolor[df$city == "Louisville"] <- "#00a9b7"

  df$lou <- if_else(df$city == "Louisville", 1, 0)

  df$text_alignment <- 1.1
  if (!is.null(alternate_text)) df$text_alignment[df$rank %in% alternate_text] <- -0.1

  ### PLOT GRAPH

  # Initial plot
  p <- ggplot(data = df,
              aes(x = factor(names, levels = rev(names)),
                  y = var,
                  fill  = factor(color, levels = color_names, ordered = TRUE)))

  p <- p + guides(fill = FALSE, color = FALSE)

  # Add bars
  p <- p +
    geom_bar(aes(color = factor(lou, levels = 0:1)),
             stat = "identity",
             size = 1) +
    coord_flip() +
    ggthemes::theme_tufte()

  p <- p + scale_fill_manual(values = color_values)
  p <- p + scale_color_manual(values = c("#ffffff", "#00a9b7"))

  # Add features
  title_scale <- min(1, 48 / nchar(plot_title))

  p <- p + theme(text = element_text(family = font_family),
                 plot.title = element_text(size = 22 * title_scale, hjust = 0.5, margin = margin(b = 20, unit = "pt")),
                 axis.text.y = element_text(hjust = 0,
                                            size = axis_size, color = rev(df$textcolor)),
                 axis.title.x = element_text(size = axis_size),
                 axis.ticks = element_blank(),
                 axis.text.x = element_blank(),
                 plot.caption = element_text(size = 36, lineheight = 0.5))

  if(subtitle_text != ""){
    p <- p + theme(plot.subtitle = element_text(hjust = 0.5, size = 16)) +
      labs(subtitle = subtitle_text)
  }

  # Add numeric labels to bars based on bar_label parameter
  if (y_title != "" & bar_label) {
    p <- p + geom_text(aes(label = label_text, hjust = text_alignment),
                       size = text_size,
                       family = font_family)
  }

  # Add vertical line to the left side of the bars based on the h_line parameter
  if (min(df$var, na.rm = TRUE) < 0) p <- p + geom_hline(yintercept = 0, linetype = "longdash", size = 2)

  # Add remaining text
  p <- p + labs(title = plot_title, y = y_title,
                x = "", caption = caption_text)
  p
}











