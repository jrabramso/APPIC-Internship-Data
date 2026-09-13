library(httr2)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)

url <- "https://membership.appic.org/directory/search?search=1&new_id=20705&program_type_id=1&school_name=&pdir_name=&lead_name=&appic_num=&PROG_Describe=&ID_MSI_KWORDS=&program_country_id=1&APP_DUE_START=&APP_DUE_END=&POS_startdate_START=&POS_startdate_END=&ACCR_APA=&ACCR_CPA=&membership_type_id=&POS_FT_funded=&POS_PT_funded=&POS_FT_high=&POS_PT_high=&SUM_INTOTAL_1112=&STAFF_LICENSED_FT=&STAFF_LICENSED_PT=&ID_SFIP_SupervisePDs=&APP_INTERVENTION=&APP_ASSESSMENT="
# Your original search URL
base_url <- url

# Function to get program links from one search-results page
get_program_links <- function(page_num) {
  
  page_url <- paste0(base_url, "&p=", page_num)
  
  message("Getting page ", page_num)
  
  response <- request(page_url) |>
    req_user_agent(
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 Chrome/139 Safari/537.36"
    ) |>
    req_perform()
  
  page <- resp_body_html(response)
  
  links <- page |> html_elements("a")
  
  tibble(
    text = map_chr(links, html_text2),
    href = map_chr(links, ~ html_attr(.x, "href"))
  ) |>
    filter(str_detect(href, "^/directory/display/"))
}

all_program_links <- map_dfr(1:32, get_program_links) |>
  distinct(href, .keep_all = TRUE)

nrow(all_program_links)

all_program_links |>
  count(text, sort = TRUE) |>
  slice_head(n = 10)


all_program_links <- all_program_links |>
  mutate(
    program_url = paste0(
      "https://membership.appic.org",
      href
    )
  )

get_program_page <- function(program_url) {
  
  message("Getting: ", program_url)
  
  response <- request(program_url) |>
    req_user_agent(
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 Chrome/139 Safari/537.36"
    ) |>
    req_retry(max_tries = 3) |>
    req_perform()
  
  page <- resp_body_html(response)
  
  tables <- page |>
    html_elements("table") |>
    html_table()
  
  tibble(
    program_url = program_url,
    status = resp_status(response),
    tables = list(tables),
    page_text = page |>
      html_element("body") |>
      html_text2()
  )
}

safe_get_program_page <- safely(
  get_program_page,
  otherwise = NULL
)

all_program_data <- vector("list", nrow(all_program_links))

for (i in seq_len(nrow(all_program_links))) {
  
  message(
    "\n[", i, "/", nrow(all_program_links), "] ",
    all_program_links$text[i]
  )
  
  all_program_data[[i]] <- safe_get_program_page(
    all_program_links$program_url[i]
  )
  
  # Small pause between requests
  Sys.sleep(runif(1, 0.25, 0.75))
  
  # Save progress every 50 pages
  if (i %% 50 == 0) {
    saveRDS(
      all_program_data,
      "appic_raw_progress.rds"
    )
    message("Progress saved.")
  }
}

appic_raw <- tibble(
  text = all_program_links$text,
  href = all_program_links$href,
  program_url = all_program_links$program_url,
  result = all_program_data
) |>
  mutate(
    data = map(result, "result"),
    error = map(result, "error")
  )

saveRDS(appic_raw, "appic_raw.rds")

appic_raw = readRDS("appic_raw.rds")

library(tidyverse)

# ============================================================
# 1. Identify the basic fields that occur in most sites
# ============================================================

all_field_labels <- appic_raw$data |>
  map2_dfr(
    appic_raw$program_url,
    \(site, url) {
      
      site$tables[[1]] |>
        keep(\(tbl) ncol(tbl) == 2) |>
        map_dfr(\(tbl) {
          tibble(
            label = as.character(tbl[[1]]),
            value = as.character(tbl[[2]])
          )
        }) |>
        mutate(program_url = url)
    }
  ) |>
  mutate(
    label = str_squish(label)
  ) |>
  filter(
    !is.na(label),
    label != ""
  )

field_frequency <- all_field_labels |>
  distinct(program_url, label) |>
  count(label, name = "n_sites") |>
  mutate(
    percent_sites = 100 * n_sites / n_distinct(appic_raw$program_url)
  ) |>
  arrange(desc(n_sites))


# Fields appearing in at least 75% of sites
basic_fields <- field_frequency |>
  filter(percent_sites >= 75) |>
  pull(label)


# See what we're including
field_frequency |>
  filter(label %in% basic_fields)

extract_site <- function(site_data, basic_fields) {
  
  tables <- site_data$tables[[1]]
  
  
  # ----------------------------------------------------------
  # Basic/common fields
  # ----------------------------------------------------------
  
  basic <- tables |>
    keep(\(tbl) ncol(tbl) == 2) |>
    map_dfr(\(tbl) {
      tibble(
        label = as.character(tbl[[1]]),
        value = as.character(tbl[[2]])
      )
    }) |>
    mutate(
      label = str_squish(label),
      value = str_squish(value),
      value = na_if(value, "")
    ) |>
    filter(label %in% basic_fields) |>
    distinct(label, .keep_all = TRUE) |>
    pivot_wider(
      names_from = label,
      values_from = value
    )
  
  
  # ----------------------------------------------------------
  # Historical table
  # ----------------------------------------------------------
  
  history_candidates <- keep(
    tables,
    \(tbl) {
      ncol(tbl) >= 3 &&
        any(
          str_detect(
            as.character(tbl[[1]]),
            "^Number of Completed Applications"
          ),
          na.rm = TRUE
        )
    }
  )
  
  
  if (length(history_candidates) == 1) {
    
    history <- history_candidates[[1]] |>
      rename(variable = 1) |>
      mutate(
        variable = str_squish(as.character(variable))
      ) |>
      # Convert ALL columns to character before pivoting
      mutate(
        across(
          everything(),
          as.character
        )
      ) |>
      pivot_longer(
        cols = -variable,
        names_to = "year",
        values_to = "value"
      ) |>
      filter(
        year %in% c("2025-2026", "2026-2027")
      ) |>
      mutate(
        value = str_squish(value),
        value = na_if(value, "")
      ) |>
      pivot_wider(
        names_from = c(variable, year),
        values_from = value,
        names_glue = "{variable}_{year}"
      )
    
  } else {
    
    history <- tibble()
    
    if (length(history_candidates) != 1) {
      warning(
        "Expected 1 historical table, found ",
        length(history_candidates)
      )
    }
  }
  
  
  # ----------------------------------------------------------
  # Combine
  # ----------------------------------------------------------
  
  bind_cols(
    basic,
    history
  )
}

aurora_result <- appic_raw |>
  filter(str_detect(text, "Aurora Mental Health")) |>
  pull(data)

aurora_raw <- aurora_result[[1]]

aurora_extracted <- extract_site(
  aurora_raw,
  basic_fields
)

glimpse(aurora_extracted)
names(aurora_extracted)

appic_extracted <- map_dfr(
  appic_raw$data,
  extract_site,
  basic_fields = basic_fields
)

appic_extracted <- all_program_links |>
  select(
    directory_name = text,
    href,
    program_url
  ) |>
  bind_cols(appic_extracted)

names(appic_extracted)
ncol(appic_extracted)

library(janitor)

appic_extracted <- appic_extracted |>
  clean_names()

missing_summary <- appic_extracted |>
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) |>
  mutate(
    percent_missing = 100 * n_missing / nrow(appic_extracted)
  ) |>
  arrange(desc(percent_missing))

missing_summary

View(appic_extracted)

state_names <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California",
  "Colorado", "Connecticut", "Delaware", "District of Columbia",
  "Florida", "Georgia", "Guam",
  "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
  "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
  "Massachusetts", "Michigan", "Minnesota", "Mississippi",
  "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
  "New Jersey", "New Mexico", "New York", "North Carolina",
  "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
  "Puerto Rico",
  "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
  "Texas", "Utah", "Vermont", "Virginia", "Washington",
  "West Virginia", "Wisconsin", "Wyoming"
)

state_pattern <- paste(state_names, collapse = "|")

appic_extracted <- appic_extracted |>
  mutate(
    state = str_extract(
      address,
      regex(
        paste0("(", state_pattern, ")\\s+\\d{5}(?:-\\d{4})?$"),
        ignore_case = TRUE
      )
    ) |>
      str_remove("\\s+\\d{5}(?:-\\d{4})?$")
  )

appic_extracted |>
  filter(is.na(state)) |>
  select(site, address)

appic_extracted = appic_extracted |> 
  mutate(apps_per_spot_2025_2026 = 
           round(as.numeric(number_of_completed_applications_2025_2026)/
                   as.numeric(total_number_of_interns_2025_2026),2),
         apps_per_spot_2026_2027 = 
           round(as.numeric(number_of_completed_applications_2026_2027)/
                   as.numeric(total_number_of_interns_2026_2027),2),
         calculated_total_required_hours = 
           as.numeric(minimum_number_of_aapi_intervention_hours_if_applicable) +
           as.numeric(minimum_number_of_aapi_assessment_hours_if_applicable))

saveRDS(appic_extracted, "appic_extracted.rds")

write_csv(
  appic_extracted,
  "appic_extracted.csv",
  na = ""
)

appic_extracted = readRDS("appic_extracted.rds")

saveRDS(appic_extracted, "appic_extracted.rds")

write_csv(
  appic_extracted,
  "appic_extracted.csv",
  na = ""
)