library(bskyr)
library(dplyr)
library(stringr)
library(googlesheets4)

# authenticate ----
# no use running if it can't do anything
if (Sys.getenv('GARGLE_KEY') =='') {
  stop('No key found to decrypt the secret.')
}
gs4_auth(path = gargle::secret_decrypt_json('inst/secret/bskyr-cran-bot.json', key = 'GARGLE_KEY'))

if (!bs_has_user()) {
  stop('bsky user not found.')
}
if (!bs_has_pass()) {
  stop('bsky pass not found.')
}
auth <- bs_auth(bs_get_user(), bs_get_pass())

# load available packages ----
pkgs <- available.packages(filters = c('CRAN', 'duplicates')) |>
  as_tibble() |>
  select(Package, Version)

# Run first time to create a spreadsheet
# gs4_auth()
# gs4_create(name = 'bskyr-cran-bot', sheets = list(pkgs = pkgs))

# get the old spreadsheet ----
old_pkgs <- read_sheet(ss = '135p8xqI3LGIyuvwSjav13tY10fRu8dUPFjVNVal18Tk', sheet = 'pkgs')


# identify changes ----
updates <- setdiff(pkgs, old_pkgs)

new_pkgs <- updates |>
  filter(!Package %in% old_pkgs$Package)

updates <- updates |>
  filter(!Package %in% new_pkgs$Package)

removed <- setdiff(old_pkgs, pkgs) |>
  filter(!Package %in% pkgs$Package)

# make posts ----

if (nrow(updates) > 0) {
  update_txt <- vapply(seq_len(nrow(updates)), function(i) {
    paste0(updates$Package[i], ' (', updates$Version[i], ')')
  }, character(1))

  n_char_update <- cumsum(nchar(update_txt))

  if (max(n_char_update) > 260) {
    # split into multiple posts
    update_txt <- update_txt |>
      split(cut(n_char_update, breaks = ceiling(max(n_char_update) / 260))) |>
      lapply(paste0, collapse = ', ')

    lapply(update_txt, function(txt) {
      bskyr::bs_post(
        text = paste0('Updates on CRAN: ', txt),
        auth = auth
      )
      # avoid immediate new posts
      Sys.sleep(3)
    })

  } else {
    update_txt <- update_txt |>
      paste0(collapse = ', ')
    bskyr::bs_post(
      text = paste0('Updates on CRAN: ', update_txt),
      auth = auth
    )
    # avoid immediate new posts
    Sys.sleep(3)
  }
}

cat('Updates:', nrow(updates), '\n')

if (nrow(new_pkgs) > 0) {
  lapply(
    seq_len(nrow(new_pkgs)),
    function(i) {
      bskyr::bs_post(
        text = paste0('New on CRAN: ', new_pkgs$Package[i], ' (', new_pkgs$Version[i], '). View at ',
                      'https://CRAN.R-project.org/package=', new_pkgs$Package[i]),
        auth = auth
      )
      # avoid immediate new posts
      Sys.sleep(3)
    }
    )
}

cat('New packages:', nrow(new_pkgs), '\n')

if (nrow(removed) > 0) {
  removed_txt <- vapply(seq_len(nrow(removed)), function(i) {
    paste0(removed$Package[i], ' (', removed$Version[i], ')')
  }, character(1))

  n_char_remove <- cumsum(nchar(removed_txt))

  if (max(n_char_remove) > 260) {
    # split into multiple posts
    removed_txt <- removed_txt |>
      split(cut(n_char_remove, breaks = ceiling(max(n_char_remove) / 260))) |>
      lapply(paste0, collapse = ', ')

    lapply(removed_txt, function(txt) {
      bskyr::bs_post(
        text = paste0('Removed from CRAN: ', txt),
        auth = auth
      )
      # avoid immediate new posts
      Sys.sleep(3)
    })

  } else {
    removed_txt <- removed_txt |>
      paste0(collapse = ', ')
    bskyr::bs_post(
      text = paste0('Removed from CRAN: ', removed_txt),
      auth = auth
    )
    # avoid immediate new posts
    Sys.sleep(3)
  }
}

cat('Removed packages:', nrow(removed), '\n')

# update the spreadsheet ----
write_sheet(pkgs, ss = '135p8xqI3LGIyuvwSjav13tY10fRu8dUPFjVNVal18Tk', sheet = 'pkgs')
