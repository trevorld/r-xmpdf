# SPDX License List data
# CC0 licensed
# Update CC urls
if (TRUE) {
    library("dplyr")
    library("jsonlite")
    library("rvest")

    spdx_file <-"raw-data/spdx_license_list.html"

    if (!file.exists(spdx_file))
        download.file("https://spdx.org/licenses/", spdx_file)

    spdx <- read_html(spdx_file) |> html_table() |> bind_rows()

    names(spdx) <- c("name", "id", "fsf", "osi", "deprecated")

    spdx_json_file <- "raw-data/spdx.json"
    if (!file.exists(spdx_json_file))
        download.file("https://raw.githubusercontent.com/sindresorhus/spdx-license-list/main/spdx.json",
                      spdx_json_file)

    spdx_list <- jsonlite::fromJSON(spdx_json_file)
    spdx_json <- spdx_list |> bind_rows() |> select(url_alt = url)
    spdx_json$id <- names(spdx_list)

    spdx_licenses <- left_join(spdx, spdx_json, by = "id")

    spdx_licenses <- mutate(spdx_licenses,
                     fsf = ifelse(fsf == "Y", TRUE, fsf),
                     fsf = ifelse(fsf == "", FALSE, fsf),
                     fsf = as.logical(fsf),
                     osi = ifelse(osi == "Y", TRUE, fsf),
                     osi = ifelse(osi == "", FALSE, osi),
                     osi = as.logical(osi),
                     deprecated = ifelse(is.na(deprecated), FALSE, TRUE),
                     url = paste0("https://spdx.org/licenses/", id, ".html"),
                     url_alt = ifelse(id == "CC-BY-1.0", "https://creativecommons.org/licenses/by/1.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-2.0", "https://creativecommons.org/licenses/by/2.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-2.5", "https://creativecommons.org/licenses/by/2.5/", url_alt),
                     url_alt = ifelse(id == "CC-BY-3.0", "https://creativecommons.org/licenses/by/3.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-4.0", "https://creativecommons.org/licenses/by/4.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-1.0", "https://creativecommons.org/licenses/by-nc/1.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-2.0", "https://creativecommons.org/licenses/by-nc/2.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-2.5", "https://creativecommons.org/licenses/by-nc/2.5/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-3.0", "https://creativecommons.org/licenses/by-nc/3.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-4.0", "https://creativecommons.org/licenses/by-nc/4.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-ND-1.0", "https://creativecommons.org/licenses/by-nc-nd/1.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-ND-2.0", "https://creativecommons.org/licenses/by-nc-nd/2.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-ND-2.5", "https://creativecommons.org/licenses/by-nc-nd/2.5/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-ND-3.0", "https://creativecommons.org/licenses/by-nc-nd/3.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-ND-4.0", "https://creativecommons.org/licenses/by-nc-nd/4.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-SA-1.0", "https://creativecommons.org/licenses/by-nc-sa/1.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-SA-2.0", "https://creativecommons.org/licenses/by-nc-sa/2.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-SA-2.5", "https://creativecommons.org/licenses/by-nc-sa/2.5/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-SA-3.0", "https://creativecommons.org/licenses/by-nc-sa/3.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-NC-SA-4.0", "https://creativecommons.org/licenses/by-nc-sa/4.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-ND-1.0", "https://creativecommons.org/licenses/by-nd/1.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-ND-2.0", "https://creativecommons.org/licenses/by-nd/2.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-ND-2.5", "https://creativecommons.org/licenses/by-nd/2.5/", url_alt),
                     url_alt = ifelse(id == "CC-BY-ND-3.0", "https://creativecommons.org/licenses/by-nd/3.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-ND-4.0", "https://creativecommons.org/licenses/by-nd/4.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-SA-1.0", "https://creativecommons.org/licenses/by-sa/1.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-SA-2.0", "https://creativecommons.org/licenses/by-sa/2.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-SA-2.5", "https://creativecommons.org/licenses/by-sa/2.5/", url_alt),
                     url_alt = ifelse(id == "CC-BY-SA-3.0", "https://creativecommons.org/licenses/by-sa/3.0/", url_alt),
                     url_alt = ifelse(id == "CC-BY-SA-4.0", "https://creativecommons.org/licenses/by-sa/4.0/", url_alt),
                     url_alt = ifelse(id == "CC0-1.0", "https://creativecommons.org/publicdomain/zero/4.0/", url_alt)
    )
    which_cc <- grep("^Creative Commons", spdx_licenses$name)
    spdx_licenses$name[which_cc] <- gsub(" Non Commercial", "-NonCommercial", spdx_licenses$name[which_cc])
    spdx_licenses$name[which_cc] <- gsub(" Share Alike", "-ShareAlike", spdx_licenses$name[which_cc])
    spdx_licenses$name[which_cc] <- gsub(" No Deriv", "-NoDeriv", spdx_licenses$name[which_cc])
    spdx_licenses <- as.data.frame(spdx_licenses)

    spdx_licenses <- select(spdx_licenses, id, name, url, fsf, osi, deprecated, url_alt) |> as.data.frame()
    rownames(spdx_licenses) <- spdx_licenses$id
}

save(spdx_licenses, file="data/spdx_licenses.rda", version=2)
