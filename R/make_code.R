#' Add code based on locality, municipality and state codes
#'
#' @param this.tibble
#'
#' @return code.tibble
#' @export
#'
#' @examples
make_code <- function(this.tibble){



  if("CVE_ENT" %in% names(this.tibble) & "CVE_LOC" %in% names(this.tibble)){

  code.tibble <- this.tibble %>%
    dplyr::mutate(cve_loc = as.character(CVE_LOC), cve_mun = as.character(CVE_MUN), cve_ent = as.character(CVE_ENT)) %>%
    dplyr::mutate(CVE_ENT = as.character(CVE_ENT), CVE_MUN = as.character(CVE_MUN), CVE_LOC = as.character(CVE_LOC)) %>%
    dplyr::mutate(no_loc = nchar(CVE_LOC), no_mun = nchar(CVE_MUN), no_ent = nchar(CVE_ENT)) %>%
    dplyr::mutate(CVE_ENT = dplyr::if_else(no_ent==1,paste0("0",cve_ent), cve_ent)) %>%
    dplyr::mutate(CVE_MUN = dplyr::if_else(no_mun==1, paste0(CVE_ENT,"00", cve_mun),
                                         dplyr::if_else(no_mun==2, paste0(CVE_ENT,"0", cve_mun),
                                                        dplyr::if_else(no_mun==3, paste0(CVE_ENT, cve_mun),
                                                                       dplyr::if_else(no_mun==4, paste0("0", cve_mun), cve_mun))))) %>%
    dplyr::mutate() %>%
    dplyr::mutate(CVE_LOC=dplyr::if_else(no_loc==1, paste0(CVE_MUN,"000",cve_loc),
                                         dplyr::if_else(no_loc==2, paste0(CVE_MUN, "00",cve_loc),
                                                        dplyr::if_else(no_loc==3, paste0(CVE_MUN, "0", cve_loc),
                                                                       dplyr::if_else(no_loc==4, paste0(CVE_MUN, cve_loc),
                                                                                      dplyr::if_else(no_loc==8, paste0("0",cve_loc), cve_loc)))))) %>%
    dplyr::select(CVE_ENT, CVE_LOC, CVE_MUN, everything())

  }

  if(!"CVE_LOC" %in% names(this.tibble) & "NOM_LOC" %in% names(this.tibble)){

    code.tibble <- this.tibble %>%
      dplyr::mutate(NOM_ENT = toupper(stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII"))) %>%
      dplyr::mutate(NOM_LOC = toupper(stringi::stri_trans_general(str = NOM_LOC, id = "Latin-ASCII"))) %>%
      dplyr::select(-CVE_MUN, -NOM_MUN) %>%
      dplyr::left_join(inegi.cost.comm, by = c("NOM_ENT","NOM_LOC", "CVE_ENT")) %>%
      dplyr::select(CVE_ENT, CVE_LOC, CVE_MUN, everything())

  }

  if(!"CVE_ENT" %in% names(this.tibble)){

    edos.codes <- readr::read_csv("/home/atlantis/coastalvulnerability-inputs/processed_data/edos_gcyuc.csv") %>%
      dplyr::mutate(NOM_ENT = toupper(stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII")))

    inegi.table <- "inegi_coast_comm.csv"
    inegi.cost.comm <- fix_inegi(inegi.table)

    if("entidad" %in% names(this.tibble) & !"NOM_ENT" %in% names(this.tibble) & !"CVE_LOC" %in% names(this.tibble)){

      code.tibble <- this.tibble %>%
        dplyr::rename(NOM_ENT = entidad, NOM_LOC = localidad) %>%
        dplyr::left_join(inegi.cost.comm, by = c("NOM_ENT","NOM_LOC")) %>%
        dplyr::mutate(no_loc = nchar(CVE_LOC), no_mun = nchar(CVE_MUN), no_ent = nchar(CVE_ENT)) %>%
        dplyr::select(CVE_ENT, CVE_LOC, CVE_MUN, everything())

    }

    if("entidad" %in% names(this.tibble) & !"NOM_ENT" %in% names(this.tibble) & "CVE_LOC" %in% names(this.tibble)){

      code.tibble <- this.tibble %>%
        dplyr::rename(NOM_ENT = entidad, NOM_LOC = localidad) %>%
        dplyr::mutate(NOM_ENT = toupper(stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII"))) %>%
        dplyr::mutate(NOM_LOC = toupper(stringi::stri_trans_general(str = NOM_LOC, id = "Latin-ASCII"))) %>%
        dplyr::left_join(inegi.cost.comm, by = c("CVE_LOC","NOM_ENT","NOM_LOC")) %>%
        dplyr::mutate(no_loc = nchar(CVE_LOC), no_mun = nchar(CVE_MUN), no_ent = nchar(CVE_ENT)) %>%
        dplyr::select(CVE_ENT, CVE_LOC, CVE_MUN, everything())

    }


if("NOM_ENT" %in% names(this.tibble)){


    code.tibble <- this.tibble %>%
    dplyr::select(-NOM_MUN) %>%
    dplyr::left_join(inegi.cost.comm, by = c("NOM_ENT","NOM_LOC")) %>%
    dplyr::select(CVE_ENT, CVE_LOC, CVE_MUN, everything())

}

    }


  code.tibble.cor <- code.tibble %>%
    dplyr::mutate(NOM_ENT = toupper(stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII"))) %>%
    dplyr::mutate(NOM_MUN = toupper(stringi::stri_trans_general(str = NOM_MUN, id = "Latin-ASCII"))) %>%
    dplyr::mutate(NOM_LOC = toupper(stringi::stri_trans_general(str = NOM_LOC, id = "Latin-ASCII")))

  return(code.tibble.cor)
}
