#' Extract RNPA codes from catch files
#'
#' @param this.catchfileno
#' @param catch.files catch files
#' @param coast.selecc selected coastal states
#'
#' @return catch.file.rnp
#' @export
#'
#' @examples
get_rnpa_catch <- function(this.catchfileno, catch.files, coast.selecc, permit.locs.geo, georef.locs.gc){



  this.catchfile <- catch.files[this.catchfileno]
  print(this.catchfile)

  if(grepl("xlsx", this.catchfile)){

    catch.file <- readxl::read_xlsx(this.catchfile)

  } else if(grepl("csv", this.catchfile)){

  catch.file <- data.table::fread(this.catchfile, skip=2)
  }



  if("TIPOAVISO" %in% colnames(catch.file)){

    catch.file.rnp <- catch.file %>%
      keep_when(TIPOAVISO=="MENORES") %>%
      dplyr::rename(NOM_ENT= NOMBREESTADO, NOM_LOC= NOMBRESITIODESEMBARQUE, rnp_code = RNPAUNIDADECONOMICA) %>%
      keep_when(NOM_ENT %in% coast.selecc) %>%
      dplyr::left_join(permit.locs.geo, by = c("rnp_code","NOM_LOC","NOM_ENT"))
  }

  catch.file.rnp <- catch.file %>%
    dplyr::filter(`NOMBRE ESTADO` %in% coast.selecc) %>%
    dplyr::group_by(`RNPA UNIDAD ECONOMICA`,`NOMBRE PRINCIPAL`,`NOMBRE ESPECIE`,`AÑO CORTE`) %>%
    dplyr::summarise(tot_weight_kg=sum(`PESO DESEMBARCADO_KILOGRAMOS`), tot_value_pesos=sum(VALOR_PESOS)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(rnp_code=`RNPA UNIDAD ECONOMICA`, catch_name = `NOMBRE PRINCIPAL`, sp_name = `NOMBRE ESPECIE`, year = `AÑO CORTE`)


  return(catch.file.rnp)
}
