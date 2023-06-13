#' @title ingest_into_database
#' @description used to add models from a source folder to a destination catalog
#' @param path_to_ras_dbase PARAM_DESCRIPTION
#' @param top_of_dir_to_scrape PARAM_DESCRIPTION
#' @param code_to_place_in_source PARAM_DESCRIPTION
#' @param proj_overwrite PARAM_DESCRIPTION, Default: NULL
#' @param vdat_trans PARAM_DESCRIPTION, Default: TRUE
#' @param quiet PARAM_DESCRIPTION, Default: FALSE
#' @param overwrite PARAM_DESCRIPTION, Default: FALSE
#' @param refresh PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  [as.data.table][data.table::as.data.table], [data.table][data.table::data.table], [rbindlist][data.table::rbindlist], [fwrite][data.table::fwrite]
#'  [glue][glue::glue]
#'  [glob2rx][utils::glob2rx]
#'  [st_crs][sf::st_crs], [st_set_crs][sf::st_set_crs], [st_coordinates][sf::st_coordinates], [st_as_sf][sf::st_as_sf], [st_transform][sf::st_transform], [st_write][sf::st_write]
#'  [str_sub][stringr::str_sub], [str_detect][stringr::str_detect]
#'  [unglue_vec][unglue::unglue_vec]
#'  [sf_linestring][sfheaders::sf_linestring]
#'  [st_endpoint][lwgeom::st_endpoint], [st_startpoint][lwgeom::st_startpoint]
#'  [group_by][dplyr::group_by], [distinct][dplyr::distinct]
#'  [get_nhdplus][nhdplusTools::get_nhdplus]
#'  [aoi_get][AOI::aoi_get]
#'  [write_parquet][arrow::write_parquet]
#' @rdname ingest_into_database
#' @export
#' @import magrittr
#' @import data.table
#' @importFrom glue glue
#' @importFrom utils glob2rx
#' @importFrom sf st_crs st_set_crs st_coordinates st_as_sf st_transform st_write
#' @importFrom stringr str_sub str_detect
#' @importFrom unglue unglue_vec
#' @importFrom sfheaders sf_linestring
#' @importFrom lwgeom st_endpoint st_startpoint
#' @importFrom dplyr group_by distinct
#' @importFrom nhdplusTools get_nhdplus
#' @importFrom AOI aoi_get
#' @importFrom arrow write_parquet
#' @importFrom data.table as.data.table data.table rbindlist fwrite
#' @importFrom holyhull holyhull

ingest_into_database <- function(path_to_ras_dbase,
                                 top_of_dir_to_scrape,
                                 code_to_place_in_source,
                                 proj_overwrite=NULL,
                                 vdat_trans=FALSE,
                                 quiet=TRUE,
                                 overwrite=FALSE,
                                 refresh=TRUE) {

  ## -- Start --
  fn_time_start <- Sys.time()
  if(!quiet) {
    print(glue::glue("Parsing {top_of_dir_to_scrape} to place in {path_to_ras_dbase}"))
  }

  names <- c("nhdplus_comid","model_name","g_file","last_modified","source","units","crs","inital_scrape_name","final_name_key","notes")

  if(file.exists(file.path(path_to_ras_dbase,"model_catalog.csv",fsep = .Platform$file.sep))) {
    ras_catalog_dbase = load_catalog_csv_as_DT(file.path(path_to_ras_dbase,"model_catalog.csv",fsep = .Platform$file.sep), quiet = quiet)
  } else {
    if (!quiet) { print('Constructing ras database structure') }
    dir.create(file.path(path_to_ras_dbase,"models",fsep = .Platform$file.sep), showWarnings = FALSE)
    dir.create(file.path(path_to_ras_dbase,"models","_unprocessed",fsep = .Platform$file.sep), showWarnings = FALSE, recursive=TRUE)
    ras_catalog_dbase <- data.frame(matrix(ncol = 10, nrow = 0))
    colnames(ras_catalog_dbase) <- names
    ras_catalog_dbase <- data.table::as.data.table(ras_catalog_dbase)
    ras_catalog_dbase[,nhdplus_comid := as.character(nhdplus_comid)]
    ras_catalog_dbase[,final_name_key := as.character(final_name_key)]
  }

  # Find a list of all the .prj files
  list_of_prj_files <- list.files(top_of_dir_to_scrape, pattern = glob2rx("*.prj$"), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  if(!quiet) { print(glue::glue("Found {length(list_of_prj_files)} potential ras files")) }

  # For each prj file:
  for(file in list_of_prj_files) {
    # file <- list_of_prj_files[1]
    dir_of_file <- dirname(file)
    current_model_name <- gsub('.{4}$', '', basename(file))

    if (!quiet) { print(glue::glue("Processing {current_model_name} model")) }

    g_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.g??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    ghdf_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.g??.hdf$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    p_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.p??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    f_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.f??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    h_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.h??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    v_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.v??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    o_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.o??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    r_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.r??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    u_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.u??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    x_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.x??$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    rasmap_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.rasmap$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    prj_files <- list.files(dir_of_file, pattern=utils::glob2rx(glue::glue("{current_model_name}.prj$")), full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
    list_of_files <- c(g_files ,ghdf_files ,p_files ,f_files ,h_files, v_files, prj_files,o_files,r_files,u_files,x_files,rasmap_files)

    if(length(g_files) == 0) {
      if(!quiet) {
        print_warning_block()
        print(glue::glue('No geometry found for model:{file}'))
      }
      next
    }

    current_nhdplus_comid = NA
    current_model_units = NA
    current_model_projection = NA
    current_last_modified = as.integer(as.POSIXct(file.info(file)$mtime))

    for(potential_file in prj_files) {
      # potential_file <- prj_files[1]
      file_text <- read.delim(potential_file, header = FALSE)

      if(any(c('PROJCS','GEOGCS','DATUM','PROJECTION') == file_text)) {
        if(!quiet) { print('found a projection') }
        current_model_projection = sf::st_crs(potential_file)
      } else if(grepl("SI Units", file_text, fixed = TRUE)) {
        current_last_modified = as.integer(as.POSIXct(file.info(potential_file)$mtime))
        current_model_units = "Meter"
      } else if(grepl("English Units", file_text, fixed = TRUE)) {
        current_last_modified = as.integer(as.POSIXct(file.info(potential_file)$mtime))
        current_model_units = "Foot"
      }
    }

    if(is.na(current_model_projection) & !is.null(proj_overwrite)) {
      current_model_projection = proj_overwrite
    }

    for(g_file in g_files) {
      # g_file <- g_files[1]
      current_g_value <- stringr::str_sub(g_file,-3,-1)

      # Do we have enough info to parse the file at this point?
      cond1 = !is.na(current_model_units)
      cond2 = !is.na(current_model_projection)
      cond3 = file.exists(paste0(g_file,".hdf"))

      if(!(cond1 & cond2)) {
        current_inital_name <- paste0("unknown_",current_model_name,"_",current_g_value,"_",current_last_modified)
        current_final_name_key <- NA

        if(sum(stringr::str_detect(na.omit(ras_catalog_dbase$inital_scrape_name), current_inital_name)) == 0) {
          new_row <- data.table::data.table(current_nhdplus_comid,current_model_name,current_g_value,current_last_modified,code_to_place_in_source,current_model_units,current_model_projection,current_inital_name,current_final_name_key,"unparsed_units_proj")
          names(new_row) <- names
          ras_catalog_dbase <- data.table::rbindlist(list(ras_catalog_dbase,new_row))

          dir.create(file.path(path_to_ras_dbase,"models","_unprocessed",current_inital_name,fsep = .Platform$file.sep), showWarnings = FALSE, recursive = TRUE)
          file.copy(c(g_file ,paste0(g_file,".hdf") ,p_files ,f_files ,h_files, v_files, prj_files,o_files,r_files,u_files,x_files,rasmap_files),
                    file.path(path_to_ras_dbase,"models","_unprocessed",current_inital_name,fsep = .Platform$file.sep))
          data.table::fwrite(ras_catalog_dbase,file.path(path_to_ras_dbase,"model_catalog.csv",fsep = .Platform$file.sep), row.names = FALSE)
        } else {
          if(!quiet) { print("Model with inital scrape name already in the que") }
        }
        next()
      }

      g_pts <- list()
      g_pts[[1]] <- data.frame()
      g_pts = process_ras_g_to_xyz(
        geom_path=g_file,
        units=current_model_units,
        proj_string=current_model_projection,
        in_epoch_override=current_last_modified,
        vdat = vdat_trans,
        quiet=FALSE)
      if(nrow(g_pts[[1]]) > 0) {
        if(vdat_trans) {
          g_ptserr <- unglue::unglue_vec(g_pts[[2]], "{}:{}:{x}") %>% as.numeric()
        } else {
          g_ptserr <- unglue::unglue_vec(g_pts[[2]],"{}:{x}") %>% as.numeric()
        }
      }

      ghdf_pts <- list()
      ghdf_pts[[1]] <- data.frame()
      if(cond3) {
        ghdf_pts = process_ras_hdf_to_xyz(
          geom_path=paste0(g_file,".hdf"),
          units=current_model_units,
          proj_string=current_model_projection,
          in_epoch_override=current_last_modified,
          vdat = vdat_trans,
          quiet=FALSE)
        if(nrow(ghdf_pts[[1]]) > 0) {
          if(vdat_trans) {
            ghdf_ptserr <- unglue::unglue_vec(ghdf_pts[[2]], "{}:{}:{x}") %>% as.numeric()
          } else {
            ghdf_ptserr <- unglue::unglue_vec(ghdf_pts[[2]],"{}:{x}") %>% as.numeric()
          }
        }
      }

      cond4 = nrow(g_pts[[1]]) > 0
      cond5 = nrow(ghdf_pts[[1]]) > 0

      if(cond4 & cond5) {
        if(abs(ghdf_ptserr) > abs(g_ptserr)) {
          extrated_pts <- g_pts
          extrated_pts[[2]] <- paste(g_pts[[2]],"* G parsed")
        } else {
          extrated_pts <- ghdf_pts
          extrated_pts[[2]] <- paste(ghdf_pts[[2]],"* GHDF parsed")
        }
      } else if(cond4) {
        extrated_pts <- g_pts
      } else if(cond5) {
        extrated_pts <- ghdf_pts
      } else if(!cond4 & !cond5) {
        current_inital_name <- paste0("unknown_",current_model_name,"_",current_g_value,"_",current_last_modified)
        current_final_name_key <- NA

        if(sum(stringr::str_detect(na.omit(ras_catalog_dbase$inital_scrape_name), current_inital_name)) == 0) {
          new_row <- data.table::data.table(current_nhdplus_comid,current_model_name,current_g_value,current_last_modified,code_to_place_in_source,current_model_units,current_model_projection,current_inital_name,
                                            current_final_name_key,"unparsed_units_proj")
          names(new_row) <- names
          ras_catalog_dbase <- data.table::rbindlist(list(ras_catalog_dbase,new_row))

          dir.create(file.path(path_to_ras_dbase,"models","_unprocessed",current_inital_name,fsep = .Platform$file.sep), showWarnings = FALSE, recursive = TRUE)
          file.copy(c(g_file ,paste0(g_file,".hdf") ,p_files ,f_files ,h_files, v_files, prj_files,o_files,r_files,u_files,x_files,rasmap_files),
                    file.path(path_to_ras_dbase,"models","_unprocessed",current_inital_name,fsep = .Platform$file.sep))
          data.table::fwrite(ras_catalog_dbase,file.path(path_to_ras_dbase,"model_catalog.csv",fsep = .Platform$file.sep), row.names = FALSE)
        } else {
          if(!quiet) { print("Model with inital scrape name already in the que") }
        }
        next()
      }

      ls = sfheaders::sf_linestring(
        obj = extrated_pts[[1]]
        , x = "x"
        , y = "y"
        # , z = "z"
        , linestring_id = "xid"
        , keep = FALSE
      ) |> sf::st_set_crs(sf::st_crs("EPSG:6349"))

      end_points <- c(ls %>% lwgeom::st_endpoint(), ls %>% lwgeom::st_startpoint())
      end_points <- sf::st_coordinates(end_points) %>%
        as.data.frame() %>%
        dplyr::group_by(X,Y) %>%
        dplyr::distinct() %>%
        sf::st_as_sf(coords = c("X","Y")) %>%
        sf::st_set_crs(sf::st_crs("EPSG:6349")) %>%
        sf::st_transform(sf::st_crs("EPSG:4326"))

      ahull_poly = holyhull::holyhull(sf_frame=end_points,method='convave',alpha_value=0.01, concavity = 2, length_threshold = 0)

      # Join to comids
      current_nhdplus_comid = nhdplusTools::get_nhdplus(AOI::aoi_get(ahull_poly),realization = "flowline")
      if(length(current_nhdplus_comid) == 0){
        current_nhdplus_comid = 1
      } else {
        current_nhdplus_comid = current_nhdplus_comid[current_nhdplus_comid$streamorde == max(current_nhdplus_comid$streamorde),][1,]$comid
      }

      current_inital_name = paste0(current_nhdplus_comid,"_",current_model_name,"_",current_g_value,"_",current_last_modified)
      print(glue::glue("Parsed into:{current_inital_name}"))

      current_final_name_key = current_inital_name

      if(sum(stringr::str_detect(na.omit(ras_catalog_dbase$final_name_key), current_final_name_key)) == 0) {
        new_row <- data.table::data.table(current_nhdplus_comid,current_model_name,current_g_value,current_last_modified,code_to_place_in_source,current_model_units,current_model_projection,current_inital_name,
                                          current_final_name_key,as.character(extrated_pts[[2]]))
        names(new_row) <- names
        ras_catalog_dbase <- data.table::rbindlist(list(ras_catalog_dbase,new_row))

        dir.create(file.path(path_to_ras_dbase,"models",current_final_name_key,fsep = .Platform$file.sep), showWarnings = FALSE, recursive = TRUE)
        file.copy(c(g_file ,paste0(g_file,".hdf") ,p_files ,f_files ,h_files, v_files, prj_files,o_files,r_files,rasmap_files),
                  file.path(path_to_ras_dbase,"models",current_final_name_key,fsep = .Platform$file.sep))

        arrow::write_parquet(extrated_pts[[1]],file.path(path_to_ras_dbase,"models",current_final_name_key,"ras_xyz.parquet",fsep = .Platform$file.sep))
        sf::st_write(ahull_poly,file.path(path_to_ras_dbase,"models",current_final_name_key,"hull.fgb",fsep = .Platform$file.sep))
        data.table::fwrite(ras_catalog_dbase,file.path(path_to_ras_dbase,"model_catalog.csv",fsep = .Platform$file.sep), row.names = FALSE)
      } else {
        if(!quiet) { print(glue::glue("Model already processed: {current_final_name_key} at row {ras_catalog_dbase[ras_catalog_dbase$final_name_key == current_final_name_key,which=TRUE]}")) }
      }
    }
    # print(ras_catalog_dbase)
  }

  if(refresh) {
    refresh_master_files(path_to_ras_dbase)
  }

  runtime <- Sys.time() - fn_time_start
  units(runtime) <- "mins"
  print(paste("RAS Library appended in",round(runtime, digits = 2),"minutes"))

  return(TRUE)
}
