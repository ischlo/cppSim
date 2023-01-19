#  loading data
# spatial file of partition (administrative units)
# this file contains polygons of administrative units, but also various from that will be used for routing.

# Constructing network

# parameters:
# edges,nodes, simple
# cppr stuff
# graph


# Routing (origins and destinations based on the from)
# which from to route as origin and destination
# params : centroid
# available options: geom_centr, pw_centr, graph_centr, ew_centr (employment weighted centr)
# finding the nearest node in the network to the centroid.
# geom_centr_node
# pw_centr_node
# graph_centr_node

# grav model
# cost function: exp, pow, distance matrice
# flows:
# function sketch :
# https://cloud.r-project.org/doc/manuals/r-release/R-lang.html read for inspiration
#
# flows_matrix <- flows_mat
#
# region_data <- city
#
# run_name <- "test_from_to"

#@import tmap
#@import tmaptools
#@import tidyverse
#@import data.table
#@import cppRouting
#@import foreach foreach
#@import doParallel
#@import parallel
#@importFrom units set_units
#@import reshape2
#@importFrom rlist list.load list.save
#@importFrom sf st_geometry_type st_as_sf st_distance st_area
#@import here here
#@import parallel
#NULL


#'@title
#'Running a whole simulation of a doubly constrained gravity model
#'@description
#'this script takes a network, a flows data, and constructs everything from scratch to run a model based on some intput parameters.
#'
#'@param flows_matrix a integer matrix of flows
#'@param region_data a Sf data.frame containing at least a column which stores as well known text (wkt) the points of origin
#'@param run_name a name for the simulation
#'@param graph a cppRouting generated graph.
#'@param from the name of the culomn with origin points
#'@param to the name of the column with destination points, if same as from, set to NULL
#'@param graph_dist_matrix a network distance matrix in case it was already computed
#'@param beta_offset an offset from 0 from which to start looking for the best fit value.
#'@param dist_threshold a distance threshold to use.
#'@param norm kind of reference distance to use, 1 for manhattan, 2 for euclidean
#'@param time to convert distance to time, set to TRUE for time.
#'@param n_cores number of threads to run the model
#'@param cost_fun type of cost function
#'
#'@returns creates a folder based on the run_name parameter to which images and files are written.
#' The file run_name_best_fit.rds contain the matrices with values from the model
#', and the quality of fit values for the beta values.
#'@export
# @examples
#  merge city data and flows to assign ids from to to the flows.
#
#  graph <- london_graph_simple
#
#  region_data <- london_msoa[grep("Camden",geo_name),]
#
#  flows_london_sample <- flows_london[(workplace %in% region_data$geo_code) &
#                                        (residence %in% region_data$geo_code),]
#
#  flows_matrix <- foreach(i = 1:length(unique(region_data[,id]))
#                          ,.combine = rbind
#                          ,.final = as.matrix) %do%
#    {
#      x <- rep_len(0,nrow(region_data))
#      d <- flows_london_sample[from_id == (i + 160),.(bike,to_id)]
#      x[d[,to_id]%% 160 ] <- d[,bike]
#      x
#    }
#
#  # view(flows_matrix)
#
#  run_name <- "osm_unfilt"
#  from <-  "centr_geom"
#
#  profvis({
#    # test of the function is successful.
#    registerDoParallel(cores = 3)
#    test_simu <- simulation(london_graph_simple
#                            ,flows_matrix = flows_matrix
#                            ,region_data = region_data
#                            ,run_name = run_name
#                            ,from = from)
#    stopImplicitCluster()
#
#  })
simulation <- function(flows_matrix
                       ,region_data
                       ,run_name
                       ,graph = NULL
                       ,from = NULL
                       ,to = NULL
                       ,graph_dist_matrix = NULL
                       ,beta_offset = 0
                       ,dist_threshold = 15000
                       ,norm = 2
                       ,time = FALSE
                       ,n_cores = 3
                       ,cost_fun = "exp") {
  # run name is a prefix to include in the name of saved files

  # checking data inputs

  if(!is.null(flows_matrix) & !is.matrix(flows_matrix)) { stop("provide a flows matrix with integer values")}

  # if(!is.null(region_data) & !("sf" %in% class(region_data)) &
  #    !("POINT" %in% as.character(st_geometry_type(region_data, by_geometry = FALSE)))) { stop("provide sf data frame of spatial points") }

  if (!(from %in% colnames(region_data))) return(print("error, provide a existing column with point geometries for otigins"))

  if(is_null(to)) {to <- from} else if ( !(to %in% colnames(region_data))) return(print("error, provide a existing column with point geometries for destinations"))

  registerDoParallel(cores = n_cores)

  tryCatch({
    if (time == TRUE) {
      dir.create(paste0(run_name,"_time"))
      directory <- paste0(run_name,"_time")
    } else {
      dir.create(run_name)
      directory <- run_name
    }

  },error = function(e) print(e))

  print("created directory")

  ## Convert to cpp

  if (norm == 1) {
    centroid_distance <- norm_p(region_data[,..from] %>% st_as_sf(wkt = from, crs = 4326)
                                ,region_data[,..to] %>% st_as_sf(wkt = to, crs = 4326)
                                ,elementwise = FALSE
    ) %>%
      set_units(NULL) %>%
      round()
  } else if (norm == 2) {
    centroid_distance <- region_data[,..from] %>%
      st_as_sf(wkt = from, crs = 4326) %>%
      st_distance(region_data[,..to] %>% st_as_sf(wkt = to, crs = 4326)
                  ,by_element = FALSE) %>%
      set_units(NULL)  %>%
      round()
  }

  d <- region_data %>% st_as_sf(wkt = "geometry",crs = 4326) %>% st_area() %>% sqrt() %>% set_units(NULL)

  centroid_distance <- `diag<-`(centroid_distance
                                ,d)

  print("distance matrix computed")

  # if(is.null(graph) & is.null(graph_dist_matrix)) {
  #   stop("provide either a graph on which to compute shortest paths, or a matrix of distances")
  #   }

  if(!is.null(graph) | !is.null(graph_dist_matrix)) {

    if(is.null(graph_dist_matrix)) {


      # convert to cpp (?)
      centr_node_id_from <- paste0(from,"_id")
      region_data[,centr_node_id_from] <- find_nearest_node_on_graph(graph = graph
                                                                     ,region_data[,..from] %>% st_as_sf(wkt = from, crs = 4326)

      )

      # convert to cpp (?)
      if(!is_null(to)) {
        if((from != to)) {
          centr_node_id_to <- paste0(to,"_id")
          region_data[,centr_node_id_to] <- find_nearest_node_on_graph(graph = graph
                                                                       ,region_data[,..to] %>% st_as_sf(wkt = to, crs = 4326)

          )
        }
        else if (from == to) {
          centr_node_id_to <- centr_node_id_from
        }

      }
      print("nearest nodes found")


      graph_dist_matrix <- get_distance_matrix(Graph = graph
                                               ,from = region_data[,..centr_node_id_from][[1]]
                                               ,to = region_data[,..centr_node_id_to][[1]]
                                               ,allcores = TRUE) %>%
        round()
    }

    # graph_dist_matrix <- `diag<-`(graph_dist_matrix, d)

    # graph_dist_matrix %>% list.save(paste0(directory,"/","graph_dist_matrix",".rds"))
    #
    # print("computed and saved distance matrix")


    # add as parameter this threshold (?)
    links_of_interest <- which(centroid_distance < dist_threshold, arr.ind = TRUE)

    # links_of_interest %>% list.save("links_of_interest.rds")

    distances_graph <- graph_dist_matrix[links_of_interest] %>% unlist

    print("computed pair distances of paths of interest")

    centr_dist <- centroid_distance[links_of_interest] %>% unlist

    print("computed pair distances between from")

    # x <- sample(1:nrow(links_of_interest),3000)

    jpeg(paste0(directory,"/",run_name,"_dists_all.jpg")
         ,height = 5.83
         ,width = 8.27
         ,quality = 80
         ,units = "in"
         ,res = 150)

    smoothScatter(centr_dist#[x]
                  ,distances_graph#[x]
                  # ,col = "navyblue"
                  # ,pch = 20
                  # #,log = "y"
                  # ,cex = 0.3
                  # ,cex.lab = 1.2
                  ,main = "network and crowfly distance comparison, London"
                  ,xlab = "crow-fly distance, m"
                  ,ylab = "network shortest distance, m")
    # lines(1:max(centr_dist)
    #       ,1:max(centr_dist)*sqrt(2)
    #       ,col = "darkorange"
    #       ,lwd = 2)

    lines(1:max(centr_dist)
          ,1:max(centr_dist)
          ,col = "darkred"
          ,lwd = 2)
    # lines(centr_dist
    #       ,dist_model$fitted.values
    #       ,col = "darkgreen")
    # legend(x = "bottomright"
    #        #,y = 1000
    #        ,legend = c(
    #          # "f(x) = 1.414x"
    #          "f(x) = x"
    #          # ,"f(x) = 1.236x"
    #        )
    #        ,cex = 1.3
    #        ,col = c(
    #          # "orange"
    #          "darkred"
    #          # ,"darkgreen"
    #        )
    #        ,lwd = 2)
    dev.off()
    #### Mapping the difficultly accessible locations across london based on the network ####
    try({
      outliers_of_interest <- which((distances_graph/centr_dist)>sqrt(2))

      #### interlude to check consistency :
      ####
      # outliers_of_interest <- which((distances_graph/centr_dist)<1)
      #
      # (distances_graph/centr_dist) %>% hist()
      #
      # tmap_mode("view")
      #
      # region_data[links_of_interest[outliers_of_interest,1],"geometry"] %>% qtm() +
      #   (graph$coords[graph$coords$node_id %in% region_data[links_of_interest[outliers_of_interest,2],"geometry_id"][[1]]] %>%
      #      st_as_sf(coords = c("X","Y")
      #               ,crs=4326) %>% qtm(dots.col = "red"))

      node_outliers <-
        region_data[links_of_interest[outliers_of_interest,1],..centr_node_id_to][
          ,.(acces_diff=.N/nrow(region_data)),by = centr_node_id_to]

      # node_outliers <-
      #   region_data[links_of_interest[outliers_of_interest,1],..centr_node_id_to] %>%
      #   group_by({{centr_node_id_to}}) %>%
      #   mutate(acces_diff=dplyr::n()/nrow(region_data))

      cols <- c("geo_code","geo_name","geometry",centr_node_id_to)
      node_outliers <- merge(region_data[,..cols]
                             ,node_outliers
                             ,by = centr_node_id_to
                             ,all = TRUE)

      access_difficulty_map <- node_outliers %>%
        st_as_sf(crs = 4326, wkt = "geometry") %>%
        tm_shape() + tm_polygons(col = "acces_diff"
                                 ,size = .3
                                 ,palette = "viridis"
                                 ,style = "pretty"
                                 ,border.col = "black"
                                 ,title = "Access difficulty"
                                 #,contrast = c(.2,1)
                                 ,colorNA = "dimgray") +
        tm_layout(main.title = "Difficulty to access"
                  ,bg.color = "white"
                  ,legend.outside = TRUE

        )
      access_difficulty_map %>% tmap_save(paste0(directory,"/",run_name,"_access_difficulty_map.pdf"))
    }
    ,silent = FALSE)

  } else if(is.null(graph) & is.null(graph_dist_matrix)) { graph_dist_matrix <- centroid_distance }

  #### GRAVITY MODEL STUFF

  # key values : for an exponential cost function,
  # the range of beta values to look at is (0,2) with a best fit around  0.025 is the smaller study area
  # for time in the cost function,

  # ameliorate the following part
  # options : converging which, for with specified values

  if (time) {
    # converting distance to time, rough average with speed of 14 km/h, time in hours.
    graph_dist_matrix <- graph_dist_matrix/(1000*14)
    range_i <- 0:120
  } else {
    # converting distances to distance
    range_i <- 1:30
    graph_dist_matrix <- graph_dist_matrix/1000
  }

  # cpp convert
  beta_calib <- foreach::foreach(i = range_i
                                 ,.combine = rbind) %dopar% {
                                   beta <- (0.03*i)+beta_offset
                                   print(paste0("RUNNING MODEL FOR beta = ",beta))
                                   model_run <- run_model_cpp(flows = flows_matrix
                                                          ,distance = graph_dist_matrix
                                                          ,beta = beta
                                                          ,type = "exp"
                                   )

                                   cbind(beta, model_run$r2,model_run$e_sor)
                                 }

  # selecting the best beta
  beta_best_fit <- beta_calib[which.max(beta_calib[,2]),1] %>% as.double()

  jpeg(paste0(directory,"/",run_name,"beta_calib.jpg")
       ,height = 5.83
       ,width = 5.83
       ,quality = 80
       ,units = "in"
       ,res = 150)

  plot(beta_calib[,1]
       ,beta_calib[,3]
       ,xlab = "beta value"
       ,ylim = c(.2,1)
       ,ylab = "quality of fit"
       ,main = "influence of beta on the goodness of fit"
       ,pch = 19
       ,cex = 0.5
       ,type = "b")

  points(beta_calib[which.max(beta_calib[,2]),1]
         ,beta_calib[which.max(beta_calib[,2]),2]
         ,pch = 15)

  points(beta_calib[which.max(beta_calib[,3]),1]
         ,beta_calib[which.max(beta_calib[,3]),3]
         ,pch = 15)

  lines(beta_calib[,1]
        ,beta_calib[,2]
        ,lwd = 2
        ,col = "darkred")

  legend(x = "bottomleft"
         ,legend = c("Sorensen I","r_2")
         ,pch = c(20,NA)
         ,lwd = c(2,2)
         ,lty = c(8,1)
         ,col = c("black","darkred")
  )
  dev.off()

  # running the fit for the best beta.
  run_best_fit <- run_model_cpp(flows = flows_matrix
                            ,distance = graph_dist_matrix
                            ,beta = beta_best_fit
                            ,type = "exp"
  )

  list("best_fit" = run_best_fit$values
       ,"beta_calib" = beta_calib
  ) %>% list.save(paste0(directory,"/",run_name,"_best_fit.rds"))


  # x <- sample(1:length(run_best_fit$values),3000)
  # plotting
  jpeg(paste0(directory,"/",run_name,"_best_fit.jpg")
       ,height = 5.83
       ,width = 5.83
       ,quality = 80
       ,units = "in"
       ,res = 150)
  plot(
    flows_matrix
    ,run_best_fit$values
    ,ylab = "flows model"
    ,xlab = "flows"
    ,log = "xy"
    ,pch = 19
    ,cex = 0.5
  )
  lines(seq_len(max(run_best_fit$values))
        ,seq_len(max(run_best_fit$values))
        ,col = "darkred"
        ,lwd = 2
  )
  dev.off()

  stopImplicitCluster()
  #
}


#'
#'@title
#'Running a model
#'
#'@title
#'Run model
#'
#'@description
#'This function is the C++ implementation of run_model, it will run a model
#'
#'@param flows A integer matrix of Origin-Destination flows.
#'@param distance a distance matrix between origins and destinations, provide distance in km.
#'@param beta Exponent to use when calculating the cost function.
#'@param type The only type of cost function currently implemented is exponential, parameter value "exp".
#'
#'@returns
#'A list containing an integer matrix with predicted values.
#'
#'@examples
#'
#'data(flows_test)
#'data(distance_test)
#'
#'model_test <- run_model(flows_test,distance_test)
#'
#'@export
run_model <- function(flows
                      ,distance
                      ,beta = 0.25
                      ,type = "exp") {

  if(!is.matrix(flows) &
     !(typeof(flows) %in% c("integer"))) {
    stop("provide a matrix with integers for flows, you can force the data type with 'as.integer(flows)'")
  }

  if(!is.matrix(distance) &
     !(is.numeric(distance))) {
    stop("provide a matrix with numeric values for the distance.")
  }

  if(!(type %in% c("exp","pow"))) {
    stop("the type of cost function is either 'exp' for exponential or 'pow' for power law.")
  }

  if(!is.numeric(beta)) {
    stop("provide a numeric values for the beta parameter.")
    }

  run_model_cpp(flows
                ,distance
                ,beta = 0.25
                ,type = "exp")

}








