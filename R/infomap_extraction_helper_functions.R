#' Extract list of nodes and their assigned communities from Infomap output
#'
#' @param filename path to infomap tree file
#'
#' @return tibble with two columns indicating node name and assigned community
#' @export
com_extract <- function(filename) {
  read_delim(
    filename,
    skip = 2,
    delim = " ",
    col_names = c("path", "flow", "name", "node")
  ) %>%
    separate(path, c("community", "position")) %>%
    select(community, name)
}

#' Count number of members in a single community
#'
#' @param com community identifier
#'
#' @return number of members in a single community
#' @export
com_count <- function(com) {
  com %>%
    group_by(community) %>%
    summarise(count = n()) %>%
    filter(count > 1) %>%
    arrange(-count) %>%
    nrow()
}

#' Calculate the external connections of a community relative to all connections
#'
#' @param com community identifier
#' @param totalFlow initial input network
#' @param isolates df with isolates
#'
#' @return fraction indicating external connections relative to all connections of a community
#' @export
com_external <- function(com, totalFlow, isolates) {
  internal <- totalFlow %>%
    filter(!source %in% isolates$postcode &
             !sink %in% isolates$postcode) %>%
    left_join(com, by = c("source" = "name")) %>%
    rename("sourceCom" = "community") %>%
    left_join(com, by = c("sink" = "name")) %>%
    rename("sinkCom" = "community") %>%
    group_by(sourceCom, sinkCom) %>%
    summarise(weight = sum(weight)) %>%
    mutate(internal = sourceCom == sinkCom) %>%
    group_by(internal) %>%
    summarise(weight = sum(weight))
  totalWeight <- sum(internal$weight)
  external <- internal %>%
    filter(!internal)
  external$weight / totalWeight
}

#' Extract the
#'
#' @param com
#' @param totalFlow
#' @param isolates
#'
#' @return
#' @export
#'
#' @examples
com_externalMedian <- function(com, totalFlow, isolates) {
  sourceTot <- totalFlow %>%
    filter(!source %in% isolates$postcode &
             !sink %in% isolates$postcode) %>%
    left_join(com, by = c("source" = "name")) %>%
    rename("sourceCom" = "community") %>%
    left_join(com, by = c("sink" = "name")) %>%
    rename("sinkCom" = "community") %>%
    group_by(sourceCom) %>%
    summarise(weight = sum(weight)) %>%
    ungroup()

  totalFlow %>%
    filter(!source %in% isolates$postcode &
             !sink %in% isolates$postcode) %>%
    left_join(com, by = c("source" = "name")) %>%
    rename("sourceCom" = "community") %>%
    left_join(com, by = c("sink" = "name")) %>%
    rename("sinkCom" = "community") %>%
    group_by(sourceCom, sinkCom) %>%
    summarise(weight = sum(weight)) %>%
    mutate(internal = sourceCom == sinkCom) %>%
    group_by(sourceCom, internal) %>%
    summarise(weight = sum(weight)) %>%
    left_join(sourceTot, by = c("sourceCom", "sourceCom")) %>%
    mutate(relWeight = weight.x / weight.y) %>%
    filter(!internal) %>%
    ungroup() %>%
    select(relWeight) %>%
    summarise(median = median(relWeight)) %>%
    .$median
}

#' Merge polygons in a sf object based on community membership
#'
#' @param com community identifier
#' @param sf sf object with all spatial polygons
#' @param index index reflecting position of specific community set in sequential list of markov times
#'
#' @return
#' @export
comm_merge <- function(com, sf, index) {
  sf %>%
    left_join(com, by = c("pc4" = "name")) %>%
    drop_na(community) %>%
    group_by(community) %>%
    summarise() %>%
    mutate(index = index)
}

#' Extract codelength from Infomap output
#'
#' @param filename path to infomap tree file
#'
#' @return codelength of the community configuration found in the tree file
#' @export
codeLength_extract <- function(filename) {
  # the first line of an infomap tree file looks like this
  ## # 'ex.net . -N 30 --undirected --tree --inner-parallelization --two-level --markov-time 0.5 --out-name ex-comm/ex-0.5' -> 3524 nodes partitioned in 19s from codelength 10.865314553 in one level to codelength 5.996218787 in 2 levels.
  l <- read_lines(filename, skip = 0, n_max = 1)
  l %>% str_extract("(?<=to codelength ).*?(?= in 2 levels)")
}
