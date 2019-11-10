
#' Merge edges for list of nodes with community membership
#'
#' @param flow edge list
#' @param com community membership list
#' @param isolates list of isolate postcodes
#'
#' @return merged edge list
#' @export
merge_flow <- function(flow, com, isolates) {
  flow %>%
    filter(!source %in% isolates$postcode & !sink %in% isolates$postcode) %>%
    left_join(com, by = c("source" = "name")) %>%
    rename("sourceCom" = "community") %>%
    left_join(com, by = c("sink" = "name")) %>%
    rename("sinkCom" = "community") %>%
    mutate(source = pmin(sourceCom, sinkCom)) %>%
    mutate(sink = pmax(sourceCom, sinkCom)) %>%
    select(-sourceCom, -sinkCom) %>%
    filter(source != sink) %>%
    group_by(source, sink) %>%
    summarise(count = sum(count), weight = sum(weight)) %>%
    ungroup() %>%
    filter(count > 10)
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

#' Summary stats per (merged) community
#'
#' @param com community identifier
#' @param totalFlow edge list
#' @param isolates list of isolate postcodes
#'
#' @return df with stats per community
#' @export
com_statsPerCom <- function(com, totalFlow, isolates) {
  internal <- totalFlow %>%
    filter(!source %in% isolates$postcode & !sink %in% isolates$postcode) %>%
    left_join(com, by = c("source" = "name")) %>%
    rename("sourceCom" = "community") %>%
    left_join(com, by = c("sink" = "name")) %>%
    rename("sinkCom" = "community") %>%
    filter(sourceCom == sinkCom) %>%
    group_by(sourceCom) %>%
    summarise(internal = sum(weight)) %>%
    rename("com" = "sourceCom")

  outgoing <- totalFlow %>%
    filter(!source %in% isolates$postcode & !sink %in% isolates$postcode) %>%
    left_join(com, by = c("source" = "name")) %>%
    rename("sourceCom" = "community") %>%
    left_join(com, by = c("sink" = "name")) %>%
    rename("sinkCom" = "community") %>%
    filter(sourceCom != sinkCom) %>%
    group_by(sourceCom) %>%
    summarise(outgoing = sum(weight)) %>%
    rename("com" = "sourceCom")

  incoming <- totalFlow %>%
    filter(!source %in% isolates$postcode & !sink %in% isolates$postcode) %>%
    left_join(com, by = c("source" = "name")) %>%
    rename("sourceCom" = "community") %>%
    left_join(com, by = c("sink" = "name")) %>%
    rename("sinkCom" = "community") %>%
    filter(sourceCom != sinkCom) %>%
    group_by(sinkCom) %>%
    summarise(incoming = sum(weight)) %>%
    rename("com" = "sinkCom")

  full_join(internal, outgoing) %>%
    full_join(incoming) %>%
    replace_na(list(incoming = 0, outgoing = 0)) %>%
    mutate(outgoingRel = outgoing / (outgoing + internal))
}
