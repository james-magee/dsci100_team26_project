


#
# READ PLAYER STATISTICS FOR YEAR (year) AND ADD RESULTING TABLE VERTICALLY TO (df)
#
add_year <- function (df, year) {

    html <- read_html(sprintf("https://www.basketball-reference.com/leagues/NBA_2020_totals.html", year))

    plyr_table <- html |> 
        html_node("table") |>
        html_table()

    plyr_table <- plyr_table |>
        mutate(across(!c("Player", "Tm"), as.numeric))
        drop_na()
    
    
    
    if (is.null(df)) {
        return(plyr_table)
    } else {
        return(rbind(df, plyr_table))
    }
}



#
# READ PLAYER PERFORMANCE STATISTICS FOR RANGE OF YEARS (year_range)
#
get_years <- function (year_range) {
    years_df <- add_year(NULL, year_range[1])
    for (year in tail(year_range, -1)) {
        years_df <- add_year(years_df, year)
        Sys.sleep(5) # necessary to prevent overloading the servers        
    }
    return(years_df)
}



#
# READ ALL PLAYERS WITH LAST NAME (letter) and ADD VERTICALLY TO (df)
#
get_player_letter <- function (df, letter) {
    
    html <- read_html(sprintf("https://www.basketball-reference.com/players/%s/", letter))

    plyr_table <- html |> 
            html_node("table") |>
            html_table()

    plyr_table <- plyr_table |>
        select(Player, Ht, Wt) |>
        filter(Player != "Player") |>
        mutate(Ht = as.numeric(Ht)) |>
        mutate(Wt = as.numeric(Wt)) |>
        drop_na()
    
    
    
    if (is.null(df)) {
        return(plyr_table)
    } else {
        return(rbind(df, plyr_table))
    }
}



#
# READ ALL PLAYER HT, WT, AND NAME 
#
get_all_players <- function () {
    all_players <- get_player_letter(NULL, letters[1]) # start with "a"
    for (letter in tail(letters, -1)) {
        get_player_letter(all_players, letter)
        Sys.sleep(5) # necessary to prevent overloading the servers
    }
    return(all_players)
}