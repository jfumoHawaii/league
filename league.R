rm(list = ls())
library(httr)
library(jsonlite)
library(ggplot2)
library(reshape2)
library(echarts4r)
library(htmlwidgets)
library(dplyr)


# Function to retrieve all players using pagination
get_all_hr <- function(season = 2026, limit = 10000) {
  
  offset <- 0
  all_rows <- list()
  
  repeat {
    
    url <- paste0(
      "https://statsapi.mlb.com/api/v1/stats?",
      "stats=season&group=hitting",
      "&season=", season,
      "&playerPool=ALL",
      "&limit=", limit,
      "&offset=", offset
    )
    
    res <- GET(url)
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    
    # Extract splits
    splits <- data$stats$splits[[1]]
    
    # Break loop if no more data
    if (length(splits) == 0) break
    
    all_rows[[length(all_rows) + 1]] <- splits
    
    # Increment offset
    offset <- offset + limit
    
    # Safety break (optional)
    if (offset > 20000) break
  }
  
  # Combine all pages
  df <- do.call(rbind, all_rows)
  
  # Build final dataframe
  hr_df <- data.frame(
    player = df$player$fullName,
    home_runs = df$stat$homeRuns,
    team = df$team$name,
    stringsAsFactors = FALSE
  )
  
  # Sort by HR
  hr_df <- hr_df[order(-hr_df$home_runs), ]
  
  return(hr_df)
}

# Run function
df <- get_all_hr(2026)


########################
stack=read.csv("league.csv")

df$player[df$player=="Ronald Acuña Jr."]="Ronald Acuna Jr."
df$player[df$player=="Teoscar Hernández"]="Teoscar Hernandez"
df$player[df$player=="José Ramírez"]="Jose Ramirez"
df$player[df$player=="Jeremy Peña"]="Jeremy Pena"
df=df[,-3]
df=rbind(df,data.frame(player=unique(unlist(stack[,c(2:10)])[which(unlist(stack[,c(2:10)]) %in% df$player==F)]),home_runs=0))

stackeroo=lapply(c(1:length(stack$Name)),function(name){unlist(lapply(c(1:9),function(x){df$home_runs[which(df$player==as.character(stack[name,c(2:10)][x]))]}))})

another=data.frame(name=stack$Name,tierA=unlist(lapply(c(1:20),function(x){stackeroo[[x]][1]})),tierB=unlist(lapply(c(1:20),function(x){  sum(stackeroo[[x]][2:3])  })), tierC=unlist(lapply(c(1:20),function(x){  sum(stackeroo[[x]][4:length(stackeroo[[x]])])  })))
another=another[order(-rowSums(another[2:4])),]

mat=t(data.matrix(another))
colnames(mat) <- another$name
mat=mat[-1,]
mat=mat[,seq(20,1,-1)]

file_name <- "leagueScores.csv"

# If file doesn't exist, initialize it
if (!file.exists(file_name)) {
  readDF <- data.frame(name = stack$Name)
} else {
  readDF <- read.csv(file_name, stringsAsFactors = FALSE)
}

# Ensure names match current league
if (!"name" %in% colnames(readDF)) {
  readDF$name <- stack$Name
}

# Add today's column
new_col <- paste("ABC", gsub("-", "", Sys.Date()), sep='')

readDF[[new_col]] <- unlist(lapply(1:length(stack$Name), function(x) {
  paste(as.numeric(mat[, which(stack$Name[x] == colnames(mat))]), collapse = "-")
}))

# ✅ ALWAYS overwrite cleanly
write.csv(readDF, file_name, row.names = FALSE)



rownames(readDF)=readDF$name
readDF=readDF[,-1]


ts=data.frame(name=NA,time=NA,A=NA,B=NA,C=NA)
for(i in 1:length(colnames(readDF))){
  ts=rbind(ts,
        data.frame(  
          name=rownames(readDF),
          time=(substr(rep(colnames(readDF)[i],length(rownames(readDF))),4,15)),
          A=sapply(strsplit(readDF[,i],"-"), `[`, 1),
          B=sapply(strsplit(readDF[,i],"-"), `[`, 2),
          C=sapply(strsplit(readDF[,i],"-"), `[`, 3)
        )
  )
  
}
ts=ts[-1,]

ts$sums=rowSums(apply(ts[, 3:5], 2, as.numeric))

ts$time=as.POSIXct(strptime(ts$time,"%Y%m%d"),timezone="Pacific/Honolulu")

avg_df <- ts %>%
  group_by(time) %>%
  summarise(sums = mean(sums, na.rm = TRUE)) %>%
  mutate(name = "Average")

avg_df=as.data.frame(avg_df)
avg_df=data.frame(name="Average",time=avg_df$time,A=0,B=0,C=0,sums=avg_df$sums)


# Combine with original data
ts <- rbind(ts, avg_df)
##############
legend_selection <- as.list(
  setNames(
    rep(FALSE, length(unique(ts$name))),
    unique(ts$name)
  )
)

p_int <- ts %>%
  group_by(name) %>%
  e_charts(time) %>%
  e_line(sums, symbol = "none", smooth = TRUE) %>%
  
  e_tooltip(
    trigger = "axis",
    backgroundColor = "rgba(255,255,255,0.95)",  # white box
    borderColor = "#ccc",
    borderWidth = 1,
    textStyle = list(color = "#333"),
    extraCssText = "box-shadow: 0 2px 8px rgba(0,0,0,0.15);"
  ) %>%
  
  e_legend(
    show = TRUE,
    type = "scroll",
    orient = "vertical",
    right = 10,
    top = "middle",
    selected = legend_selection   # now safe
  ) %>%
  
  e_title("League Scores Over Time") %>%
  e_theme("shine") %>%
  
  e_datazoom(type = "inside") %>%
  e_toolbox_feature(feature = c("dataZoom", "restore"))

# save
saveWidget(p_int, "docs/index.html", selfcontained = FALSE)
