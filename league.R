rm(list = ls())
library(httr)
library(jsonlite)

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


barplot((mat),horiz = T,
        col = c("dodgerblue","goldenrod","forestgreen"),
        las = 2)  # rotate names if needed
legend("bottomright",legend=as.vector(c("Tier A","Tier B","Tier C")),pch=c(15,15,15),col=c("dodgerblue","goldenrod","forestgreen"))



readDF=read.csv("leagueScores.csv")

readDF[[paste("ABC",gsub("-","",Sys.Date()),sep='')]]=unlist(lapply(c(1:length(stack$Name)),function(x){paste(as.numeric(mat[,which(stack$Name[x]==colnames(mat))]),collapse="-")}))

write.csv(readDF,"leagueScores.csv",row.names = F)


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


library(ggplot2)
library(plotly)
library(htmlwidgets)
library(googledrive)

# plot
p <- ggplot(ts, aes(x = time, y = sums, color = name)) +
  geom_line(linewidth = 1) +
  theme_minimal()

p_int <- ggplotly(p)

# save
file <- "ts_plot.html"
saveWidget(p_int, file, selfcontained = TRUE)
p_int

# upload
#drive_auth()
#
#
#drive_upload(file, overwrite = TRUE)


#######################
library(reshape2)

ts_long <- melt(ts,
                id.vars = c("name", "time"),
                measure.vars = c("A", "B", "C"),
                variable.name = "tier",
                value.name = "value")

ts_long$value <- as.numeric(ts_long$value)

ggplot(ts_long, aes(x = time, y = value, fill = tier)) +
  geom_area(position = "stack") +
  facet_wrap(~name) +
  theme_minimal()

pdf(width=11,height=8.5,"wedges.pdf")
ggplot(ts_long[ts_long$name=="Jimmy" | ts_long$name=="Matt" | ts_long$name=="Cam" | ts_long$name=="JG",], aes(x = time, y = value, fill = tier)) +
  geom_area(position = "stack") +
  facet_wrap(~name) +
  theme_minimal()
dev.off()
dev.off()
######################
mat <- reshape2::dcast(ts, name ~ time, value.var = "sums")
rownames(mat) <- mat$name
mat <- mat[,-1]

d <- dist(mat)
hc <- hclust(d)

plot(hc)
######################
p_smooth=ggplot(ts, aes(x = time, y = sums, color = name)) +
  geom_smooth(se = FALSE, linewidth = 1.2) +
  theme_minimal()

p_smooth <- ggplotly(p_smooth)

# save
file <- "ts_smooth.html"
saveWidget(p_smooth, file, selfcontained = TRUE)

