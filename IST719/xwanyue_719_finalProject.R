my.dir <- "~/Documents/窃书不能算偷/SYRACUSE/719 Data Visualization/Data Sets/"
vgames <- read.csv(paste0(my.dir, "vgsales.csv"), header = TRUE, stringsAsFactors = FALSE)
vgames$Year <- as.Date(vgames$Year, "%Y")

# install.packages('portfolio')
library(RColorBrewer)
library(portfolio)
library(ggplot2)
library(ggraph)
library(igraph)
library(tidyverse)
library(treemap)
# library(data.tree)
library(jsonlite)
library(dplyr)
library(hrbrthemes)
# install.packages("hrbrthemes")

# lollipop plot
publish.global.sales <- vgames %>% group_by(Publisher) %>% summarise(count = n()) %>% arrange(desc(count))
top15.publish.global.sales <- publish.global.sales[c(1:10),]

top15.publish.global.sales %>%
  ggplot(aes(x=Publisher, y=count)) +
  geom_segment(aes(x=Publisher, xend=Publisher, y=0, yend=count), color="grey") +
  geom_point(size=3, color="#69b3a2") +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("")

write.csv(top15.publish.global.sales, "top15.publish.global.sales.csv")

empty_bar=11
to_add = matrix(NA, empty_bar, ncol(top15.publish.global.sales))
colnames(to_add) = colnames(top15.publish.global.sales)
top15.publish.global.sales=rbind(top15.publish.global.sales, to_add)
# top15.publish.global.sales$id=seq(1, 35)
top15.publish.global.sales$id <- c(1:23)

label_tmp=top15.publish.global.sales
label_tmp <- label_tmp[c(1:23),]
number_of_bar=nrow(label_tmp)
angle= 90 - 360 * (label_tmp$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_tmp$hjust<-ifelse( angle < -90, 1, 0)
label_tmp$angle<-ifelse(angle < -90, angle+180, angle)
label_tmp$Publisher <- paste(label_tmp$Publisher, " (", label_tmp$global,")", sep="")

ggplot(top15.publish.global.sales, aes(x=as.factor(id), y=global+500)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", fill=alpha("#69b3a2", 0.8)) +
  ylim(-7000,13000) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar(start = 0) + 
  geom_text(data=label_tmp, aes(x=id, y=global+200, label=Publisher), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_tmp$angle, hjust=label_tmp$hjust, inherit.aes = FALSE ) +
  geom_text( aes(x=24, y=8000, label="Who sells more weapons?"), color="black", inherit.aes = FALSE)


#
ggplot(top15.publish.global.sales,aes(Publisher,global,fill=Publisher))+
  geom_bar(width=1,stat="identity")+
  theme_minimal() +
  coord_polar() + xlab("")+ylab("")
  # theme(legend.position = "none" , axis.text.y = element_blank() ,
        # axis.ticks = element_blank())

# which producter has been rated positively?
colnames(vgames)
table(vgames$Platform)
table(vgames$Genre)
range(vgames$Year)
table(vgames$Publisher)

publish.global.sales <- vgames %>% group_by(Publisher) %>% summarise(count = n(), global = sum(Global_Sales), portion = (global/8920.44)*100) %>% arrange(desc(count))
vgames %>% filter(Publisher == 'Sony Computer Entertainment') %>% select(Name)

sum(vgames$Global_Sales)
# Basic Donut Chart
colors <- c("#fad4d4", "#f5b5b5", "#fc9d9d","#ea728c", "#bd4482", "#963366")

par(mfrow = c(2,2))
agg.Region <- data.frame(
  region=c("North America", "Europe", "Japan", "Others"),
  sales=c(sum(vgames$NA_Sales), sum(vgames$EU_Sales), sum(vgames$JP_Sales), Other = sum(vgames$Other_Sales))
)
agg.Region$fraction <- agg.Region$sales/sum(agg.Region$sales)
agg.Region$ymax = cumsum(agg.Region$fraction)
agg.Region$ymin = c(0, head(agg.Region$ymax, n=-1))
agg.Region$labelPosition <- (agg.Region$ymax + agg.Region$ymin) / 2
agg.Region$label <- paste0(agg.Region$region, ": ", agg.Region$sales)
ggplot(agg.Region, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=region)) +
  geom_rect() +
  scale_fill_manual(values=colors) +
  geom_text( x=2, aes(y=labelPosition, label=label), size=3.5) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")

agg.Genre <- as.data.frame(vgames %>%
                             group_by(Genre) %>%
                             summarise(Count = n(), 
                                       Global_Sales = sum(Global_Sales),
                                       fraction = round(Global_Sales/sum(vgames$Global_Sales), 4) * 100))
agg.Genre$ymax = cumsum(agg.Genre$fraction)
agg.Genre$ymin = c(0, head(agg.Genre$ymax, n=-1))
agg.Genre$labelPosition <- (agg.Genre$ymax + agg.Genre$ymin) / 2
agg.Genre$label <- paste0(agg.Genre$Genre, ": ", agg.Genre$fraction)
ggplot(agg.Genre, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Genre)) +
  geom_rect() +
  scale_fill_manual(values=colors) +
  geom_text( x=2, aes(y=labelPosition, label=label), size=2) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")

agg.platform <- as.data.frame(vgames %>%
                             group_by(Platform) %>%
                             summarise(Count = n(), 
                                       Global_Sales = sum(Global_Sales),
                                       fraction = round(Global_Sales/sum(vgames$Global_Sales), 4) * 100)) %>%
  arrange(desc(Global_Sales))
agg.platform.main <- agg.platform[agg.platform$Global_Sales > 300,]
agg.platform.less <- agg.platform[agg.platform$Global_Sales < 300,]
agg.platform.main <- rbind(agg.platform.main, c('Other', sum(agg.platform.less$Count), sum(agg.platform.less$Global_Sales), sum(agg.platform.less$fraction)))
agg.platform <- agg.platform.main
agg.platform$ymax = cumsum(agg.platform$fraction)
agg.platform$ymin = c(0, head(agg.platform$ymax, n=-1))
agg.platform$labelPosition <- (agg.platform$ymax + agg.platform$ymin) / 2
agg.platform$label <- paste0(agg.platform$Platform, ": ", agg.platform$fraction)
ggplot(agg.platform, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Platform)) +
  geom_rect() +
  # scale_fill_manual(values=colors) +
  geom_text( x=2, aes(y=labelPosition, label=label), size=2.5) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")

agg.publisher <- as.data.frame(vgames %>%
                                group_by(Publisher) %>%
                                summarise(Count = n(), 
                                          Global_Sales = sum(Global_Sales),
                                          fraction = round(Global_Sales/sum(vgames$Global_Sales), 4) * 100)) %>%
  arrange(desc(Global_Sales))
agg.publisher.main <- agg.publisher[agg.publisher$fraction > 2.00,]
agg.publisher.less <- agg.publisher[agg.publisher$fraction < 2.00,]
agg.publisher.main <- rbind(agg.publisher.main, c('Other', sum(agg.publisher.less$Count), sum(agg.publisher.less$Global_Sales), sum(agg.publisher.less$fraction)))
agg.publisher <- agg.publisher.main
agg.publisher$ymax = cumsum(agg.publisher$fraction)
agg.publisher$ymin = c(0, head(agg.publisher$ymax, n=-1))
agg.publisher$labelPosition <- (agg.publisher$ymax + agg.publisher$ymin) / 2
agg.publisher$label <- paste0(agg.publisher$Publisher, ": ", agg.publisher$fraction)
ggplot(agg.publisher, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Publisher)) +
  geom_rect() +
  # scale_fill_manual(values=colors) +
  geom_text( x=2, aes(y=labelPosition, label=label), size=1.5) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")

# what is the dirtibution of the game genre?

agg.global <- aggregate(vgames$Global_Sales, list(genre = vgames$Genre),sum)
colnames(agg.global) <- c("genre", "global")
agg.na <- aggregate(vgames$NA_Sales, list(genre = vgames$Genre),sum)
colnames(agg.na) <- c("genre", "na")
agg.eu <- aggregate(vgames$EU_Sales, list(genre = vgames$Genre),sum)
colnames(agg.eu) <- c("genre", "eu")
agg.jp <- aggregate(vgames$JP_Sales, list(genre = vgames$Genre),sum)
colnames(agg.jp) <- c("genre", "jp")
agg.other <- aggregate(vgames$Other_Sales, list(genre = vgames$Genre),sum)
colnames(agg.other) <- c("genre", "other")
agg.sales.combine <- merge(agg.global, agg.na, by = "genre")
agg.sales.combine <- merge(agg.sales.combine, agg.eu, by = "genre")
agg.sales.combine <- merge(agg.sales.combine, agg.jp, by = "genre")
agg.sales.combine <- merge(agg.sales.combine, agg.other, by = "genre")

group <- rep(agg.global$genre, times =1, each=4)
subgroup <- rep(c("NA","EU","JP","Else"), 12)
values <- c()
i <- 1
for (i in 1:12){
  if (i == 1){
    values <- c(agg.na$na[i])
    values <- c(values, agg.eu$eu[i])
    values <- c(values, agg.jp$jp[i])
    values <- c(values, agg.other$other[i])
  }
  else{
    values <- c(values, agg.na$na[i])
    values <- c(values, agg.eu$eu[i])
    values <- c(values, agg.jp$jp[i])
    values <- c(values, agg.other$other[i])}
}
# global <- rep(agg.global$global,each =4)
data <- data.frame(group,subgroup,values)
field <- c("subgroup", "values")
data$label <- do.call("paste", c(data[field], sep = "-"))
data$cut <- cut(data$values, breaks = c(0,150,300,450,600,750,900))
vgames %>% group_by(Genre) %>% summarise(portion = sum(Global_Sales)/8920.44) %>% arrange(desc(portion))
sum(vgames$Global_Sales)
# data$id <- c(1:48)

par(mar = c(0,0,0,0))

tree <- treemap(data, 
        index=c("group","subgroup"),     
        vSize="values",
        vColor = "cut",
        type="categorical",
        range = c(11.36, 877.83),
        title.legend = "Quantiles",
        position.legend = "bottom",
        fontsize.labels=c(10,12),             
        fontcolor.labels=c("#F7F9FE"),    
        fontface.labels=c(2,1),     
        fontfamily.title = "sans",
        fontfamily.labels= "sans",
        bg.labels=c("transparent"),          
        align.labels=list(c("center", "center"), c("right", "top")),                                 
        overlap.labels= 0.2,                     
        inflate.labels= F,
        border.col=c("white","#F7F9FE"),
        border.lwds=c(3.5,0.5),
        palette= c("#f5b5b5", "#fc9d9d","#ea728c", "#bd4482", "#963366", "#5e2040"),
        title="Units Sold by Different Game Genre",    
        fontsize.title=15,
        algorithm="squarified"
)
mean(data$values)
quantile(data$values)

# install.packages("voronoiTreemap")
library(voronoiTreemap)
data(ExampleGDP)
knitr::kable(head(ExampleGDP,3))
gdp_json <- vt_export_json(vt_input_from_df(ExampleGDP))
vt_d3(gdp_json, legend = TRUE, legend_title = "Continents", seed = 1)

h1 <- c(rep("Total", 4*12))
h2 <- data$group
h3 <- data$subgroup
color <- c(rep("yes", 4*12))
weight <- data$values
codes <- data$subgroup
test <- ExampleGDP[FALSE,]
test <- data.frame(h1,h2,h3,color,weight,codes)
test$color <- as.character(test$color)
test$color[test$h2 == "Action"] <- c(rep("#341A3F",4))
test$color[test$h2 == "Adventure"] <- c(rep("#53235D",4))
test$color[test$h2 == "Fighting"] <- c(rep("#ff2a6d",4))
test$color[test$h2 == "Misc"] <- c(rep("#d1f7ff",4))
test$color[test$h2 == "Platform"] <- c(rep("#73fffe",4))
test$color[test$h2 == "Puzzle"] <- c(rep("#89e3f6",4))
test$color[test$h2 == "Racing"] <- c(rep("#4d9e9b",4))
test$color[test$h2 == "Role-Playing"] <- c(rep("#44786a",4))
test$color[test$h2 == "Shooter"] <- c(rep("#05d9e8",4))
test$color[test$h2 == "Simulation"] <- c(rep("#005678",4))
test$color[test$h2 == "Sports"] <- c(rep("#092047",4))
test$color[test$h2 == "Strategy"] <- c(rep("#b7c1de",4))

test$color[test$weight > 0] <- c("#440a4f")
test$color[test$weight > 12] <- c("#461e78")
test$color[test$weight > 57] <- c("#4f61a1")
test$color[test$weight > 110] <- c("#34bdcc")
test$color[test$weight > 227] <- c("#b2eee6")
test$color[test$weight > 750] <- c("#4afff0")

test$codes <- as.character(test$codes)
pdf(file="saving_plot.pdf")
gdp_json <- vt_export_json(vt_input_from_df(test))
vt_d3(gdp_json, legend = TRUE, 
      legend_title = "Continents", 
      color_border = "white", 
      size_border = "5px", 
      size_circle ="2px",
      color_circle = "black",
      width = 2000,
      height = 2000,
      seed = 66)

library(ggvoronoi)
# install.packages("ggvoronoi")


bg <- "#01012b"
rep("#05d9e8","#005678","#d1f7ff","#4d9e9b","#44786a","#2f404d","#85ebd9","#003062","#7a04eb","#6287f8","#7700a6","#3d43b4")

# Stacked Bar Plot
vgame <- vgames
vgame <-vgame %>% 
  filter(Genre == "Action" | Genre == "Sports" | Genre == "Shooter" | Genre == "Role-Playing" | Genre == "Platform" | Genre == "Misc")
# agg.platform <- as.data.frame(vgame %>%
#                                 group_by(Platform, Genre) %>%
#                                 summarise(Count = n()))
agg.platform <- as.data.frame(vgame %>%
                                      group_by(Platform, Genre) %>%
                                      summarise(Sales = sum(Global_Sales)))

# agg.platform <- merge(agg.platform, agg.platform.sales, by = c('Platform',"Genre"))

platform.stat <- as.data.frame(vgame %>%
                                  group_by(Platform) %>%
                                  summarise(Sales = sum(Global_Sales),
                                            Proportion = paste0(round(Sales/sum(vgame$Global_Sales), 4) * 100, "%")))

platform.stat <- platform.stat[order(platform.stat$Sales,decreasing = TRUE),]


# ("#440a4f", "#461e78", "#4f61a1", "#34bdcc", "#b2eee6", "#4afff0")

# colors <- c("#fad4d4", "#f5b5b5", "#fc9d9d","#ea728c", "#bd4482", "#963366", "#5e2040")
colors <- c("#4afff0", "#b2eee6", "#34bdcc", "#4f61a1", "#461e78", "#440a4f")
ggplot(agg.platform) +
  geom_bar(aes(x = reorder(Platform, -Sales), y = Sales, fill = Genre), position="stack", stat = "identity") +
  scale_fill_manual(values=colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title = element_text(size = 12), 
        axis.text = element_text(size = 7, face = "bold"), 
        legend.title = element_text(face = "bold", size = 8)) + 
  geom_text(data=platform.stat,aes(x=Platform,y=Sales,label=Sales),vjust=0, size =2.3) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,1100, 150)) +
  labs(title='Global Sales of Top 6 Genres in Different Platform', 
       y="Global Sales", fill="Genre", x = "Platform")

small.portion <- agg.platform[agg.platform$Platform == 'SCD'| agg.platform$Platform == "NG" | agg.platform$Platform == "WS" | agg.platform$Platform == "TG16" | agg.platform$Platform == "3DO" | agg.platform$Platform == "GG"| agg.platform$Platform == "PCFX",]
small.portion.stat <- platform.stat[platform.stat$Platform == 'SCD'| platform.stat$Platform == "NG" | platform.stat$Platform == "WS" | platform.stat$Platform == "TG16" | platform.stat$Platform == "3DO" | platform.stat$Platform == "GG"| platform.stat$Platform == "PCFX",]
ggplot(small.portion) +
  geom_bar(aes(x = reorder(Platform, -Sales), y = Sales, fill = Genre), position="stack", stat = "identity") +
  scale_fill_manual(values=colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title = element_text(size = 12), 
        axis.text = element_text(size = 7, face = "bold"), 
        legend.title = element_text(face = "bold", size = 8)) + 
  geom_text(data=small.portion.stat,aes(x=Platform,y=Sales,label=Sales),vjust=0, size =2.5) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,1300, 150))
  

# what is the times series graph for sales
colors <- c("#4afff0", "#b2eee6", "#34bdcc", "#4f61a1", "#461e78", "#440a4f")
colnames(vgames)
agg.Region <- as.data.frame(vgames %>%
                              group_by(Year) %>%
                              summarise(count= n(),
                                        North_America = sum(NA_Sales),
                                        EU = sum(EU_Sales),
                                        JP = sum(JP_Sales),
                                        Other = sum(Other_Sales),
                                        Global = sum(Global_Sales)))
agg.Region <- agg.Region[c(1:39),]
agg.Region$Year <- substr(as.character(agg.Region$Year),1,4)
agg.Region$Year <- as.numeric(agg.Region$Year)
ggplot(agg.Region) +
  geom_line(aes(x=Year, y=Global), color = "#4afff0", size = 0.6) +
  geom_area(fill="#4afff0", alpha=0.5, aes(x=Year, y=Global)) +
  geom_point(size=0.8, color="#4afff0", aes(x=Year, y=Global)) +
  
  geom_line(aes(x=Year, y=North_America), color = "#34bdcc", size = 0.6) +
  geom_area(fill="#34bdcc", alpha=0.4, aes(x=Year, y=North_America)) +
  geom_point(size=0.8, color="#34bdcc", aes(x=Year, y=North_America)) +
  
  geom_line(aes(x=Year, y=EU), color = "#4f61a1", size = 0.6) +
  geom_area(fill="#4f61a1", alpha=0.3, aes(x=Year, y=EU)) +
  geom_point(size=0.8, color="#4f61a1", aes(x=Year, y=EU)) +
  
  geom_line(aes(x=Year, y=JP), color = "#461e78", size = 0.6) +
  geom_area(fill="#461e78", alpha=0.3, aes(x=Year, y=JP)) +
  geom_point(size=0.8, color="#461e78", aes(x=Year, y=JP)) +
  
  geom_line(aes(x=Year, y=Other), color = "#440a4f", size = 0.6) +
  geom_area(fill="#440a4f", alpha=0.3, aes(x=Year, y=Other)) +
  geom_point(size=0.8, color="#440a4f", aes(x=Year, y=Other)) +
  labs(x = 'Year', y = 'Sales (Millions)', title = "Time Series Plot By Different Region") +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  theme_minimal() +
  scale_x_continuous(breaks = seq(1980, 2018, 5)) +
  scale_y_continuous(breaks = seq(0, 800, 100))
  

# Top 100 video games with publisher based on Year (Bubble Dot Plot) -> Can Make this 3D
library(viridis)
library(ggthemes)
# install.packages("geom_label")
# install.packages("ggthemes")

top_50 <- vgames[order(vgames$Global_Sales, decreasing = TRUE),]
top_50 <- top_50[c(1:100),]

top_50 <- top_50 %>% 
  mutate(annotation = case_when(
    Publisher == "Nintendo" & Global_Sales == 82.74 ~ "yes",
    Publisher == "Microsoft Game Studios" & Global_Sales == 21.82 ~ "yes",
    Publisher == "Take-Two Interactive" & Global_Sales == 21.40 ~ "yes",
    Publisher == "Sony Computer Entertainment" & Global_Sales == 14.98 ~ "yes",
    Publisher == "Activision" & Global_Sales == 14.76 ~ "yes",
    Publisher == "Ubisoft" & Global_Sales == 10.26 ~ "yes",
    Publisher == "Bethesda Softworks" & Global_Sales == 8.84 ~ "yes",
    Publisher == "Electronic Arts" & Global_Sales == 8.49 ~ "yes",
    Publisher == "Sega" & Global_Sales == 8.06 ~ "yes",
    Publisher == "SquareSoft" & Global_Sales == 7.86 ~ "yes",
    Publisher == "Atari" & Global_Sales == 7.81 ~ "yes"))
top_50$annotation[is.na(top_50$annotation)] <- 'no'

colors_top <- c("#fad4d4", "#f5b5b5", "#fc9d9d","#ea728c", "#bd4482", "#963366", "#5e2040","#fad4d4", "#f5b5b5", "#fc9d9d","#ea728c", "#bd4482", "#963366")
top_50 %>%
  arrange(desc(Global_Sales)) %>%
  ggplot(aes(x = -Rank, y=Year, size = Global_Sales, fill = Publisher)) +
  geom_point(alpha=0.6, shape=21, color = "white") +
  scale_size(range = c(5, 21), name="Global Sales") +
  theme(legend.position="bottom") +
  labs(x = "Rank", y = "Year", title = "Top 100 Video Games") +
  scale_fill_manual(values=colors_top) +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 8.5),
        legend.title=element_text(face = "bold", size = 7),
        plot.title = element_text(hjust = 1)) +
  scale_x_continuous(breaks = seq(0,100, 10)) +
  ggrepel::geom_text_repel(data=top_50 %>% filter(annotation=="yes"), aes(label=Name, color = Publisher), size=3,
                           box.padding = unit(1, "lines"),
                           point.padding = unit(.5, "lines"))

# 3D
colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')
library(plotly)
fig <- plot_ly(top_50, x = ~Rank, y = ~Year, z = ~Global_Sales, color = ~Publisher, 
               size = ~Global_Sales, colors = colors,
               marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 150),
               text = ~paste('Publisher:', Publisher, '<br>Year:', Year, '<br>Rank:', Rank,
                             '<br>Global_Sales.:', Global_Sales))

fig <- fig %>% layout(title = 'Top 100 Video Games',
                      scene = list(xaxis = list(title = 'Rank',
                                                gridcolor = 'rgb(255, 255, 255)',
                                                range = c(0,100)),
                                   yaxis = list(title = 'Year',
                                                gridcolor = 'rgb(255, 255, 255)',
                                                range = range(top_100$Year),
                                                ticklen = 5),
                                   zaxis = list(title = 'Global Sales',
                                                gridcolor = 'rgb(255, 255, 255)',
                                                ticklen = 5)),
                      paper_bgcolor = 'rgb(243, 243, 243)',
                      plot_bgcolor = 'rgb(243, 243, 243)')

fig

