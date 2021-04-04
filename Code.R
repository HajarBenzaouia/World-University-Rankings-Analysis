#import les packages
library(ggplot2)
library(dplyr)
library(plotly)
library(gridExtra)
library(fmsb)



#Read data
U_Ranking <- read.csv(file.choose())
View(U_Ranking)

#Cela permet de voir chaque colonne dans un bloc de données
glimpse(U_Ranking)
##############################################################################
##############Top 5 des universités pour les années 2012 - 2015###############
##############################################################################
cwurTop5 = U_Ranking %>% group_by(year) %>% 
  select(year,institution,world_rank) %>% top_n(-5, wt = world_rank) 

plot_ly(cwurTop5, x = ~year) %>% 
add_trace(y = cwurTop5$world_rank, name = cwurTop5$institution, showlegend=TRUE, type = 'scatter', mode = 'lines+markers', color= cwurTop5$institution) %>%
layout(title="Top 5 des universités pour les années (2012-2015)",  
       xaxis = list(showticklabels = TRUE, tickangle = 0, dtick=1,tickfont = list(size = 12)),
       yaxis = list(title = "Classement mondial",dtick=1), hovermode = 'compare')
############################################################################################
###### Le top 10, basé sur le classement mondial, pour chaque année. #######################
###### Les 3 premières places en or, argent et bronze, avec du vert le reste des places.####
############################################################################################
cwurPlotYear <- function(nYear){
  U_Ranking %>% filter(year==nYear) %>% top_n(10, -world_rank) %>% 
    ggplot(aes(x=reorder(institution, -world_rank), y=world_rank)) + 
    geom_bar(stat="identity", aes(fill=reorder(institution,-world_rank)), colour="black") +
    theme_bw() + coord_flip() +  scale_fill_manual(values=c(rep("lightgreen",7), "#CD7F32", "grey", "gold")) + guides(fill=FALSE)+
    labs(x="Institution", y="Rang mondial", 
         title=paste("Rang in ",nYear), subtitle="(smaller value is better)") 
}

d1 <- cwurPlotYear(2012)
d2 <-  cwurPlotYear(2013)
d3 <- cwurPlotYear(2014)
d4 <-  cwurPlotYear(2015)

grid.arrange(d1,d2,d3,d4, ncol=2)

##################################################################################################
######################################Classement des pays#########################################
######### Choisissons les 10 meilleurs pays#######################################################
### classement basé sur le nombre de publications, le nombre de citations, le nombre de brevets.##
##################################################################################################
c <- U_Ranking %>% group_by(country) %>% summarise(n = length(publications)) %>% top_n(10,n) %>% ungroup()

#Box plot
#Classement par publication
d1 <- U_Ranking %>% filter(country %in% c$country) %>%
  ggplot(aes(x=country, y=publications, col=country))  + guides(col=FALSE) +
  geom_boxplot() +  theme_bw() + coord_flip() + 
  labs(x="Country", y="Classement par publication", 
       title="Classement par  publication", subtitle="Groupé par country, smaller value is better") 
##################################################################################################
######################################Classement par citation#####################################
##################################################################################################
d2 <- U_Ranking %>% filter(country %in% c$country) %>%
  ggplot(aes(x=country, y=citations, col=country)) + guides(col=FALSE) +
  geom_boxplot() +  theme_bw() + coord_flip() + 
  labs(x="Country", y="Classement par citations", 
       title="Classement par citations", subtitle="Groupé par country, smaller value is better")
##################################################################################################
######################################Classement par brevets######################################
##################################################################################################
d3 <- U_Ranking %>% filter(country %in% c$country) %>%
  ggplot(aes(x=country, y=patents, col=country)) + guides(col=FALSE) +
  geom_boxplot() +  theme_bw() + coord_flip() + 
  labs(x="Country", y="Classement par brevets", 
       title="Classement par brevets", subtitle="Groupé par country, smaller value is better") 
##################################################################################################
######################################Classement par qualité de l'éducation#######################
##################################################################################################
d4 <- U_Ranking %>% filter(country %in% c$country) %>%
  ggplot(aes(x=country, y=quality_of_education, col=country)) + guides(col=FALSE) +
  geom_boxplot() +  theme_bw() + coord_flip() + 
  labs(x="Country", y="Classement par qualité de l'éducation", 
       title="Classement par qualité de l'éducation", subtitle="Groupé par country, smaller value is better")
##################################################################################################
######################################Classement selon l'emploi des anciens#######################
##################################################################################################
d5 <- U_Ranking %>% filter(country %in% c$country) %>%
  ggplot(aes(x=country, y=alumni_employment, col=country)) + guides(col=FALSE) +
  geom_boxplot() +  theme_bw() + coord_flip() + 
  labs(x="Country", y="Classement selon l'emploi des anciens", 
       title="Classement selon l'emploi des anciens", subtitle="Groupé par country, smaller value is better") 
##################################################################################################
######################################Classement selon la qualité de la faculté###################
##################################################################################################
d6 <- U_Ranking %>% filter(country %in% c$country) %>%
  ggplot(aes(x=country, y=quality_of_faculty, col=country)) + guides(col=FALSE) +
  geom_boxplot() +  theme_bw() + coord_flip() + 
  labs(x="Country", y="Classement selon la qualité de la faculté", 
       title="Classement selon la qualité de la faculté", subtitle="Groupé par country, smaller value is better") 


grid.arrange(d1,d2,d3,d4,d5,d6, ncol=2)
##################################################################################################
#############################Nombre d'universités parmi les meilleures############################
##################################################################################################
ccwur <- U_Ranking %>% group_by(country,year) %>% 
  summarise(nr = length(world_rank), minw=min(world_rank), maxw=max(world_rank), 
            avgw=round(mean(world_rank),0)) %>%
  select(country, year, nr, minw, maxw, avgw) %>% ungroup() 

# Frontières gris clair
l <- list(color = toRGB("grey"), width = 0.5)
ccwur$hover <- with(ccwur, 
                    paste("Country: ", country, '<br>', 
                          "Année: ",year, "<br>",
                          "Universities in top: ", nr, "<br>",
                          "Min rank in top: ", minw, "<br>",
                          "Max rank in top: ", maxw, "<br>",
                          "Mean rank in top: ", avgw,"<br>"
                    ))
#Spécifier la projection de la carte 
g <- list(
  showframe = TRUE,
  showcoastlines = TRUE,
  projection = list(type = 'orthogonal')
)

plot_geo(ccwur, locationmode = 'country names') %>%
  add_trace(
    z = ~nr, color = ~nr, colors = 'Spectral', frame = ~year,
    text = ~hover, locations=~country, marker = list(line = l)
  ) %>%
  colorbar(title = 'Nombre \nd universités parmi les meilleures', tickprefix = '') %>%
  layout(
    title = with(ccwur, paste('Nombre d universités parmi les meilleures')),
    geo = g
  )

##################################################################################################
#########################################Read data shanghaiData###################################
##################################################################################################
shanghaiDataCld <- read.csv(file.choose())
View(shanghaiDataCld)

shanghaiDataCld$t_score = 
  0.1 * shanghaiDataCld$alumni + 0.2 * shanghaiDataCld$award + 0.2 * shanghaiDataCld$hici + 
  0.2 * shanghaiDataCld$ns + 0.2 * shanghaiDataCld$pub + 0.1 * shanghaiDataCld$pcp

shanghaiDataCld$t_score
shanghaiDataCld$total_score[is.na(shanghaiDataCld$total_score)] = shanghaiDataCld$t_score[is.na(shanghaiDataCld$total_score)]

#Fix the duplicate name for University of California-Berkeley
shanghaiDataCld$university_name[shanghaiDataCld$university_name=="University of California-Berkeley"] <- "University of California, Berkeley"
shanghaiDataCld %>% group_by(year) %>% 
  top_n(10, wt = total_score) %>% select(year,university_name,total_score,alumni, award, hici, ns, pub, pcp) %>% ungroup() -> top10univ

#draw with plotly

plot_ly(top10univ, x = ~year) %>%
  add_trace(y = top10univ$total_score, name = top10univ$university_name, showlegend=TRUE, type = 'scatter', mode = 'lines+markers', color= top10univ$university_name) %>%
  layout(title="Shanghai (ARWU) World Ranks (2005-2015)<br>Best ranked universities based on total score", legend = list(orientation = 'h'),
         xaxis = list(showticklabels = TRUE, tickangle = 0, tickfont = list(size = 8)),
         yaxis = list(title = "Total score"),
         hovermode = 'compare')

##########################################################################################################
#############################################Shanghai World University  Rankings top 5####################
##########################################################################################################
shanghaiDataCld$university_name[shanghaiDataCld$university_name=="University of California-Berkeley"] <- "University of California, Berkeley"
shanghaiDataCld %>% group_by(year) %>% 
  top_n(5, wt = total_score) %>% select(year,university_name,total_score,alumni, award, hici, ns, pub, pcp) %>% ungroup() -> top5univ


top5SpiderWebYear <- function(nYear) {
  top5univ %>% filter(year==nYear) %>% ungroup() -> top5u
  top5 <- as.data.frame(cbind(top5u[,c(3,4,5,6,7,8,9)])) # SELECT COLUMNS TO DISPLAY
  colnames(top5) <- c("Total Score", "Alumni with Nobel", "Awarded Nobel", "Highly Cited", 
                      "Nature&Science", "Publications", "PCAP")
  rownames(top5) <- top5$university_name
  #rmin <- apply(top10,2,min); rmax <- apply(top10,2,max)
  #print(rmin)
  rmax <- 100
  rmin <- 0
  colors_border=c( "tomato", "blue", "gold", "green", "magenta", 
                   "yellow", "grey", "lightblue", "brown", "red", "lightgreen", "cyan" )
  par(mfrow=c(4,3))
  par(mar=c(1,1,5,1))
  for(i in 1:nrow(top5)){
    colorValue<-(col2rgb(as.character(colors_border[i]))%>% as.integer())/255
    radarchart(rbind(rmax,rmin,top5[i, ]),#top[i, ] columns to display
               axistype=2 , 
               pcol=rgb(colorValue[1],colorValue[2],colorValue[3], alpha = 1),
               pfcol=rgb(colorValue[1],colorValue[2],colorValue[3], alpha = 0.5),
               plwd=1 , plty=1,cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.5,vlcex=0.7, 
               title=rownames(top5[i,]))# row name like title for graph
  }
  title(paste0('\nShanghai World University  Rankings top 5 (',nYear,')'),outer=TRUE,col.main='black',cex.main=1.5)
}

top5SpiderWebYear(2005)





