

#' A function to plot yearly data from www.kolada.se. Most code is taken from https://github.com/reinholdsson/swemaps and https://github.com/reinholdsson/rkolada
#'
#' This function plots yearly data from  www.kolada.se. Output is an interactive html document, and a static pdf.
#' If gif=TRUE the function also plots a longitudinal gif of the data
#' @param name Name of outpufile/plot title. Required. Default is NULL.
#' @param kpiID kpiIDs from www.kolada.se. Can be found in the KPI_id files. Required. Default is NULL.
#' @param year Year to be plotted as interactive and PDF. Default to 2016.
#' @param value Plots values instead of percentage. Default is FALSE.
#' @param gif Plots longitudinal data of all years included. Takes substanstial time. Default is FALSE.
#' @param znorm Plots z-values per group and year. Default is FALSE.
#' @param outfile Writes output to files. Chose FALSE for Rmarkdown document. Default is TRUE.



#SwedenMap(znorm=TRUE,gif=TRUE,year='2015',value=FALSE, name='Förvarsarbetande',kpiID='N00908,N00909,N00910,N00911,N00912,N00913')
SwedenMap <- function(name=NULL,kpiID=NULL,year=2015,gif=FALSE,value=FALSE,znorm=FALSE,outfile=TRUE) {
        
        localenv <- environment()
        suppressWarnings(suppressMessages(library(ggplot2)))
        suppressWarnings(suppressMessages(library(swemaps)))
        suppressWarnings(suppressMessages(library(rkolada)))
        suppressWarnings(suppressMessages(library(maps)))
        suppressWarnings(suppressMessages(library(dplyr)))
        suppressWarnings(suppressMessages(library(RColorBrewer)))
        suppressWarnings(suppressMessages(library(htmlwidgets)))
        suppressWarnings(suppressMessages(library(rvg)))
        suppressWarnings(suppressMessages(library(ggiraph)))
        suppressWarnings(suppressMessages(library(gganimate)))

        if (is.null(kpiID)){
          stop("Key performace indicator is required\n", call.=FALSE)
        }

        if (is.null(name)){
          stop("Name is required\n", call.=FALSE)
        }

        if (is.null(year)){
          cat("Plotting 2016","\n")
        }

        if (gif){
          cat("Plotting gif of yearly changes. Expect long wait time","\n")
        }

        if (value){
                cat('Plotting values instead of percentage, and normalsing to z-values','\n')
                perValue <- 'Value'
                } else {cat('Plotting percentage','\n')
                perValue <-'Percentage'}
        if (znorm){
                cat('Plotting z-value','\n')
                perValue <-'z-value'
        } 

        
        # rkolada  https://github.com/reinholdsson/rkolada
        a <- rkolada::rkolada()
        kpi <- a$kpi()        
        
        if ( grepl(',',kpiID)){
                kpi.ID <-strsplit(as.character(kpiID),split=',')[[1]]
                } else {kpi.ID <- as.character(kpiID)}
        
        name <- name
        #year <- as.character(year)
        dput(kpi.ID)
        class(year)
        class(kpi.ID)
        
        if (gif){
                x <- a$values(kpi.ID)
                } else { x <- a$values(kpi.ID, year=year) }
                
        x <- prepare_map_data(x)
        x <- x[x$gender=='T',]
        head(x)
        sweden_map <- x %>%
        	select(leaflet_long, leaflet_lat, knkod, kpi.id, municipality.id, knnamn, lnnamn) %>%
        	rename(long = leaflet_long, lat= leaflet_lat, region=knkod, subregion=knnamn) %>%
        	fortify() %>%
                unique()
        
        if (value){
                x$tip <- paste0(
                  "<b>", x$knnamn, "</b> ",
                  "<br>", prettyNum(round(x$value), big.mark = ","))
                } else {
                        x$tip <- paste0(
                        "<b>", x$knnamn, "</b> ",
                        "<br>", paste0(round(x$value),'%'))
                }	
        
        
        x$title <- kpi[match(x$kpi.id, kpi$kpi.id),]$kpi.title
        
        unique(x$title)
        
        
        if (value){ 
                if(any(grepl('%',x$title))) {
                        stop('Mixed percentage and values')}
                } else if (value==F) {
                        if(any(!grepl('%',x$title))) {
                        stop('Mixed percentage and values')}   
                }
        
        if (znorm){ 
                x2 <- x
                x2 <- x2[x2$period==year,]
                x2 <- x2 %>%
                        group_by(title) %>%
                        mutate(value, value = (value-mean(value,na.rm=T))/sd(value,na.rm=T)) #g
                        
                        x2 <- as.data.frame(x2)
                        midPoint <- 0
                } else {x2 <- x
                x2 <- x2[x2$period==year,]
                midPoint <- mean(x2[x2$period==year,]$value)
        }
        x3 <- unique(subset(x2,select=c(knkod,knnamn,tip,value,title)))            
        gg <- ggplot(x3, aes(map_id=knkod, fill=value), environment = localenv )+ 
        	geom_map_interactive(data=x3, map=sweden_map, aes(fill=value, 
        		tooltip=tip, map_id=knkod, data_id=knkod), 
        		colour="black", size=0.05)+
        	#geom_map(map=sweden_map,aes(map_id=knkod), size=0.25) +
        	expand_limits(x = sweden_map$long, y = sweden_map$lat)+
        	theme_void() +
        	facet_wrap(~title,  labeller = labeller(title = label_wrap_gen(25)), ncol=4)+
        	scale_fill_gradient2(low="firebrick1", mid='white', high="steelblue", midpoint=midPoint) +
        	theme(legend.position="top")+
        	ggtitle(name)+
        	theme(plot.background = element_rect(fill = '#999999'))+
        	labs(fill=perValue) +
        	theme(plot.title = element_text(hjust = 0.5))
        
        width <- if(length(kpi.ID)==1) {2} else if(length(kpi.ID)==2) {4} else if(length(kpi.ID)==3) {6} else if(length(kpi.ID)==4) {8}  else {8} 
        height <- ceiling(length(kpi.ID)/4)*5
       
       

        m <- ggiraph(code = {print(gg)},  hover_css = "fill:red;r:3pt;" , width_svg=width, height_svg=height)
        saveWidget(m, file=paste0(name,'.html'),selfcontained=F)
        
        ggsave(paste0(name,'.pdf'),gg,height=height,width=width)
        
        
        ggiraph(code = {print(gg)},  hover_css = "fill:red;r:3pt;", width_svg=width, height_svg=height)) 
        gg
       
        	
        

        
        if (gif) {
                if (znorm){ 
                        x2 <- x
                        x2 <- x2 %>%
                                group_by(title) %>%
                                mutate(value, value = (value-mean(value,na.rm=T))/sd(value,na.rm=T)) #g
                                
                                x2 <- as.data.frame(x2)
                                midPoint <- 0
                        } else {x2 <- x
                        midPoint <- mean(x2[x2$period==year,]$value)
                }
                x3 <- unique(subset(x2,select=c(knkod,knnamn,tip,period,value,title)))            
                                        
                gg <- ggplot(x3, aes(map_id=knkod, fill=value, frame = period))+ 
                	geom_map_interactive(data=x3, map=sweden_map, aes(fill=value, 
                		tooltip=tip, map_id=knkod, data_id=knkod), 
                		colour="black", size=0.05)+
                	#geom_map(map=sweden_map,aes(map_id=knkod), size=0.25) +
                	expand_limits(x = sweden_map$long, y = sweden_map$lat)+
                	theme_void() +
                	facet_wrap(~title,  labeller = labeller(title = label_wrap_gen(25)), ncol=4)+
                	scale_fill_gradient2(low="firebrick1", mid='white', high="steelblue",  midpoint=midPoint) +
                	theme(legend.position="top")+
                	ggtitle(name)+
                	theme(plot.background = element_rect(fill = '#999999'))+
                	labs(fill=perValue) +
                	theme(plot.title = element_text(hjust = 0.5))
                
                width <- if(length(kpi.ID)==1) {200} else if(length(kpi.ID)==2) 400 else if(length(kpi.ID)==3) {600} else if(length(kpi.ID)==4) {800}  else {800} 
                height <- ceiling(length(kpi.ID)/4)*300
                
                
                gg_animate(gg,paste0(name,'.gif'),ani.height=height,ani.width=width
        }
        
} 

