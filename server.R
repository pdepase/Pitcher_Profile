



server <- function(input, output,session) {
  library(shinythemes)
  library(shinyjs)
  library(shinyWidgets)
  library(tidyverse)
  library(hrbrthemes)
  library(flexdashboard)
  library(shinydashboard)
  library(shinydashboard)
  library(feather)

  ## LOAD IN NECESSARY DATAFRAMES ##
  Standards <- read_feather("Standards.feather")
  FB_velo_summary <- read_feather("FB_velo_summary.feather")
  
  
  ## WHEN SEARCH BUTTON IS PRESSED THIS WILL RUN ##
  observeEvent(input$search, {
    if(!is.null(input$Pitcher)){
      updateTabItems(session, "tabs", "Overview")
      
      
      
      
      ## FILTER DATABASE TO IMPORT ONLY THE SELECTED PITCHERS DATA##
      
      Pitcher_Name <- input$Pitcher
      
      
      pitcher <- read_feather("pitcher_profile.feather")
      
      pitcher <- pitcher %>% filter(mlb_name==Pitcher_Name)
      
      pitcher$Pitcher_Team <- ifelse(pitcher$inning_topbot=="Bot",pitcher$away_team, pitcher$home_team)
      
      pitcher$game_date <- as.Date(pitcher$game_date)
      
      pitcher <- pitcher %>% arrange(desc(game_date))
      
      player_team <-  pitcher[1,49]
      
      page_top <-  paste(Pitcher_Name,"-", player_team)
      
      
      
      
      pitcher <- pitcher %>% filter(game_year=="2024" & is.na(release_extension)==FALSE)
      
      
      
      ##CREATE PITCH CHARACTERISTICS TABLE##
      
      pitch_characteristics <- pitcher %>% group_by(pitch_name) %>% summarise(Pitches=n(),
                                                                                   Velo=mean(release_speed),
                                                                                   Vertical_Break=mean(VB),
                                                                                   Horizontal_Break=mean(HB),
                                                                                   RH=mean(release_pos_z),
                                                                                   RS=(-1)*mean(release_pos_x),
                                                                                   EXT=mean(release_extension)
      )
      
      pitch_characteristics<- pitch_characteristics %>% mutate(USG=100*Pitches/sum(Pitches))
      
      pitch_characteristics <- pitch_characteristics %>% arrange(desc(USG))
      
      
      
      
      
      
      
      
      pitch_characteristics$HB <- pitch_characteristics$Horizontal_Break
      pitch_characteristics$VB <- pitch_characteristics$Vertical_Break
      
      
      pitch_characteristics <- pitch_characteristics %>% select(pitch_name,USG,Velo, VB,HB, RH,RS,EXT)
      
      
      ##CREATE LIST OF PITCHES THAT THE PITCHER HAS##
      pitch_for_plot <- pitch_characteristics %>% select(pitch_name,USG)
      pitch_for_plot <- pitch_for_plot %>% arrange(desc( USG))
      pitch_for_plot <-  pitch_for_plot %>% slice(1:4)
      pitch_for_plot <- pitch_for_plot %>% filter(USG>5)
      
      names(pitch_characteristics)[names(pitch_characteristics) == "pitch_name"] <- "Pitch Type"
      names(pitch_characteristics)[names(pitch_characteristics) == "USG"] <- "Usage%"
      
      
      
      pitcher$IsChase <- ifelse(pitcher$IsInZone=="0" & pitcher$IsSwing=="1",1,0 )
      pitcher$IsOutZone <- ifelse(pitcher$IsInZone=="0",1,0)
      
     
      
      
     ## CREATE PITCH LEVEL RESULTS TABLE## 
      pitch_results <-  pitcher %>% group_by(pitch_name) %>% summarise(Pitches=n(),
                                                                            AB=sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun)+sum(IsOut),
                                                                            STRpct=100*(sum(IsSwing)+sum(IsCalledStrike))/Pitches,
                                                                            SwingPCT=100*sum(IsSwing)/Pitches,
                                                                            SwMissPCT=100*sum(IsSwMiss)/sum(IsSwing),
                                                                            CSW=100*(sum(IsSwMiss)+sum(IsCalledStrike))/Pitches,
                                                                            ChasePCT=100*sum(IsChase)/sum(IsOutZone),
                                                                            SOpct=100*sum(IsStrikeout)/(AB + sum(IsWalk)+sum(IsHBP)),
                                                                            BBpct=100*sum(IsWalk)/(AB + sum(IsWalk)+sum(IsHBP)),
                                                                            BAA=(sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun))/AB,
                                                                            OBP=(sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun)+sum(IsWalk)+sum(IsHBP))/(AB+sum(IsWalk)+sum(IsHBP)),
                                                                            SLG=(sum(IsSingle)+2*sum(IsDouble)+3*sum(IsTriple)+4*sum(IsHomerun))/AB,
                                                                            wOBA=(0.883*sum(IsSingle) + 1.238*sum(IsDouble) + 1.558*sum(IsTriple) + 1.979*sum(IsHomerun) + 0.699*sum(IsWalk)+0.728*sum(IsHBP))/(AB+sum(IsWalk)+sum(IsHBP)),
                                                                            xwOBA=(0.883*sum(xSingle_EVLA) + 1.238*sum(xDouble_EVLA) + 1.558*sum(xTriple_EVLA) + 1.979*sum(xHomeRun_EVLA) + 0.699*sum(IsWalk)+0.728*sum(IsHBP))/(AB+sum(IsWalk)+sum(IsHBP)),
                                                                            xBA=(sum(xSingle_EVLA)+sum(xDouble_EVLA)+sum(xTriple_EVLA)+sum(xHomeRun_EVLA))/AB,
                                                                            xSLG=(sum(xSingle_EVLA)+2*sum(xDouble_EVLA)+3*sum(xTriple_EVLA)+4*sum(xHomeRun_EVLA))/AB)
      
      
      
      
      
      names(pitch_results)[names(pitch_results) == "pitch_name"] <- "Pitch Type"
      names(pitch_results)[names(pitch_results) == "SwingPCT"] <- "Swing%"
      names(pitch_results)[names(pitch_results) == "SwMissPCT"] <- "SM%"
      
      names(pitch_results)[names(pitch_results) == "STRpct"] <- "Strike%"
      names(pitch_results)[names(pitch_results) == "SOpct"] <- "K%"
      names(pitch_results)[names(pitch_results) == "BBpct"] <- "BB%"
      names(pitch_results)[names(pitch_results) == "ChasePCT"] <- "Chase%"
      
      
      pitch_results$AB <- as.integer(pitch_results$AB)
      
      pitch_results$BAA <- format(pitch_results$BAA, digits = 3)
      pitch_results$OBP <- format(pitch_results$OBP, digits = 3)
      pitch_results$SLG <- format(pitch_results$SLG, digits = 3)
      pitch_results$wOBA <- format(pitch_results$wOBA, digits = 3)
      pitch_results$xwOBA <- format(pitch_results$xwOBA, digits = 3)
      pitch_results$xBA <- format(pitch_results$xBA, digits = 3)
      pitch_results$xSLG <- format(pitch_results$xSLG, digits = 3)
      
      
      
      
      
      
      
      
      
      
      
      
      ##CREATE OVERALL PITCHER RESULTS TABLE##
      Results <-  pitcher %>% group_by(game_year) %>% summarise(Pitches=n(),
                                                                AB=sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun)+sum(IsOut),
                                                                STRpct=100*(sum(IsSwing)+sum(IsCalledStrike))/Pitches,
                                                                SwingPCT=100*sum(IsSwing)/Pitches,
                                                                SwMissPCT=100*sum(IsSwMiss)/sum(IsSwing),
                                                                CSW=100*(sum(IsSwMiss)+sum(IsCalledStrike))/Pitches,
                                                                ChasePCT=100*sum(IsChase)/sum(IsOutZone),
                                                                SOpct=100*sum(IsStrikeout)/(AB + sum(IsWalk)+sum(IsHBP)),
                                                                BBpct=100*sum(IsWalk)/(AB + sum(IsWalk)+sum(IsHBP)),            
                                                                HRpct = 100 *sum(IsHomerun)/(AB+sum(IsWalk)+sum(IsHBP)),
                                                                BAA=(sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun))/AB,
                                                                OBP=(sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun)+sum(IsWalk)+sum(IsHBP))/(AB+sum(IsWalk)+sum(IsHBP)),
                                                                SLG=(sum(IsSingle)+2*sum(IsDouble)+3*sum(IsTriple)+4*sum(IsHomerun))/AB,
                                                                wOBA=(0.883*sum(IsSingle) + 1.238*sum(IsDouble) + 1.558*sum(IsTriple) + 1.979*sum(IsHomerun) + 0.699*sum(IsWalk)+0.728*sum(IsHBP))/(AB+sum(IsWalk)+sum(IsHBP)),
                                                                xwOBA=(0.883*sum(xSingle_EVLA) + 1.238*sum(xDouble_EVLA) + 1.558*sum(xTriple_EVLA) + 1.979*sum(xHomeRun_EVLA) + 0.699*sum(IsWalk)+0.728*sum(IsHBP))/(AB+sum(IsWalk)+sum(IsHBP)),
                                                                xBA=(sum(xSingle_EVLA)+sum(xDouble_EVLA)+sum(xTriple_EVLA)+sum(xHomeRun_EVLA))/AB,
                                                                xSLG=(sum(xSingle_EVLA)+2*sum(xDouble_EVLA)+3*sum(xTriple_EVLA)+4*sum(xHomeRun_EVLA))/AB)
      
      
      
      
      
      names(Results)[names(Results) == "game_year"] <- "Year"
      names(Results)[names(Results) == "SwingPCT"] <- "Swing%"
      names(Results)[names(Results) == "SwMissPCT"] <- "SM%"
      names(Results)[names(Results) == "ChasePCT"] <- "Chase%"
      names(Results)[names(Results) == "STRpct"] <- "Strike%"
      names(Results)[names(Results) == "SOpct"] <- "K%"
      names(Results)[names(Results) == "BBpct"] <- "BB%"
      names(Results)[names(Results) == "HRpct"] <- "HR%"
      
      
      Results$Year <- as.integer(Results$Year)
      Results$AB <- as.integer(Results$AB)
      
      Results <-  Results %>% arrange(desc(Year))
      ##CREATE GAUGES FOR K%, BB%, HR%, wOBA, xwOBA, AND FB VELO##
      
      k_rated <- c(Results$'K%'[1])
      k_rate_pct <- round(100*ecdf(Standards$K_rate)(k_rated),2)
      Gauge1 <-  gauge(k_rate_pct, 
                       min = 0, 
                       max = 100,
                       label = "K%",
                       sectors = gaugeSectors(success = c(75,100 ), 
                                              warning = c(25, 75),
                                              danger = c(0, 25)))
      
      
      
      bb_rated <- c( Results$'BB%'[1])
      bb_rate_pct <- round(100-100*ecdf(Standards$BB_rate)(bb_rated),2)
      
      Gauge2 <- gauge(bb_rate_pct, 
                      min = 0, 
                      max = 100, 
                      label = "BB%",
                      sectors = gaugeSectors(success = c(75, 100), 
                                             warning = c(25, 75),
                                             danger = c(0, 25)))
      
      
      hr_rated <- c( Results$'HR%'[1])
      hr_rate_pct <- round(100-100*ecdf(Standards$HR_PA)(hr_rated),2)
      
      Gauge3 <- gauge(hr_rate_pct, 
                      min = 0, 
                      max = 100,
                      label = "HR%",
                      sectors = gaugeSectors(success = c(75, 100), 
                                             warning = c(25, 75),
                                             danger = c(0, 25)))
      
      wOBA_rated <- c( Results$'wOBA'[1])
      wOBA_rate_pct <- round(100-100*ecdf(Standards$wOBA)(wOBA_rated),2)
      Gauge4 <- gauge(wOBA_rate_pct, 
                      min = 0, 
                      max = 100,
                      label = "wOBA",
                      sectors = gaugeSectors(success = c(75, 100), 
                                             warning = c(25, 75),
                                             danger = c(0, 25)))
      
      
      
      xwOBA_rated <- c( Results$'xwOBA'[1])
      xwOBA_rate_pct <- round(100-100*ecdf(Standards$xwOBA)(xwOBA_rated),2)
      Gauge5 <- gauge(xwOBA_rate_pct, 
                      min = 0, 
                      max = 100,
                      label = "xwOBA",
                      sectors = gaugeSectors(success = c(75, 100), 
                                             warning = c(25, 75),
                                             danger = c(0, 25)))
      
      
      FBV_rated <- c( max(pitch_characteristics$Velo))
      FBV_rate_pct <- round(100*ecdf(FB_velo_summary$avg_velo)(FBV_rated),2)
      Gauge6 <- gauge(FBV_rate_pct, 
                      min = 0, 
                      max = 100,
                      label = "FB Velo",
                      sectors = gaugeSectors(success = c(75, 100), 
                                             warning = c(25, 75),
                                             danger = c(0, 25)))
      
      
      Results$BAA <- format(Results$BAA, digits = 3)
      Results$OBP <- format(Results$OBP, digits = 3)
      Results$SLG <- format(Results$SLG, digits = 3)
      Results$wOBA <- format(Results$wOBA, digits = 3)
      Results$xwOBA <- format(Results$xwOBA, digits = 3)
      Results$xBA <- format(Results$xBA, digits = 3)
      Results$xSLG <- format(Results$xSLG, digits = 3)
      
      
      
      
      ##CREATE BINS FOR PITCH HEIGHT AND SIDE##
      pitcher$PitchHeightBin <- ifelse(pitcher$plate_z<1.6,1,
                                            ifelse(pitcher$plate_z>=1.6 & pitcher$plate_z<2.25,2,
                                                   ifelse(pitcher$plate_z>=2.25 & pitcher$plate_z<2.93,3,
                                                          ifelse(pitcher$plate_z>=2.93 & pitcher$plate_z<3.6,4,
                                                                 ifelse(pitcher$plate_z>=3.6 ,5,"Error")))))
      
      
      
      pitcher$PitchSideBin <- ifelse(pitcher$plate_x<(-0.92),5,
                                          ifelse(pitcher$plate_x>=(-0.92) & pitcher$plate_x<(-0.31),4,
                                                 ifelse(pitcher$plate_x>=(-.31) & pitcher$plate_x<0.31,3,
                                                        ifelse(pitcher$plate_x>=0.31 & pitcher$plate_x<0.92,2,
                                                               ifelse(pitcher$plate_x>=0.92 ,1,"Error")))))
      
      
      
      
      
      
      
      ##MAKE PLATE LOCATION FROM PITCHERS VIEW NOT CATCHERS##
      pitcher <-  pitcher %>% mutate(plate_x_adj= plate_x * (-1))
      
      ##PLOT PITCH LOCATIONS OF HIGHEST USAGE PITCH##
      Pitch1 <- paste(pitch_for_plot[1,1],sep = "")
      
      title1 <- paste("------------------------------ ",Pitch1," 2024 Results", "------------------------------",sep = "")
      
      output$Pitch1_Zones_Title <- renderText(title1)
      
      pitch1_data <- pitcher %>% filter(pitch_name==Pitch1)
      
      
      # COLOR SCALE FOR A SMOOTHED SCATTERPLOT##
      library(RColorBrewer)
      buylrd = c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF",
                 "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026") 
      myColRamp = colorRampPalette(c(buylrd))
      
      
      
      ##CREATE ZONE SUMMARYS FOR HIS HIGHEST USAGE PITCH##
      
      Pitch_1_Zone_Summary <- pitch1_data %>% group_by(PitchHeightBin,PitchSideBin) %>% summarise(Pitches=n(),
                                                                                                  wOBA=(0.883*sum(IsSingle) + 1.238*sum(IsDouble) + 1.558*sum(IsTriple) + 1.979*sum(IsHomerun) + 0.699*sum(IsWalk)+0.728*sum(IsHBP))/(sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun)+sum(IsOut)+sum(IsWalk)+sum(IsHBP)),
                                                                                                  SwMissPCT=sum(IsSwMiss)/sum(IsSwing),
                                                                                                  HH=sum(Is95mphEV)/Pitches,
                                                                                                  AVG=(sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun))/(sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun)+sum(IsOut)))
      
      Pitch_1_Sw_Zones <- ggplot(Pitch_1_Zone_Summary, aes(x=PitchSideBin,y=PitchHeightBin,  fill= SwMissPCT))+
        geom_raster()+
        scale_fill_gradient2(low="blue", high="red", mid = "white", midpoint =0.25 ) +
        geom_rect(xmin= 1.5, xmax=4.5, ymin=1.5, ymax=4.5,color='black', alpha=0)+
        ggtitle("SM% By Location") + xlab("Blue=Bad for Pitcher, Red= Good for Pitcher") +ylab("")+theme(plot.title = element_text(hjust = 0.5))
      
      
      output$Pitch1_SwMiss <- renderPlot(Pitch_1_Sw_Zones)
      
      
      
      Pitch_1_HH_Zones <- ggplot(Pitch_1_Zone_Summary, aes(x=PitchSideBin,y=PitchHeightBin,  fill= HH))+
        geom_raster()+
        scale_fill_gradient2(low="red", high="blue",mid = "white",midpoint = 0.05) +
        geom_rect(xmin= 1.5, xmax=4.5, ymin=1.5, ymax=4.5,color='black', alpha=0)+
        ggtitle("HH Per Pitch By Location") + xlab("Blue=Bad for Pitcher, Red= Good for Pitcher") +ylab("")+theme(plot.title = element_text(hjust = 0.5))
      
      output$Pitch1_HardHit <- renderPlot(Pitch_1_HH_Zones)
      
      
      
      Pitch_1_wOBA_Zones <- ggplot(Pitch_1_Zone_Summary, aes(x=PitchSideBin,y=PitchHeightBin,  fill= wOBA))+
        geom_raster()+
        scale_fill_gradient2(low="red", high="blue",mid = "white",midpoint = 0.325) +
        geom_rect(xmin= 1.5, xmax=4.5, ymin=1.5, ymax=4.5,color='black', alpha=0)+
        ggtitle("wOBA By Location") + xlab("Blue=Bad for Pitcher, Red= Good for Pitcher") +ylab("")+theme(plot.title = element_text(hjust = 0.5))
      
      output$Pitch1_wOBA <- renderPlot(Pitch_1_wOBA_Zones)
      
      
      Pitch_1_BA_Zones <- ggplot(Pitch_1_Zone_Summary, aes(x=PitchSideBin,y=PitchHeightBin,  fill= AVG))+
        geom_raster()+
        scale_fill_gradient2(low="red", high="blue",mid = "white",midpoint = 0.251 ) +
        geom_rect(xmin= 1.5, xmax=4.5, ymin=1.5, ymax=4.5,color='black', alpha=0)+
        ggtitle("Batting Average By Location") + xlab("Blue=Bad for Pitcher, Red= Good for Pitcher") +ylab("")+theme(plot.title = element_text(hjust = 0.5))
      
      output$Pitch1_BA <- renderPlot(Pitch_1_BA_Zones)
      
      
      ##CREATE ZONE SUMMARIES FOR HIS SECOND HIGHEST USAGE PITCH##
      if(length(pitch_for_plot$pitch_name)>1){
        Pitch2 <- paste(pitch_for_plot[2,1],sep = "")
        
        pitch2_data <- pitcher %>% filter(pitch_name==Pitch2)
        
        title2 <- paste("------------------------------ ",Pitch2," 2024 Results", "------------------------------",sep = "")
        
        output$Pitch2_Zones_Title <- renderText(title2)
        
        
        
        
        
        Pitch_2_Zone_Summary <- pitch2_data %>% group_by(PitchHeightBin,PitchSideBin) %>% summarise(Pitches=n(),
                                                                                                    wOBA=(0.883*sum(IsSingle) + 1.238*sum(IsDouble) + 1.558*sum(IsTriple) + 1.979*sum(IsHomerun) + 0.699*sum(IsWalk)+0.728*sum(IsHBP))/(sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun)+sum(IsOut)+sum(IsWalk)+sum(IsHBP)),
                                                                                                    SwMissPCT=sum(IsSwMiss)/sum(IsSwing),
                                                                                                    HH=sum(Is95mphEV)/Pitches,
                                                                                                    AVG=(sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun))/(sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun)+sum(IsOut)))
        
        Pitch_2_Sw_Zones <- ggplot(Pitch_2_Zone_Summary, aes(x=PitchSideBin,y=PitchHeightBin,  fill= SwMissPCT))+
          geom_raster()+
          scale_fill_gradient2(low="blue", high="red", mid = "white", midpoint =0.25 ) +
          geom_rect(xmin= 1.5, xmax=4.5, ymin=1.5, ymax=4.5,color='black', alpha=0)+
          ggtitle("SM% By Location") + xlab("Blue=Bad for Pitcher, Red= Good for Pitcher") +ylab("")+theme(plot.title = element_text(hjust = 0.5))
        
        
        
        
        output$P2plot <- renderPlot(smoothScatter(x=pitch2_data$plate_x_adj,y=pitch2_data$plate_z,colramp=myColRamp,main=Pitch2,xlab = "Side",ylab = "Height", xlim = c(-2,2), ylim = c(1,4))+ rect(xleft = (-0.92), ybottom = 1.6,xright = 0.92,ytop = 3.6))
        output$Pitch2_SwMiss <-renderPlot(Pitch_2_Sw_Zones)
        
        
        
        Pitch_2_HH_Zones <- ggplot(Pitch_2_Zone_Summary, aes(x=PitchSideBin,y=PitchHeightBin,  fill= HH))+
          geom_raster()+
          scale_fill_gradient2(low="red", high="blue",mid = "white",midpoint = 0.05) +
          geom_rect(xmin= 1.5, xmax=4.5, ymin=1.5, ymax=4.5,color='black', alpha=0)+
          ggtitle("Hard Hit Per Pitch By Location") + xlab("Blue=Bad for Pitcher, Red= Good for Pitcher") +ylab("")+theme(plot.title = element_text(hjust = 0.5))
        
        output$Pitch2_HardHit <- renderPlot(Pitch_2_HH_Zones)
        
        
        
        Pitch_2_wOBA_Zones <- ggplot(Pitch_2_Zone_Summary, aes(x=PitchSideBin,y=PitchHeightBin,  fill= wOBA))+
          geom_raster()+
          scale_fill_gradient2(low="red", high="blue",mid = "white",midpoint = 0.325) +
          geom_rect(xmin= 1.5, xmax=4.5, ymin=1.5, ymax=4.5,color='black', alpha=0)+
          ggtitle("wOBA By Location") + xlab("Blue=Bad for Pitcher, Red= Good for Pitcher") +ylab("")+theme(plot.title = element_text(hjust = 0.5))
        
        output$Pitch2_wOBA <- renderPlot(Pitch_2_wOBA_Zones)
        
        
        Pitch_2_BA_Zones <- ggplot(Pitch_2_Zone_Summary, aes(x=PitchSideBin,y=PitchHeightBin,  fill= AVG))+
          geom_raster()+
          scale_fill_gradient2(low="red", high="blue",mid = "white",midpoint = 0.251 ) +
          geom_rect(xmin= 1.5, xmax=4.5, ymin=1.5, ymax=4.5,color='black', alpha=0)+
          ggtitle("Batting Average By Location") + xlab("Blue=Bad for Pitcher, Red= Good for Pitcher") +ylab("")+theme(plot.title = element_text(hjust = 0.5))
        
        output$Pitch2_BA <- renderPlot(Pitch_2_BA_Zones)
        
        
        
      }
      
      ##IF HE ONLY HAS 1 PITCH RETURN A BLANK PLOT##
      if(length(pitch_for_plot$pitch_name)<2){
        empty <- data.frame(plate_x=numeric(),plate_z=numeric())
        output$P2plot <- renderPlot(ggplot(empty))
        output$Pitch2_SwMiss <- renderPlot("")
        output$Pitch2_HardHit <- renderPlot("")
        output$Pitch2_wOBA <- renderPlot("")
        output$Pitch2_BA <- renderPlot("")
        output$Pitch2_Zones_Title <- renderText("")
      }
      
      
      ##CREATE ZONE SUMMARIES FOR HIS 3RD HIGHEST USAGE PITCH IF HE HAS ONE##
      if(length(pitch_for_plot$pitch_name)>2){
        Pitch3 <- paste(pitch_for_plot[3,1],sep = "")
        
        pitch3_data <- pitcher %>% filter(pitch_name==Pitch3)
        
        title3 <- paste("------------------------------ ",Pitch3," 2024 Results", "------------------------------",sep = "")
        
        output$Pitch3_Zones_Title <- renderText(title3)
        
        
        
        output$P3plot <- renderPlot(smoothScatter(x=pitch3_data$plate_x_adj,y=pitch3_data$plate_z,colramp=myColRamp,main=Pitch3,xlab = "Side",ylab = "Height", xlim = c(-2,2), ylim = c(1,4))+ rect(xleft = (-0.92), ybottom = 1.6,xright = 0.92,ytop = 3.6))
        
        
        
        
        Pitch_3_Zone_Summary <- pitch3_data %>% group_by(PitchHeightBin,PitchSideBin) %>% summarise(Pitches=n(),
                                                                                                    wOBA=(0.883*sum(IsSingle) + 1.238*sum(IsDouble) + 1.558*sum(IsTriple) + 1.979*sum(IsHomerun) + 0.699*sum(IsWalk)+0.728*sum(IsHBP))/(sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun)+sum(IsOut)+sum(IsWalk)+sum(IsHBP)),
                                                                                                    SwMissPCT=sum(IsSwMiss)/sum(IsSwing),
                                                                                                    HH=sum(Is95mphEV)/Pitches,
                                                                                                    AVG=(sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun))/(sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun)+sum(IsOut)))
        
        Pitch_3_Sw_Zones <- ggplot(Pitch_3_Zone_Summary, aes(x=PitchSideBin,y=PitchHeightBin,  fill= SwMissPCT))+
          geom_raster()+
          scale_fill_gradient2(low="blue", high="red", mid = "white", midpoint =0.25 ) +
          geom_rect(xmin= 1.5, xmax=4.5, ymin=1.5, ymax=4.5,color='black', alpha=0)+
          ggtitle("SM% By Location") + xlab("Blue=Bad for Pitcher, Red= Good for Pitcher") +ylab("")+theme(plot.title = element_text(hjust = 0.5))
        
        output$Pitch3_SwMiss <- renderPlot(Pitch_3_Sw_Zones)
        
        
        Pitch_3_HH_Zones <- ggplot(Pitch_3_Zone_Summary, aes(x=PitchSideBin,y=PitchHeightBin,  fill= HH))+
          geom_raster()+
          scale_fill_gradient2(low="red", high="blue",mid = "white",midpoint = 0.05) +
          geom_rect(xmin= 1.5, xmax=4.5, ymin=1.5, ymax=4.5,color='black', alpha=0)+
          ggtitle("Hard Hit Per Pitch By Location") + xlab("Blue=Bad for Pitcher, Red= Good for Pitcher") +ylab("")+theme(plot.title = element_text(hjust = 0.5))
        
        output$Pitch3_HardHit <- renderPlot(Pitch_3_HH_Zones)
        
        
        
        Pitch_3_wOBA_Zones <- ggplot(Pitch_3_Zone_Summary, aes(x=PitchSideBin,y=PitchHeightBin,  fill= wOBA))+
          geom_raster()+
          scale_fill_gradient2(low="red", high="blue",mid = "white",midpoint = 0.325) +
          geom_rect(xmin= 1.5, xmax=4.5, ymin=1.5, ymax=4.5,color='black', alpha=0)+
          ggtitle("wOBA By Location") + xlab("Blue=Bad for Pitcher, Red= Good for Pitcher") +ylab("")+theme(plot.title = element_text(hjust = 0.5))
        
        output$Pitch3_wOBA <- renderPlot(Pitch_3_wOBA_Zones)
        
        
        Pitch_3_BA_Zones <- ggplot(Pitch_3_Zone_Summary, aes(x=PitchSideBin,y=PitchHeightBin,  fill= AVG))+
          geom_raster()+
          scale_fill_gradient2(low="red", high="blue",mid = "white",midpoint = 0.251 ) +
          geom_rect(xmin= 1.5, xmax=4.5, ymin=1.5, ymax=4.5,color='black', alpha=0)+
          ggtitle("Batting Average By Location") + xlab("Blue=Bad for Pitcher, Red= Good for Pitcher") +ylab("")+theme(plot.title = element_text(hjust = 0.5))
        
        output$Pitch3_BA <- renderPlot(Pitch_3_BA_Zones)
        
        
        
      }
      
      ##RETURN A BLANK PLOT IF HE HAS 2 OR LESS PITCHES##
      if(length(pitch_for_plot$pitch_name)<3){
        empty <- data.frame(plate_x=numeric(),plate_z=numeric())
        output$P3plot <- renderPlot("")
        output$Pitch3_SwMiss <- renderPlot("")
        output$Pitch3_HardHit <- renderPlot("")
        output$Pitch3_wOBA <- renderPlot("")
        output$Pitch3_BA <- renderPlot("")
        output$Pitch3_Zones_Title <- renderText("")
      }
      
      
      
      
      
      ##CREATE ZONE SUMMARIES FOR HIS 4TH HIGHEST USED PITCH IF HE HAS ONE##
      if(length(pitch_for_plot$pitch_name)>3){
        Pitch4 <- paste(pitch_for_plot[4,1],sep = "")
        
        pitch4_data <- pitcher %>% filter(pitch_name==Pitch4)
        
        title4 <- paste("------------------------------ ",Pitch4," 2024 Results", "------------------------------",sep = "")
        
        output$Pitch4_Zones_Title <- renderText(title4)
        
        
        
        output$P4plot <- renderPlot(smoothScatter(x=pitch4_data$plate_x_adj,y=pitch4_data$plate_z,colramp=myColRamp,main=Pitch4,xlab = "Side",ylab = "Height", xlim = c(-2,2), ylim = c(1,4))+ rect(xleft = (-0.92), ybottom = 1.6,xright = 0.92,ytop = 3.6))
        
        
        
        
        
        Pitch_4_Zone_Summary <- pitch4_data %>% group_by(PitchHeightBin,PitchSideBin) %>% summarise(Pitches=n(),
                                                                                                    wOBA=(0.883*sum(IsSingle) + 1.238*sum(IsDouble) + 1.558*sum(IsTriple) + 1.979*sum(IsHomerun) + 0.699*sum(IsWalk)+0.728*sum(IsHBP))/(sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun)+sum(IsOut)+sum(IsWalk)+sum(IsHBP)),
                                                                                                    SwMissPCT=sum(IsSwMiss)/sum(IsSwing),
                                                                                                    HH=sum(Is95mphEV)/Pitches,
                                                                                                    AVG=(sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun))/(sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun)+sum(IsOut)))
        
        Pitch_4_Sw_Zones <- ggplot(Pitch_4_Zone_Summary, aes(x=PitchSideBin,y=PitchHeightBin,  fill= SwMissPCT))+
          geom_raster()+
          scale_fill_gradient2(low="blue", high="red", mid = "white", midpoint =0.25 ) +
          geom_rect(xmin= 1.5, xmax=4.5, ymin=1.5, ymax=4.5,color='black', alpha=0)+
          ggtitle("SM% By Location") + xlab("Blue=Bad for Pitcher, Red= Good for Pitcher") +ylab("")+theme(plot.title = element_text(hjust = 0.5))
        
        output$Pitch4_SwMiss <- renderPlot(Pitch_4_Sw_Zones)
        
        
        
        
        Pitch_4_HH_Zones <- ggplot(Pitch_4_Zone_Summary, aes(x=PitchSideBin,y=PitchHeightBin,  fill= HH))+
          geom_raster()+
          scale_fill_gradient2(low="red", high="blue",mid = "white",midpoint = 0.05) +
          geom_rect(xmin= 1.5, xmax=4.5, ymin=1.5, ymax=4.5,color='black', alpha=0)+
          ggtitle("Hard Hit Per Pitch By Location") + xlab("Blue=Bad for Pitcher, Red= Good for Pitcher") +ylab("")+theme(plot.title = element_text(hjust = 0.5))
        
        output$Pitch4_HardHit <- renderPlot(Pitch_4_HH_Zones)
        
        
        
        Pitch_4_wOBA_Zones <- ggplot(Pitch_4_Zone_Summary, aes(x=PitchSideBin,y=PitchHeightBin,  fill= wOBA))+
          geom_raster()+
          scale_fill_gradient2(low="red", high="blue",mid = "white",midpoint = 0.325) +
          geom_rect(xmin= 1.5, xmax=4.5, ymin=1.5, ymax=4.5,color='black', alpha=0)+
          ggtitle("wOBA By Location") + xlab("Blue=Bad for Pitcher, Red= Good for Pitcher") +ylab("")+theme(plot.title = element_text(hjust = 0.5))
        
        output$Pitch4_wOBA <- renderPlot(Pitch_4_wOBA_Zones)
        
        
        Pitch_4_BA_Zones <- ggplot(Pitch_4_Zone_Summary, aes(x=PitchSideBin,y=PitchHeightBin,  fill= AVG))+
          geom_raster()+
          scale_fill_gradient2(low="red", high="blue",mid = "white",midpoint = 0.251 ) +
          geom_rect(xmin= 1.5, xmax=4.5, ymin=1.5, ymax=4.5,color='black', alpha=0)+
          ggtitle("Batting Average By Location") + xlab("Blue=Bad for Pitcher, Red= Good for Pitcher") +ylab("")+theme(plot.title = element_text(hjust = 0.5))
        
        output$Pitch4_BA <- renderPlot(Pitch_4_BA_Zones)
        
        
      }
      
      ##RETURN A BLANK PLOT IF HE HAS 3 PITCHES OR LESS##
      if(length(pitch_for_plot$pitch_name)<4){
        empty <- data.frame(plate_x=numeric(),plate_z=numeric())
        output$P4plot <- renderPlot("")
        output$Pitch4_SwMiss <- renderPlot("")
        output$Pitch4_HardHit <- renderPlot("")
        output$Pitch4_wOBA <- renderPlot("")
        output$Pitch4_BA <- renderPlot("")
        output$Pitch4_Zones_Title <- renderText("")
      }
      
      
      
      ##GAME USAGE BY DATE PLOT CREATION##
      Usage_Summary <- pitcher %>% group_by(game_date, pitch_name) %>% summarise(Pitches=n())
      
      
      Usage_Summary <- Usage_Summary %>% group_by(game_date) %>% mutate(USG = 100* prop.table(Pitches))
      
      Usage_Plot <- ggplot(Usage_Summary, aes(game_date,USG,color=pitch_name))+geom_line() +xlab("Game Date") +ggtitle("2024 Pitch Usage by Outing")  + theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"))  + scale_color_discrete(name = "Pitch Type")
      
      
      
      sc_colors <- c(
        "4-Seam Fastball" = "#D22D49",
        "Fastball" = "#D22D49",
        "Sinker" = "#FE9D00",
        "Cutter" = "#933F2C",
        "Slider" = "#EEE716",
        "Sweeper" = "#DDB33A",
        "Slurve" = "#93AFD4",
        "Knuckle Curve" = "#6236CD",
        "Curveball" = "#00D1ED",
        "Split-Finger" = "#3BACAC",
        "Forkball" = "#55CCAB",
        "Changeup" = "#1DBE3A",
        "ChangeUp" = "#1DBE3A")
      
      
      
     ##CALCULATE ARM ANGLE AND ADD IT TO HIS MOVEMENT PLOT##
      
      Height=pitcher$height_ft[1]
      
      dat1 <- pitcher %>%
        group_by(mlb_name) %>%
        dplyr::reframe(pitch_name = pitch_name,
                       VB = VB,
                       HB = HB,
                       release_pos_z = mean(release_pos_z, na.rm = TRUE),
                       release_pos_x = mean(-1*release_pos_x, na.rm = TRUE),
                       adj = release_pos_z - Height * 0.7,
                       opp = abs(-1*release_pos_x),
                       hyp = sqrt(opp^2 + adj^2),
                       radian = acos(((adj^2) + (hyp^2) - (opp^2)) / (2*(adj * hyp))),
                       Angle = radian * 57.295779513,
                       Angle = 90 - Angle,
                       degree = case_when(p_throws == 'R' ~ Angle * pi / 180,
                                          p_throws == 'L' ~ -Angle * pi / 180),
                       slope = tan(degree))
      
      
      plot <- ggplot(data = dat1, aes(x = HB, y = VB, color = pitch_name)) +
        geom_point(size = 2, na.rm = TRUE) +
        geom_segment(aes(x = 0, y = -25, xend = 0, yend = 25), linewidth = 1, color = "grey55") +
        geom_segment(aes(x = -25, y = 0, xend = 25, yend = 0), linewidth = 1, color = "grey55") +
        geom_abline(slope = dat1$slope) + labs(caption = paste("Arm Angle:", round(dat1$Angle, digits = 1), "Degrees")) +
        ggtitle(paste(dat1$Pitcher), "Pitcher POV") + scale_color_manual(values = sc_colors)
      
      
      
      
      
      
      
      
      
      
      
      output$BreakPlot <- renderPlot(plot)
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
     
      ## RELEASE POINT PLOT
      Release_Plot <- ggplot(pitcher,aes(x=(-1)*release_pos_x,y=release_pos_z, color=pitch_name)) +geom_point() +xlab("Release Side") + ylab("Release Height") +ggtitle("2024 Release Point Consistency") + theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold")) + scale_color_discrete(name = "Pitch Type") 
      
      ## CREATE ROLLING VELO/HB/VB PLOTS##
      pitcher <- pitcher %>% arrange(game_date,at_bat_number,pitch_number)
      
      pitcher <- pitcher %>% mutate(pitchID = row_number())
      
    
      pitcher_outings <- pitcher %>% group_by(game_date, pitch_name) %>% summarise(Velo=mean(release_speed), Vert=mean(VB), Horz=mean(HB))
      
      rolling_velo <- ggplot(pitcher_outings, aes(x=game_date, y=Velo,color=pitch_name)) + geom_line() + ggtitle("Velocity") + xlab("Pitch Number") + ylab("Velocity")+ theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"))  + scale_color_discrete(name = "Pitch Type")
      
      rolling_VB <- ggplot(pitcher_outings, aes(x=game_date, y=Vert,color=pitch_name)) + geom_line() + ggtitle("Vertical Break") + xlab("Pitch Number") + ylab("VB")+ theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"))  + scale_color_discrete(name = "Pitch Type")
      
      rolling_HB <- ggplot(pitcher_outings, aes(x=game_date, y=Horz,color=pitch_name)) + geom_line() + ggtitle("Horizontal Break") + xlab("Pitch Number") + ylab("HB")+ theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"))  + scale_color_discrete(name = "Pitch Type")
      
      
      ##CREATE DISTRIBUTION PLOTS##
      speed_dist <- ggplot(pitcher,aes(x=release_speed,group=pitch_name,fill=pitch_name))+geom_density(adjust=1.5, alpha=0.4) +ggtitle("Release Speed Density") +xlab("Release Speed") + ylab("Probability") + scale_color_discrete(name = "Pitch Type") + theme(plot.title = element_text(hjust = 0.5))  + theme_ipsum(base_family = "roboto-condensed")
      
      HM_dist <- ggplot(pitcher,aes(x=HB,group=pitch_name,fill=pitch_name))+geom_density(adjust=1.5, alpha=0.4) +ggtitle("HB Density") +xlab("Horizontal Break") + ylab("Probability") + scale_color_discrete(name = "Pitch Type") + theme(plot.title = element_text(hjust = 0.5)) +  theme_ipsum(base_family = "roboto-condensed")
      
      VM_dist <- ggplot(pitcher,aes(x=VB,group=pitch_name,fill=pitch_name))+geom_density(adjust=1.5, alpha=0.4) +ggtitle("VB Density") +xlab("Induced Vertical Break") + ylab("Probability") + scale_color_discrete(name = "Pitch Type") + theme(plot.title = element_text(hjust = 0.5)) +  theme_ipsum(base_family = "roboto-condensed")
      
      RelSide_dist <- ggplot(pitcher,aes(x=(-1)*release_pos_x,group=pitch_name,fill=pitch_name))+geom_density(adjust=1.5, alpha=0.4) +ggtitle("Release Side Density") +xlab("Release Side") + ylab("Probability") + scale_color_discrete(name = "Pitch Type") + theme(plot.title = element_text(hjust = 0.5)) +  theme_ipsum(base_family = "roboto-condensed")
      
      RelHeight_dist <- ggplot(pitcher,aes(x=release_pos_z,group=pitch_name,fill=pitch_name))+geom_density(adjust=1.5, alpha=0.4) +ggtitle("Release Height Density") +xlab("Release Height") + ylab("Probability") + scale_color_discrete(name = "Pitch Type") + theme(plot.title = element_text(hjust = 0.5)) +  theme_ipsum(base_family = "roboto-condensed")
      
      
      #CREATE BIP METRICS AND PLOTS##
      pitcher_BIP <- pitcher %>% filter(IsInPlay=="1")
      pitcher_BIP <- pitcher_BIP %>% filter(is.na(launch_speed)==FALSE & is.na(launch_angle)==FALSE)
      
      
      ES_dist <- ggplot(pitcher_BIP,aes(x=launch_speed))+geom_density(adjust=1.5,fill="lightblue", alpha=0.4) + geom_vline(xintercept=mean(pitcher_BIP$launch_speed), color="blue", linetype="dashed", size=1.5)  +ggtitle("Exit Velocity Density") +xlab("Exit Velo") + ylab("Probability") + theme(plot.title = element_text(hjust = 0.5))+  theme_ipsum(base_family = "roboto-condensed")
      Angle_dist <- ggplot(pitcher_BIP,aes(x=launch_angle))+geom_density(adjust=1.5,fill="red", alpha=0.4) + geom_vline(xintercept=mean(pitcher_BIP$launch_angle), color="blue", linetype="dashed", size=1.5)  +ggtitle("Launch Density") +xlab("Launch Angle") + ylab("Probability") + theme(plot.title = element_text(hjust = 0.5)) + theme_ipsum(base_family = "roboto-condensed") 
      
      
      ##QUERY TOOL CODE##
      observeEvent(input$go,{
        Pitcher_Name <- input$Pitcher
        
        pitcher <- read_feather("pitcher_profile.feather")
        
        pitcher <- pitcher %>% filter(mlb_name==Pitcher_Name)
        
        
        pitcher$game_date <- as.Date(pitcher$game_date)
        
        ##DEFINE THE PARAMETERS TO FILTER BY WITH YOUR QUERY##
        date1 <- input$start_date
        date2<- input$end_date
        
        pitcher <- pitcher %>% filter(game_date>= date1 & game_date <= date2 )
        
        Velo_Low <- input$VeloMin
        Velo_High <- input$VeloMax
        HB_Low<- input$HBMin
        HB_High<- input$HBMax
        VB_Low<- input$VBMin
        VB_High<- input$VBMax
        RH_High<- input$RHMax
        RH_Low<- input$RHMin
        RS_Low<- (-1)* input$RSMax
        RS_High<- (-1)*input$RSMin
        
        pitcher <- pitcher %>% filter( release_speed> Velo_Low & release_speed < Velo_High  )
        pitcher <- pitcher %>% filter( release_pos_z> RH_Low & release_pos_z < RH_High  )
        pitcher <- pitcher %>% filter( release_pos_x> RS_Low & release_pos_x < RS_High  )
        pitcher <- pitcher %>% filter( VB> VB_Low & VB < VB_High  )
        pitcher <- pitcher %>% filter( HB> HB_Low & HB < HB_High  )
        
        
        
        
        ##FILTER BY BALLS##
        if(input$balls != "Any"){
          ball_num=input$balls
          pitcher <- pitcher %>% filter(balls == ball_num)
        }
        
        ##FILTER BY STRIKES##
        if(input$strikes != "Any"){
          strike_num=input$strikes
          if(strike_num=="<2"){
            pitcher <- pitcher %>% filter(strikes == "1" | strikes=="0")
          }
          if(strike_num !="<2"){
            
            pitcher <- pitcher %>% filter(strikes == strike_num)
          }
        }
        
        
        ##FILTER BY OUTS##
        if(input$outs != "Any"){
          out_num=input$outs
          if(out_num=="<2"){
            pitcher <- pitcher %>% filter(outs_when_up == "1" | outs_when_up=="0")
          }
          if(out_num !="<2"){
            
            pitcher <- pitcher %>% filter(outs_when_up == out_num)
          }
        }
        
        ##FILTER BY RUNNERS##
        if(input$runners !="Any"){
          
          runner_state=input$runners
          if(runner_state=="Empty"){
            pitcher <- pitcher %>% filter(Runners=="000")
          }
          if(runner_state=="1st Only"){
            pitcher <- pitcher %>% filter(Runners=="100")
          }  
          if(runner_state=="2nd Only"){
            pitcher <- pitcher %>% filter(Runners=="010")
          }
          if(runner_state=="3rd Only"){
            pitcher <- pitcher %>% filter(Runners=="001")
          }
          if(runner_state=="1st and 2nd"){
            pitcher <- pitcher %>% filter(Runners=="110")
          }
          if(runner_state=="1st and 3rd"){
            pitcher <- pitcher %>% filter(Runners=="101")
          }
          if(runner_state=="2nd and 3rd"){
            pitcher <- pitcher %>% filter(Runners=="011")
          }
          if(runner_state=="Bases Loaded"){
            pitcher <- pitcher %>% filter(Runners=="111")
          }
          if(runner_state=="Scoring Pos"){
            pitcher <- pitcher %>% filter(Runners=="010"|Runners=="001"|Runners=="110"|Runners=="101"|Runners=="011"|Runners=="111")
          }
          if(runner_state=="Not Empty"){
            pitcher <- pitcher %>% filter(Runners!="000")
          }
          
          
          
        }
        
        ##FILTER BY BATTER HANDEDNESS##
        if(input$batter_hand !="Any"){
          if(input$batter_hand=="Right"){
            pitcher <- pitcher %>% filter(stand=="R")
          }
          if(input$batter_hand=="Left"){
            pitcher <- pitcher %>% filter(stand=="L")
          }
          
        }
        
        
        ##FILTER BY HOME/AWAY##
        if(input$home !="Any"){
          if(input$home=="Home"){
            pitcher <- pitcher %>% filter(inning_topbot =="Top")
          }
          if(input$home=="Away"){
            pitcher <- pitcher %>% filter(inning_topbot=="Bottom")
          }
          
        }
        
        ##FILTER BY PITCH TYPE##
        if(input$pitch_type != "Any"){
          
          
          pitcher <- pitcher %>% filter(pitch_name== input$pitch_type )
        }
        
        ##FILTER BY BIP TYPE##
        if(input$BIP_Type[1]!="Any"){
          pitcher <- pitcher %>% filter(bb_type %in% input$BIP_Type)
        }
        
        
        ##FILTER BY PITCH LOCATION##
        if(input$pitch_location !="Any"){
          
          pitcher$PitchHeightBin <- ifelse(pitcher$plate_z<1.6,1,
                                           ifelse(pitcher$plate_z>=1.6 & pitcher$plate_z<2.25,2,
                                                  ifelse(pitcher$plate_z>=2.25 & pitcher$plate_z<2.93,3,
                                                         ifelse(pitcher$plate_z>=2.93 & pitcher$plate_z<3.6,4,
                                                                ifelse(pitcher$plate_z>=3.6 ,5,"Error")))))
          
          
          
          pitcher$PitchSideBin <- ifelse(pitcher$plate_x<(-0.92),5,
                                         ifelse(pitcher$plate_x>=(-0.92) & pitcher$plate_x<(-0.31),4,
                                                ifelse(pitcher$plate_x>=(-.31) & pitcher$plate_x<0.31,3,
                                                       ifelse(pitcher$plate_x>=0.31 & pitcher$plate_x<0.92,2,
                                                              ifelse(pitcher$plate_x>=0.92 ,1,"Error")))))
          
          
          
          
          
          
          
          
          
          
          if(input$pitch_location=="Up"){
            pitcher <- pitcher %>% filter((PitchSideBin=="2" |PitchSideBin=="3"|PitchSideBin=="4") & (PitchHeightBin=="4"))
          }
          
          
          if(input$pitch_location=="Middle"){
            pitcher <- pitcher %>% filter((PitchSideBin=="2" |PitchSideBin=="3"|PitchSideBin=="4") & (PitchHeightBin=="3"))
          }
          
          if(input$pitch_location=="Down"){
            pitcher <- pitcher %>% filter((PitchSideBin=="2" |PitchSideBin=="3"|PitchSideBin=="4") & (PitchHeightBin=="2"))
          }
          
          if(input$pitch_location=="Below Zone"){
            pitcher <- pitcher %>% filter(PitchHeightBin=="1")
          }
          
          if(input$pitch_location=="Above Zone"){
            pitcher <- pitcher %>% filter(PitchHeightBin=="5")
          }
          
          if(input$pitch_location=="Out of Zone"){
            pitcher <- pitcher %>% filter(IsInZone=="0")
          }
          
          if(input$pitch_location=="In Zone"){
            pitcher <- pitcher %>% filter(IsInZone=="1")
          }
          
          if(input$pitch_location=="Center"){
            pitcher <- pitcher %>% filter((PitchHeightBin=="2"|PitchHeightBin=="3" | PitchHeightBin=="4")& PitchSideBin=="3")
          }
          
          if(input$pitch_location=="Armside"){
            if(pitcher$p_throws[1]=="R"){
              pitcher <- pitcher %>% filter(PitchSideBin=="4"|PitchSideBin=="5")
            }
            
            if(pitcher$p_throws[1]=="L"){
              pitcher <- pitcher %>% filter(PitchSideBin=="1"|PitchSideBin=="2")
            }
          }
          
          if(input$pitch_location=="Gloveside"){
            if(pitcher$p_throws[1]=="L"){
              pitcher <- pitcher %>% filter(PitchSideBin=="4"|PitchSideBin=="5")
            }
            
            if(pitcher$p_throws[1]=="R"){
              pitcher <- pitcher %>% filter(PitchSideBin=="1"|PitchSideBin=="2")
            }
          }
          
          
          if(input$pitch_location=="In"){
            pitcher$IsIn <- ifelse(pitcher$stand=="R" & pitcher$PitchSideBin=="4" &(pitcher$PitchHeightBin=="2" | pitcher$PitchHeightBin=="3" | pitcher$PitchHeightBin=="4"),1,
                                   ifelse(pitcher$stand=="L" & pitcher$PitchSideBin=="2" &(pitcher$PitchHeightBin=="2" | pitcher$PitchHeightBin=="3" | pitcher$PitchHeightBin=="4"),1,0))
            
            pitcher <- pitcher %>% filter(IsIn=="1")
          }
          
          if(input$pitch_location=="In (Ball)"){
            pitcher$IsInBall <- ifelse(pitcher$stand=="R" & pitcher$PitchSideBin=="5" ,1,
                                       ifelse(pitcher$stand=="L" & pitcher$PitchSideBin=="1" ,1,0))
            
            pitcher <- pitcher %>% filter(IsInBall=="1")
          }
          
          if(input$pitch_location=="Away (Ball)"){
            pitcher$IsOutBall <- ifelse(pitcher$stand=="R" & pitcher$PitchSideBin=="1" ,1,
                                        ifelse(pitcher$stand=="L" & pitcher$PitchSideBin=="5" ,1,0))
            
            pitcher <- pitcher %>% filter(IsOutBall=="1")
          }
          
          
          if(input$pitch_location=="Away"){
            pitcher$IsAway <- ifelse(pitcher$stand=="L" & pitcher$PitchSideBin=="4" &(pitcher$PitchHeightBin=="2" | pitcher$PitchHeightBin=="3" | pitcher$PitchHeightBin=="4"),1,
                                     ifelse(pitcher$stand=="R" & pitcher$PitchSideBin=="2" &(pitcher$PitchHeightBin=="2" | pitcher$PitchHeightBin=="3" | pitcher$PitchHeightBin=="4"),1,0))
            
            pitcher <- pitcher %>% filter(IsAway=="1")
          }
          
          
        }
       
         ##FILTER BY PLATE APPEARANCE RESULT##
        if(input$PA_result[1] != "Any"){
          pitcher$event_simple <- ifelse(pitcher$events=="sac_fly_double_play","sac_fly",
                                         ifelse(pitcher$events=="sac_bunt_double_play","sac_bunt",
                                                ifelse(pitcher$events=="double_play" | pitcher$events=="triple_play" | pitcher$events=="grounded_into_double_play" | pitcher$events=="force_out" | pitcher$events=="fielders_choice_out" | pitcher$events=="fielders_choice" ,"field_out", pitcher$events)))
          pitcher <- pitcher %>% filter(event_simple %in% input$PA_result)
        }
        
        ##FILTER BY PITCH RESULT##
        if(input$Pitch_results[1] != "Any"){
          pitcher$description_simple <- ifelse(pitcher$description=="swinging_strike_blocked","swinging_strike",
                                               ifelse(pitcher$description=="blocked_ball","ball",
                                                      ifelse(pitcher$description=="hit_into_play_no_out" | pitcher$description=="hit_into_play_score","hit_into_play", pitcher$description)))
          pitcher <- pitcher %>% filter(description_simple %in% input$Pitch_results)
        }
        
        
        
        
        
        pitcher$adj_EV <- ifelse(is.na(pitcher$launch_speed)==TRUE,0,(pitcher$launch_speed)*pitcher$IsInPlay)
        
        pitcher$adj_LA <- ifelse(is.na(pitcher$launch_angle)==TRUE,0,(pitcher$launch_angle)*pitcher$IsInPlay)
        
        ##CREATE A SUMMARY OF THE FILTERED DATA##
        filtered_summary <- pitcher %>% group_by(pitch_name) %>% summarise(Pitches=n(),
                                                                           AB=sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun)+sum(IsOut)+sum(IsError),
                                                                           BIP=sum(IsInPlay),
                                                                           Strikepct=100*(sum(IsSwing)+sum(IsCalledStrike))/Pitches,
                                                                           Swingpct=100*sum(IsSwing)/Pitches,
                                                                           SMpct=100*sum(IsSwMiss)/sum(IsSwing),
                                                                           CSW=100*(sum(IsSwMiss)+sum(IsCalledStrike))/Pitches,
                                                                           wOBA=(0.883*sum(IsSingle) + 1.238*sum(IsDouble) + 1.558*sum(IsTriple) + 1.979*sum(IsHomerun) + 0.699*sum(IsWalk)+0.728*sum(IsHBP))/(AB+sum(IsWalk)+sum(IsHBP)),
                                                                           xwOBA=(0.883*sum(xSingle_EVLA) + 1.238*sum(xDouble_EVLA) + 1.558*sum(xTriple_EVLA) + 1.979*sum(xHomeRun_EVLA) + 0.699*sum(IsWalk)+0.728*sum(IsHBP))/(AB+sum(IsWalk)+sum(IsHBP)),
                                                                           AVG=(sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun))/AB,
                                                                           OBP=(sum(IsSingle)+sum(IsDouble)+sum(IsTriple)+sum(IsHomerun)+sum(IsWalk)+sum(IsHBP))/(AB+sum(IsWalk)+sum(IsHBP)),
                                                                           SLG=(sum(IsSingle)+2*sum(IsDouble)+3*sum(IsTriple)+4*sum(IsHomerun))/AB,
                                                                           EV= sum(adj_EV)/BIP,
                                                                           HHpct= 100*sum(Is95mphEV)/BIP,
                                                                           LA=sum(adj_LA)/BIP,
                                                                           Velo=mean(release_speed),
                                                                           VM=mean(VB),
                                                                           HM=mean(HB),
                                                                           RH=mean(release_pos_z),
                                                                           RS=(-1)*mean(release_pos_x))
        
        
        
        filtered_summary<- filtered_summary %>% mutate(USG=100*Pitches/sum(Pitches))
        
        filtered_summary <- filtered_summary %>% arrange(desc(USG))
        
        
        
        filtered_summary_1 <- filtered_summary %>% select(pitch_name, USG,Velo,VM,HM,RH,RS, Strikepct, Swingpct, SMpct,CSW)
        
        filtered_summary_2 <- filtered_summary %>% select(pitch_name, AB,BIP,wOBA,xwOBA,AVG,OBP,SLG,EV,HHpct,LA)
        
        
        filtered_summary_2$AVG <- format(filtered_summary_2$AVG, digits = 3)
        filtered_summary_2$wOBA <- format(filtered_summary_2$wOBA, digits = 3)
        filtered_summary_2$xwOBA <- format(filtered_summary_2$xwOBA, digits = 3)
        filtered_summary_2$SLG <- format(filtered_summary_2$SLG, digits = 3)
        filtered_summary_2$OBP <- format(filtered_summary_2$OBP, digits = 3)
        
        
        
        
        names(filtered_summary_1)[names(filtered_summary_1) == "pitch_name"] <- "Pitch Type"
        names(filtered_summary_2)[names(filtered_summary_2) == "pitch_name"] <- "Pitch Type"  
        names(filtered_summary_1)[names(filtered_summary_1) == "Swingpct"] <- "Swing%"
        names(filtered_summary_1)[names(filtered_summary_1) == "SMpct"] <- "SM%"
        names(filtered_summary_2)[names(filtered_summary_2) == "HHpct"] <- "HH%"
        
        names(filtered_summary_1)[names(filtered_summary_1) == "Strikepct"] <- "STR%"
        
        filtered_summary_2$AB <-as.integer(filtered_summary_2$AB)
        filtered_summary_2$BIP <-as.integer(filtered_summary_2$BIP)
        
        
        
        
        output$query_table <- renderTable( filtered_summary_1 )
        
        output$query_table2 <- renderTable( filtered_summary_2 )
        
        ## MAKE A PLOT OF THE FILTERED DATA##
        filter_plot <- ggplot(pitcher, aes(x=(-1)*plate_x, y=plate_z, color=pitch_name))+geom_point()+ xlim(-2,2) + ylim(0,5) + xlab("") + ylab("") +ggtitle("Pitchers View") + scale_color_discrete(name = "Pitch Type") + geom_rect(xmin= -0.94, xmax=0.94, ymin=1.6, ymax=3.6,color='black', alpha=0)+ theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"))
        
        output$query_plot <- renderPlot(filter_plot)
        
      })
      
      
      
      
      
      
      
      ##OUTPUT ALL THE ITEMS WE CREATED##
      output$Name_and_Team <- renderText(page_top)
      output$Table1_Title <- renderText("---------- 2024 Pitch Characteristics ----------")
      output$PC_table <- renderTable(pitch_characteristics, rownames = FALSE, striped = TRUE)
      
      output$Plots_Title <- renderText("----------------------- 2024 Percentiles -----------------------")
      
      output$Table2_Title <- renderText("------------------------------------------------------- 2024 Pitch Type Results -------------------------------------------------------")
      
      output$PR_table <- renderTable(pitch_results, rownames = FALSE, striped = TRUE)
      output$Table3_Title <- renderText("----------------------------------------------------- Overall Results By Year -----------------------------------------------------")
      output$Results_table <- renderTable(Results, rownames = FALSE, striped = TRUE)
      
      
      output$Percentile_text <- renderText("------------------------------------------------------- 2024 Pitch Locations -------------------------------------------------------")
      
      
      
      
      
      output$P1plot <- renderPlot(smoothScatter(x=pitch1_data$plate_x_adj,y=pitch1_data$plate_z,colramp=myColRamp,main=Pitch1,xlab = "Side",ylab = "Height", xlim = c(-2,2), ylim = c(1,4))+ rect(xleft = (-0.92), ybottom = 1.6,xright = 0.92,ytop = 3.6))
      
      
      output$ReleasePlot <- renderPlot(Release_Plot)
      
      output$Velo_Time <- renderPlot(rolling_velo)
      output$VB_Time <- renderPlot(rolling_VB)
      output$HB_Time <- renderPlot(rolling_HB)
      output$velo_dist <- renderPlot(speed_dist)
      output$VB_dist <- renderPlot(VM_dist)
      output$HB_dist <- renderPlot(HM_dist)
      output$RH_dist <- renderPlot(RelHeight_dist)
      output$RS_dist <- renderPlot(RelSide_dist)
      output$EV_dist <- renderPlot(ES_dist)
      output$LA_dist <- renderPlot(Angle_dist)
      output$Kgauge <- renderGauge(Gauge1)
      
      output$BBgauge <- renderGauge(Gauge2)
      output$HRgauge <- renderGauge(Gauge3)
      output$wOBAgauge <- renderGauge(Gauge4)
      output$xwOBAgauge <- renderGauge(Gauge5)
      output$FBVgauge <- renderGauge(Gauge6)
      output$USG_Lines <- renderPlot(Usage_Plot)
      
    }}
  )}

