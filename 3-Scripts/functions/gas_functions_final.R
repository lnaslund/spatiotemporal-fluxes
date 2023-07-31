#############
file.finder <- function(data.path=".", start="1800-01-01", end="2200-01-01", type = "flux"){
  if(type == "flux"){
    ext <- "f00"
  }
  if(type == "temperature"){
    ext <- "temp"
  }

  desired_folders <- list.dirs(data.path, recursive=F) %>% 
    str_extract("\\d\\d\\d\\d.\\d\\d.\\d\\d") %>% 
    na.omit() %>%
    as_tibble() %>% 
    mutate(value=ymd(value)) %>% 
    filter(between(value, as.Date(start), as.Date(end))) %>% 
    mutate(paths = paste0(data.path, "/", value)) %>% 
    pull(paths)
  
  desired_files <- NULL
  for(j in 1:length(desired_folders)){
    prefix <- desired_folders[j]
    temp <- list.files(desired_folders[j])
    for(i in 1:length(temp)){
      if(str_detect(temp[i], ext)==FALSE){
        next
      }
      else{
        desired_files <- c(paste0(prefix, "/", temp[i]), desired_files)
      }
    }
  }
 
  return(desired_files)
  
}


##################
read.flux <- function(flux.files, time.zone){
  data <- NULL
  for(i in 1:length(flux.files)){
    tryCatch(
      exp = {
        temp <- read.delim2(flux.files[i], sep=",", skip=1, header=T, skipNul = TRUE) %>% 
          dplyr::select(SysTime, X.CH4.d_ppm, X.CO2.d_ppm) %>% 
          rename(date_time = SysTime, CH4_dry=X.CH4.d_ppm, CO2_dry = X.CO2.d_ppm) %>% 
          mutate(date_time = mdy_hms(date_time, tz=time.zone), CH4_dry = as.numeric(CH4_dry), CO2_dry = as.numeric(CO2_dry))
        
        data <- rbind(data, temp)
      },
      error = function(e){
        message(paste0("Read error for file:", flux.files[i]))
      }
    )
  }
  return(data)
}

#############
read.temp <- function(temp.files, time.zone){
  temp_data <- NULL
  for(i in 1:length(temp.files)){
    temp <- data.table::fread(temp.files[i], skip=2, header=T)
    
    if( "Temp, (*C)" %in% names(temp)){
      temp <- temp %>% rename("date_time" = "Date Time, GMT -0400", "temp" = "Temp, (*C)") %>% 
        dplyr::select(date_time, temp) %>% 
        mutate(date_time = ymd_hms(date_time, tz = time.zone), temp_k = temp+273.15)
    }
    
    else{
      temp <- temp %>% 
        rename("date_time" = "Date Time, GMT -0400", temp = "Temp, (*F)") %>% 
        dplyr::select(date_time, temp) %>% 
        mutate(date_time = ymd_hms(date_time, tz = time.zone), temp_k = (temp-32)*(5/9)+273.15)
    }
    
    
    temp_data <- rbind(temp_data, temp)
  }
  return(temp_data)
}

##################
return.ts <- function(df, time.stamp, time.zone){
  ret <-as.POSIXct(rep(NA, length(time.stamp)))
  for(i in 1:length(time.stamp)){
    temp <- df %>% dplyr::filter(floor_date(date_time)==floor_date(ymd_hms(time.stamp[i], tz = time.zone)))
    ret[i] <- temp[nrow(temp), 1]
  }
  return(ret)
}

#################
# Calculates slopes using slr

calc.slopes.slr <- function(data_df, times_df, time.zone, perc_cut=0.2){
  flux_list <- list()

  flux_list$site <- times_df$Site
  flux_list$location <- times_df$Location
  flux_list$start <- times_df$Start
  flux_list$site_loc <- paste(times_df$Site, times_df$Location, times_df$Start, sep=" ")
  flux_list$start <- return.ts(data_df, times_df$Start, time.zone)
  flux_list$end <- return.ts(data_df, times_df$End, time.zone)

  data_filt <- list()
  ch4_plot <- list()
  co2_plot <- list()

    for(i in 1:length(flux_list$start)){

      if(flux_list$start[i]>flux_list$end[i] | is.na(seconds(flux_list$start[i])) | is.na(seconds(flux_list$end[i]))){
        data_filt[[i]] <- NULL
        flux_list$s_ch4[i] <- NA
        flux_list$i_ch4[i] <- NA
        flux_list$s_co2[i] <- NA
        flux_list$i_co2[i] <- NA
        ch4_plot[[i]] <- NULL
        co2_plot[[i]] <- NULL
        print(paste0("Timestamp error row ", i))
      }

      else{
        data_filt[[i]] <- data_df %>% filter(date_time > flux_list$start[i], date_time < flux_list$end[i]) %>%
          mutate(secs=as.numeric(seconds(date_time)), secs = secs-min(secs)) %>%
          mutate(points =if_else(row_number() %in% c((round(perc_cut*n(), digits=0):n())), "included", "excluded"))


        flux_list$s_ch4[i] <- summary(lm(CH4_dry ~ secs, data=data_filt[[i]] %>% filter(points=="included")))$coefficients[2,1]
        flux_list$i_ch4[i] <- summary(lm(CH4_dry ~ secs, data=data_filt[[i]] %>% filter(points=="included")))$coefficients[1,1]

        flux_list$s_co2[i] <- summary(lm(CO2_dry ~ secs, data=data_filt[[i]] %>% filter(points=="included")))$coefficients[2,1]
        flux_list$i_co2[i] <- summary(lm(CO2_dry ~ secs, data=data_filt[[i]] %>% filter(points=="included")))$coefficients[1,1]


        ch4_plot[[i]] <- ggplot(data_filt[[i]], aes(secs, CH4_dry))+
          geom_point(aes(col=points))+
          scale_color_manual(values=c("black", "grey"))+
          geom_abline(intercept=flux_list$i_ch4[i], slope=flux_list$s_ch4[i], linetype="dashed")+
          ggtitle(paste(i, ":", flux_list$site_loc[i]))


        co2_plot[[i]] <- ggplot(data_filt[[i]], aes(secs, CO2_dry))+
          geom_point(aes(col=points))+
          scale_color_manual(values=c("black", "grey"))+
          geom_abline(intercept=flux_list$i_co2[i], slope=flux_list$s_co2[i], linetype="dashed")+
          ggtitle(paste(i, ":", flux_list$site_loc[i]))

      }


  }

  flux_list$data_filt <- data_filt
  flux_list$ch4_plot <- ch4_plot
  flux_list$co2_plot <- co2_plot

  return(flux_list)
}

####################################

point.slopes <- function(ts, num.pts){
  
  slope_list <- list()
  ts_list <- list()
  
  for(i in 1:(nrow(ts)-num.pts+1)){
    slope_list$start[i] <- i
    ind <- c(i:(i+num.pts-1))
    ts_list[[i]] <- ts %>% slice(ind)
    
    slope_list$slope_ch4[i] <- coef(lm(CH4_dry ~ secs, data=ts %>% slice(ind)))[[2]]
    slope_list$r2_ch4[i] <- summary(lm(CH4_dry ~ secs, data=ts %>% slice(ind)))$r.squared
    
    slope_list$slope_co2[i] <- coef(lm(CO2_dry ~ secs, data=ts %>% slice(ind)))[[2]]
    slope_list$r2_co2[i] <- summary(lm(CO2_dry ~ secs, data=ts %>% slice(ind)))$r.squared
  }
  
  slope_list$data <- ts_list
  slope_list$dens_ch4 <- density(slope_list$slope_ch4, na.rm=T) 
  slope_list$dens_co2 <- density(slope_list$slope_co2, na.rm=T) 
  slope_list$dens_max_ch4 <- slope_list$dens_ch4$x[which.max(slope_list$dens_ch4$y)]
  slope_list$dens_max_co2 <- slope_list$dens_co2$x[which.max(slope_list$dens_co2$y)]
  slope_list$dens_plot_ch4 <- ggplot(data.frame(slope=slope_list$slope_ch4), aes(slope))+geom_density()+ geom_vline(xintercept=slope_list$dens_max_ch4)
  slope_list$dens_plot_co2 <- ggplot(data.frame(slope=slope_list$slope_co2), aes(slope))+geom_density()+ geom_vline(xintercept=slope_list$dens_max_co2)
  
  xx_ch4 <- slope_list$dens_ch4$x
  xx_co2 <- slope_list$dens_co2$x
  dx_ch4 <- xx_ch4[2]-xx_ch4[1]
  dx_co2 <- xx_co2[2]-xx_co2[1]
  yy_ch4 <-slope_list$dens_ch4$y
  yy_co2 <-slope_list$dens_co2$y
  
  max_ch4 <- slope_list$dens_max_ch4
  max_co2 <- slope_list$dens_max_co2
  
  C_ch4 <- sum(yy_ch4)*dx_ch4
  C_co2 <- sum(yy_co2)*dx_co2
  
  prob_ch4 <- NULL
  for(i in 1: length(xx_ch4)){
    p_ch4 <- (sum(yy_ch4[xx_ch4<= max_ch4 + i*dx_ch4])* dx_ch4)/C_ch4 - (sum(yy_ch4[xx_ch4 <= max_ch4 - i*dx_ch4])*dx_ch4)/C_ch4
    prob_ch4 <- c(prob_ch4, p_ch4)
  }
  
  prob_co2 <- NULL
  for(i in 1: length(xx_co2)){
    p_co2 <- (sum(yy_co2[xx_co2<= max_co2 + i*dx_co2])* dx_co2)/C_co2 - (sum(yy_co2[xx_co2 <= max_co2 - i*dx_co2])*dx_co2)/C_co2
    prob_co2 <- c(prob_co2, p_co2)
  }
  
  dev_ch4 <- which.min(abs(prob_ch4-0.1)) * dx_ch4
  dev_co2 <- which.min(abs(prob_co2-0.1)) * dx_co2
  
  top_ch4 <- which(slope_list$slope_ch4 > slope_list$dens_max_ch4 - dev_ch4) 
  top_co2 <- which(slope_list$slope_co2 > slope_list$dens_max_co2 - dev_co2) 
  bottom_ch4 <- which(slope_list$slope_ch4 < slope_list$dens_max_ch4 + dev_ch4)
  bottom_co2 <- which(slope_list$slope_co2 < slope_list$dens_max_co2 + dev_co2)
  
  int_ch4 <- intersect(top_ch4, bottom_ch4)
  int_co2 <- intersect(top_co2, bottom_co2)
  
  inds_ch4 <- NULL
  for(i in 1:length(int_ch4)){
    inds_ch4 <- c(inds_ch4, int_ch4[i]:(int_ch4[i]+num.pts-1))
  }
  inds_co2 <- NULL
  for(i in 1:length(int_co2)){
    inds_co2 <- c(inds_co2, int_co2[i]:(int_co2[i]+num.pts-1))
  }
  
  highlight_ch4 <- unique(inds_ch4)
  highlight_co2 <- unique(inds_co2)
  ts$num <- 1:nrow(ts)
  
  slope_list$plot_ch4 <- ts %>% mutate(hl = if_else(num %in% highlight_ch4, "include", "excluded")) %>% 
    ggplot(aes(secs, CH4_dry))+ geom_point(aes(col=hl)) + scale_color_manual(values=c("black", "green")) + theme(legend.position = "none")
  
  
  slope_list$plot_co2 <- ts %>% mutate(hl = if_else(num %in% highlight_co2, "include", "excluded")) %>% 
    ggplot(aes(secs, CO2_dry))+ geom_point(aes(col=hl)) + scale_color_manual(values=c("black", "deepskyblue")) + theme(legend.position = "none")
  
  return(slope_list)
} 


#################################################
calc.slopes <- function(data_df, times_df, time.zone, num.pts=10){
  flux_list <- list()
  
  flux_list$site <- times_df$Location
  flux_list$location <- times_df$Trap
  flux_list$start <- times_df$Start
  flux_list$site_loc <- paste(times_df$Location, times_df$`Trap #`, times_df$Start, sep=" ")
  flux_list$start <- return.ts(data_df, times_df$Start, time.zone)
  flux_list$end <- return.ts(data_df, times_df$End, time.zone)
  
  data_filt <- list()
  ch4_plot <- list()
  co2_plot <- list()
  
  for(i in 1:length(flux_list$start)){
    
    if(flux_list$start[i]>flux_list$end[i] | is.na(seconds(flux_list$start[i])) | is.na(seconds(flux_list$end[i]))){
      data_filt[[i]] <- NULL
      flux_list$s_ch4[i] <- NA
      flux_list$i_ch4[i] <- NA
      flux_list$s_co2[i] <- NA
      flux_list$i_co2[i] <- NA
      ch4_plot[[i]] <- NULL
      co2_plot[[i]] <- NULL
      print(paste0("Timestamp error row ", i))
    }
    
    else{
      data_filt[[i]] <- data_df %>% filter(date_time > flux_list$start[i], date_time < flux_list$end[i]) %>% 
        mutate(secs=as.numeric(seconds(date_time)), secs = secs-min(secs))
      
      tryCatch(temp <- point.slopes(data_filt[[i]], num.pts), 
               error=function(e)
                 print(paste0("error time stamp number:", i)))
      
      flux_list$s_ch4[i] <- temp$dens_max_ch4
      
      flux_list$s_co2[i] <- temp$dens_max_co2
      
      ch4_plot[[i]] <- temp$plot_ch4 + ggtitle(paste0(i, ":", flux_list$site_loc[i], "  ", "\nm = ", round(flux_list$s_ch4[i],4)))
      
      co2_plot[[i]] <- temp$plot_co2 + ggtitle(paste0(i, ":", flux_list$site_loc[i], "  ", "\nm = ", round(flux_list$s_co2[i],4)))
      
    }
    
    
  }
  
  flux_list$data_filt <- data_filt
  flux_list$ch4_plot <- ch4_plot
  flux_list$co2_plot <- co2_plot
  
  return(flux_list)
}
######################################

shiny_edit <- function(flux.list = flux_list, ch4_ind_vec, co2_ind_vec){
  require(shiny)
  
  ui <- fluidPage(tags$h1("ABB Flux Data Processing"),
                  tabsetPanel(
                    tabPanel("CH4",
                             wellPanel(
                               actionButton("run_ch4", "Next Plot"),
                               actionButton("reject_ch4", "Reject Default Fit"),
                               actionButton("accept_ch4", "Accept Default Fit"),
                               textOutput("instr_ch4"),
                               textOutput("end_ch4"),
                               tags$head(tags$style("#end_ch4{color: red;
                                 font-size: 20px;
                                 }"))
                             ),
                             fluidRow(plotlyOutput("plot_ch4", width = "80%")),
                             fluidRow(plotlyOutput("subplot_ch4", width = "80%"))), 
                    
                    tabPanel("CO2",
                             wellPanel(
                               actionButton("run_co2", "Next Plot"),
                               actionButton("reject_co2", "Reject Default Fit"),
                               actionButton("accept_co2", "Accept Default Fit"),
                               textOutput("instr_co2"),
                               textOutput("end_co2"),
                               tags$head(tags$style("#end_co2{color: red;
                                 font-size: 20px;
                                 }"))
                             ),
                             fluidRow(plotlyOutput("plot_co2", width = "80%")),
                             fluidRow(plotlyOutput("subplot_co2", width = "80%")))))
  
  
  server <- function(input, output, session) {
    ### CH4 code
    
    count_ch4 <- 0
    
    msg_ch4 <- reactiveValues(message = NULL)
    
    observeEvent(input$run_ch4, {
      count_ch4 <<- count_ch4 + 1
      
      msg_ch4$message <- ""
      
      if (count_ch4 > length(needs_correct_ch4)) {
        output$end_ch4 <- renderText("There are no more plots.")
        return()
      }
      
      else{
        data_ch4 <- flux_list$data_filt[[needs_correct_ch4[count_ch4]]]
        title_ch4 <- flux_list$site_loc[[needs_correct_ch4[count_ch4]]]
        data_ch4$points <- "exclude"
        
        output$plot_ch4 <- renderPlotly({
          select_data_ch4 <-
            event_data("plotly_selected", source = paste0("select_ch4", count_ch4))
          
          if (!is.null(select_data_ch4)) {
            idx_ch4 <- select_data_ch4$pointNumber + 1
            data_ch4$points[idx_ch4] <- "include"
          }
          
          mod1_ch4 <- lm(CH4_dry ~ secs, data = data_ch4)
          b_ch4 <- summary(mod1_ch4)$coefficients[[1]]
          m_ch4 <- summary(mod1_ch4)$coefficients[[2]]
          r2_ch4 <- summary(mod1_ch4)$adj.r.squared
          
          y.q_ch4 <- (max(data_ch4$CH4_dry) - min(data_ch4$CH4_dry)) / 10
          y.m_ch4 <- max(data_ch4$CH4_dry) - y.q_ch4
          x.m_ch4 <- min(data_ch4$secs) + 10
          
          correct_vec_ch4_m[count_ch4] <<- m_ch4
          correct_vec_ch4_b[count_ch4] <<- b_ch4
          
          caption_ch4 <-
            paste(paste0("R2 = ", round(r2_ch4, digits = 2)),
                  paste0("y=", round(m_ch4, digits = 2), " x + " , round(b_ch4, digits = 2)),
                  sep = "\n")
          
          
          p_ch4 <-
            ggplot(data_ch4, aes(secs, CH4_dry, col = I(points)))  + geom_smooth(method = "lm", formula = "y~x")+ geom_point() +
            scale_color_manual(name = "", values = c("black", "blue")) + labs(x = "seconds", y =
                                                                                "CH4 (ppm)") + ggtitle(paste0(count_ch4, ":", title_ch4)) +
            annotate(
              "text",
              x = x.m_ch4,
              y = y.m_ch4,
              hjust = 0,
              label = caption_ch4,
              parse = T,
              size = 5
            )
          ggplotly(p_ch4, source = paste0("select_ch4", count_ch4))
          
        })
      }
    })
    
    
    observeEvent(input$accept_ch4, {
      msg_ch4$message <- "Default accepted. Please select next plot."
    })
    
    
    observeEvent(input$reject_ch4, {
      msg_ch4$message <- "Default rejected. Please select data to fit."
      
      output$subplot_ch4 <- renderPlotly({
        d <- event_data("plotly_selected", source = paste0("select_ch4", count_ch4))
        if (is.null(d))
          return()
        
        data_ch4 <- flux_list$data_filt[[needs_correct_ch4[count_ch4]]]
        data_filt <-
          data_ch4 %>% filter(row_number() %in% (d$pointNumber + 1))
        
        q_ch4 <- ggplot(data_filt, aes(secs, CH4_dry)) +
          geom_smooth(method = "lm", formula = "y~x") +
          geom_point(color = "blue")
        
        
        
        mod2_ch4 <- lm(CH4_dry ~ secs, data = data_filt)
        b2_ch4 <-  summary(mod2_ch4)$coefficients[[1]]
        m2_ch4 <- summary(mod2_ch4)$coefficients[[2]]
        r2.2_ch4 <- summary(mod2_ch4)$adj.r.squared
        
        correct_vec_ch4_m[count_ch4] <<- m2_ch4
        correct_vec_ch4_b[count_ch4] <<- b2_ch4
        
        y.q_ch4 <- (max(data_filt$CH4_dry) - min(data_filt$CH4_dry)) / 10
        y.m_ch4 <- max(data_filt$CH4_dry) - y.q_ch4
        x.m_ch4 <- min(data_filt$secs) + 3
        
        caption_ch4 <-
          paste(paste0("R2 = ", round(r2.2_ch4, digits = 2)),
                paste0("y=", round(m2_ch4, digits = 2), " x + " , round(b2_ch4, digits = 2)),
                sep = "\n")
        
        q_ch4 + annotate(
          "text",
          x = x.m_ch4,
          y = y.m_ch4,
          hjust = 0,
          label = caption_ch4,
          parse = T,
          size = 5
        ) + labs(x = "seconds", y = "CH4 (ppm)")
      })
    })
    
    output$instr_ch4 <- renderText({
      msg_ch4$message
    })
    
    ### CO2 code
    count_co2 <- 0
    
    msg_co2 <- reactiveValues(message = NULL)
    
    observeEvent(input$run_co2, {
      count_co2 <<- count_co2 + 1
      
      msg_co2$message <- ""
      
      if (count_co2 > length(needs_correct_co2)) {
        output$end_co2 <- renderText("There are no more plots.")
        return()
      }
      
      else{
        data_co2 <- flux_list$data_filt[[needs_correct_co2[count_co2]]]
        title_co2 <- flux_list$site_loc[[needs_correct_co2[count_co2]]]
        data_co2$points <- "exclude"
        
        output$plot_co2 <- renderPlotly({
          select_data <-
            event_data("plotly_selected", source = paste0("select", count_co2))
          
          if (!is.null(select_data)) {
            idx <- select_data$pointNumber + 1
            data_co2$points[idx] <- "include"
          }
          
          mod1_co2 <- lm(CO2_dry ~ secs, data = data_co2)
          b_co2 <- summary(mod1_co2)$coefficients[[1]]
          m_co2 <- summary(mod1_co2)$coefficients[[2]]
          r2_co2 <- summary(mod1_co2)$adj.r.squared
          
          y.q_co2 <- (max(data_co2$CO2_dry) - min(data_co2$CO2_dry)) / 10
          y.m_co2 <- max(data_co2$CO2_dry) - y.q_co2
          x.m_co2 <- min(data_co2$secs) + 10
          
          correct_vec_co2_m[count_co2] <<- m_co2
          correct_vec_co2_b[count_co2] <<- b_co2
          
          caption_co2 <-
            paste(paste0("R2 = ", round(r2_co2, digits = 2)),
                  paste0("y=", round(m_co2, digits = 2), " x + " , round(b_co2, digits = 2)),
                  sep = "\n")
          
          
          p_co2 <-
            ggplot(data_co2, aes(secs, CO2_dry, col = I(points))) + geom_point() + geom_smooth(method = "lm", formula = "y~x") +
            scale_color_manual(name = "", values = c("black", "blue")) + labs(x = "seconds", y =
                                                                                "CO2 (ppm)") + ggtitle(paste0(count_co2, ":", title_co2)) +
            annotate(
              "text",
              x = x.m_co2,
              y = y.m_co2,
              hjust = 0,
              label = caption_co2,
              parse = T,
              size = 5
            )
          ggplotly(p_co2, source = paste0("select", count_co2))
          
        })
      }
    })
    
    
    observeEvent(input$accept_co2, {
      msg_co2$message <- "Default accepted. Please select next plot."
    })
    
    
    observeEvent(input$reject_co2, {
      msg_co2$message <- "Default rejected. Please select data to fit."
      
      output$subplot_co2 <- renderPlotly({
        d <- event_data("plotly_selected", source = paste0("select", count_co2))
        if (is.null(d))
          return()
        
        data_co2 <- flux_list$data_filt[[needs_correct_co2[count_co2]]]
        data_filt <-
          data_co2 %>% filter(row_number() %in% (d$pointNumber + 1))
        
        q_co2 <- ggplot(data_filt, aes(secs, CO2_dry)) +
          geom_smooth(method = "lm", formula = "y~x") +
          geom_point(color = "blue")
        
        
        
        mod2_co2 <- lm(CO2_dry ~ secs, data = data_filt)
        b2_co2 <-  summary(mod2_co2)$coefficients[[1]]
        m2_co2 <- summary(mod2_co2)$coefficients[[2]]
        r2.2_co2 <- summary(mod2_co2)$adj.r.squared
        
        correct_vec_co2_m[count_co2] <<- m2_co2
        correct_vec_co2_b[count_co2] <<- b2_co2
        
        y.q_co2 <- (max(data_filt$CO2_dry) - min(data_filt$CO2_dry)) / 10
        y.m_co2 <- max(data_filt$CO2_dry) - y.q_co2
        x.m_co2 <- min(data_filt$secs) + 3
        
        caption_co2 <-
          paste(paste0("R2 = ", round(r2.2_co2, digits = 2)),
                paste0("y=", round(m2_co2, digits = 2), " x + " , round(b2_co2, digits = 2)),
                sep = "\n")
        
        q_co2 + annotate(
          "text",
          x = x.m_co2,
          y = y.m_co2,
          hjust = 0,
          label = caption_co2,
          parse = T,
          size = 5
        ) + labs(x = "seconds", y = "CO2 (ppm)")
      })
    })
    
    output$instr_co2 <- renderText({
      msg_co2$message
    })
    
  }
  
  shinyApp(ui, server)
}




####################

add_temps <- function(flux_list, temp_data){
  for(i in 1:length(flux_list$start)){
    temp <- temp_data %>% filter(date_time > flux_list$start[i], date_time < flux_list$end[i]) %>% 
      mutate(secs=as.numeric(seconds(date_time)), secs = secs-min(secs))
    flux_list$temperature[i] <- mean(temp$temp_k)
  }
  return(flux_list)
}

################
add_temps_sync <- function(flux_list, temp_data){
  temp_filt <- list()
  for(i in 1:length(flux_list$start)){
    temp_filt[[i]] <- temp_data %>% filter(date_time > flux_list$start[i], date_time < flux_list$end[i]) %>% 
      mutate(secs=as.numeric(seconds(date_time)), secs = secs-min(secs))
    flux_list$data_filt[i][[1]] <- flux_list$data_filt[i][[1]] %>% mutate(secs=round(secs, digits=0)) %>% left_join(temp_filt[[i]], by="secs")
  }
  flux_list$temp_filt <- temp_filt
  return(flux_list)
}
####################

calc_flux <- function(flux_list, width, length, height, add_vol, R=8.205746 * 10^-5, c = 86.4){
  
  flux_ch4_mol <- NULL
  flux_ch4_g <- NULL
  flux_co2_mol <-NULL
  flux_co2_g <- NULL
  
  for(i in 1:length(flux_list$site_loc)){
    s_ch4 <- flux_list$s_ch4[i]
    s_co2 <- flux_list$s_co2[i]
    v <- (width * length * height) + add_vol
    a <- width * length
    t <- flux_list$temperature[i]
    R <- R
    c <- c
    
    flux_ch4_mol <- c(flux_ch4_mol,(s_ch4*(v/(R*t*a))*c))
    flux_co2_mol <- c(flux_co2_mol,(s_co2*(v/(R*t*a))*c))
  }
  
  flux_ch4_g <- map_dbl(flux_ch4_mol, ~.x * 10^-3 * 16.04)
  flux_co2_g <- map_dbl(flux_co2_mol, ~.x * 10^-3 * 44.01)
  
  
  ret <- data.frame("site" = flux_list$site, "location" = flux_list$location, "start_time"=flux_list$start, "flux_ch4_mmol"=flux_ch4_mol, "flux_ch4_g"=flux_ch4_g, "flux_co2_mmol"=flux_co2_mol, "flux_co2_g"=flux_co2_g)
  return(ret)
}

