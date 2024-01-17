library(data.table)

list.files('s:')
list.files('s:/stressed sturgeon_2022-2024/Nanticoke Vemco data 11-9-23/')
nan <- list.files('s:/stressed sturgeon_2022-2024/Nanticoke Vemco data 11-9-23/',
                  pattern = '\\.csv$', full.names = T) |> 
  lapply(fread, fill = TRUE) |> 
  rbindlist()

nan[, .N, by = Transmitter]

caps <- fread('s:/Section 6_2019-2022/marshyhopeas-2019-2022/data/raw/sturgeon capture data.csv')

caps22_23 <- readxl::read_excel('s:/stressed sturgeon_2022-2024/22 and 23 ATS bio data.xlsx') |> 
  setDT()

caps[nan, , on = c(TransmitterNumber = 'Transmitter'),
     nomatch = 0] |> 
  _[, .(range(`Date and Time (UTC)`)), by = TransmitterNumber]

# No 2022 or 2023-tagged Marshyhope fish
caps22_23[nan, , on = c(`VEMCO ID` = 'Transmitter'),
          nomatch = 0]

sturg <- nan[grepl('9001', Transmitter)]
library(ggplot2)

ggplot(data = sturg) +
  geom_point(aes(x = `Date and Time (UTC)`, y=2)) +
  facet_wrap(~Transmitter)

# library(rvdat)
# list.files('s:/stressed sturgeon_2022-2024/Nanticoke Vemco data 11-9-23/',
#            pattern = '\\.vrl$', full.names = T) |> 
#   lapply(vdat_to_folder, outdir = 'data/vps events')

bwt <- list.files('data/vps events', pattern = 'TEMP', recursive = T, full.names = T) |> 
  lapply(fread) |> 
  rbindlist()

temp_plot <- ggplot() +
  geom_vline(data = sturg, aes(xintercept = `Date and Time (UTC)`,
                               color = Transmitter),
             alpha = 0.2) +
  scale_color_viridis_d() +
  geom_line(data = bwt[`Device Time (UTC)` %between% c('2023-08-09', '2023-11-09')],
            aes(x = `Device Time (UTC)`, y = `Ambient (deg C)`,
                group = factor(`Serial Number`)),
                alpha = 0.2) +
  labs(x = NULL, y = 'Water Temp. (C)') +
    guides(color = guide_legend(override.aes = list(linewidth = 3, alpha = 1)))+
  theme_minimal() +
  theme(legend.position = c(0.1, 0.2),
        axis.text.x = element_blank()) 

temp_plot_zoom <- ggplot() +
  geom_vline(data = sturg, aes(xintercept = `Date and Time (UTC)`,
                               color = Transmitter),
             alpha = 0.2) +
  scale_color_viridis_d() +
  geom_line(data = bwt[`Device Time (UTC)` %between% c('2023-09-11', '2023-09-26')],
            aes(x = `Device Time (UTC)`, y = `Ambient (deg C)`,
                group = factor(`Serial Number`)),
            alpha = 0.2) +
  labs(x = NULL, y = 'Water Temp. (C)') +
  guides(color = guide_legend(override.aes = list(linewidth = 3, alpha = 1)))+
  theme_minimal() +
  theme(axis.text.x = element_blank())

diag <- list.files('data/vps events', pattern = 'DIAG', recursive = T, full.names = T) |> 
  lapply(fread) |> 
  rbindlist()


ggplot() +
  geom_vline(data = sturg, aes(xintercept = `Date and Time (UTC)`,
                               color = Transmitter)) +
  scale_color_viridis_d() +
  geom_line(data = diag[`Device Time (UTC)` %between% c('2023-08-09', '2023-11-09')],
            aes(x = `Device Time (UTC)`, y = `Noise Mean (mV)`,
                group = factor(`Serial Number`)),
            alpha = 0.2) +
  labs(x = NULL, y = 'Water Temp. (C)') +
  theme_minimal() +
  theme(legend.position = c(0.1, 0.2))


#### flow @ Bridgeville (USGS 01487000)
flow <- fread('https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=01487000&startDT=2023-08-08T00:00&endDT=2023-11-09T23:59&siteStatus=all',
              header = F)
setnames(flow, flow[1,] |> as.character())
flow <- flow[-c(1,2)]
flow <- type.convert(flow, as.is = T)
setnames(flow, c('69488_00060', '69489_00065'), c('flow', 'gauge'))
flow[, datetime := as.POSIXct(datetime, tz = 'America/New_York')]
flow[, cmps := flow / 35.315]
setattr(flow$datetime, 'tzone', 'UTC')

flow_plot <- ggplot(data = flow) +
  geom_vline(data = sturg, aes(xintercept = `Date and Time (UTC)`,
                               color = Transmitter),
             alpha = 0.2, show.legend = F) +
  scale_color_viridis_d() +
  geom_line(aes(x = as.POSIXct(datetime), y = cmps)) +
  labs(x = NULL, y = 'Flow (Cubic m per s)') +
  theme_minimal()



flow_plot_zoom <- ggplot(data = flow[datetime %between% c('2023-09-11', '2023-09-26')]) +
  geom_vline(data = sturg, aes(xintercept = `Date and Time (UTC)`,
                               color = Transmitter),
             alpha = 0.2, show.legend = F) +
  scale_color_viridis_d() +
  geom_line(aes(x = as.POSIXct(datetime), y = cmps)) +
  labs(x = NULL, y = 'Flow (Cubic m per s)') +
  theme_minimal()


library(patchwork)
temp_plot / flow_plot

temp_plot_zoom / flow_plot_zoom +
  plot_layout(guides = 'collect')
