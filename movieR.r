#############################
# movieR: movies about data #
#############################

# Author: Daniel Arribas-Bel <daniel.arribas.bel@gmail.com>
# Copyright 2010 by Daniel Arribas-Bel 
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#    See: <http://creativecommons.org/licenses/GPL/2.0/> or <http://www.gnu.org/licenses/>

library(PBSmodelling)

movieR <- function(csv_path, output_folder, header=TRUE, na.rm=TRUE,
                cycles=10, lapse=0.25, row.names=FALSE){
                print('Initializing...')
                z <- parser(csv_path, header=header, na.rm=na.rm, row.names=row.names)
                print('csv loaded')
                temp <- paste(output_folder, '_TEMP/', sep='')
                system(paste('mkdir', temp))
                sm <- smoother(z, temp, cycles=cycles)
                print('Smoothing files printed, setting up movie...')
                cmd <- paste("dir2slideshow -notitle -n movie -o", output_folder, "-t", lapse, temp)
                system(cmd)
                print('Starting movie...')
                movie <- paste(output_folder, 'movie', '.txt', sep='')
                cmd <- paste('dvd-slideshow -p -w -o ', output_folder,
                            ' ', movie, sep='')
                system(cmd)
                print('Cleaning up...')
                cmd <- paste('rm -R', temp)
                system(cmd)
                print('Movie ready!!!')

                }

parser <- function(csv_path, header=TRUE, na.rm=TRUE, row.names=FALSE){
    csv <- read.csv(csv_path, header=header)
    names=names(csv)
    x=FALSE
    y=FALSE
    attach(csv)
    for(col in names(csv)){
        z <- density(get(col), na.rm=na.rm)
        if(x[1]==FALSE){
            x <- z$x
            }else{
            x <- cbind(x, z$x)
            }
        if(y[1]==FALSE){
            y <- z$y
            }else{
            y <- cbind(y, z$y)
            }
        }
    detach(csv)
    csv <- FALSE
    z <- list(names=names, x=x, y=y)                # x,y are matrices

    z
    }

smoother <- function(z, output_folder, cycles=4){
    xTop <- max(z$x)
    xMin <- min(z$x)
    yTop <- max(z$y)
    yMin <- min(z$y)
    n <- length(z$x[,1])
    jump <- 0

    ceiling <- list(x=z$x[, 1], y=z$y[, 1])
    print(paste('     working on col', z$names[1]))
    file <- paste(output_folder, pad0(jump, 3)[1], pad0(0, 3)[1], '.png', sep='')
    png(file, width=960, height=960, bg="white")
    plot(ceiling$x, ceiling$y, xlim=c(xMin, xTop), cex.main=3,
        type='l', ylim=c(yMin, yTop), xlab='', ylab='', main=z$names[1], sub='')
    dev.off()

    for(col in seq(2, length(z$names))){
        print(paste('     working on col', z$names[col]))
        floor <- ceiling
        ceiling <- list(x=z$x[, col], y=z$y[, col])
        dif <- list(x=(ceiling$x - floor$x) / cycles, y=(ceiling$y - floor$y) / cycles)
        for(step in seq(cycles)){
            x <- floor$x + (dif$x * step)
            y <- floor$y + (dif$y * step)
            file <- paste(output_folder, pad0(jump, 3)[1], pad0(step, 3)[1], '.png', sep='')
            if(step==cycles){
                title=z$names[col]
                }else{
                title=''
                }
            png(file, width=960, height=960, bg="white")
            plot(x, y, xlim=c(xMin, xTop), cex.main=3,
                type='l', ylim=c(yMin, yTop), xlab='', ylab='', main=title, sub='')
            dev.off()
            if(step==cycles){
                file <- paste(file, 'a', sep='')
                png(file, width=960, height=960, bg="white")
                plot(x, y, xlim=c(xMin, xTop), cex.main=3,
                    type='l', ylim=c(yMin, yTop), xlab='', ylab='', main=title, sub='')
                dev.off()
                file <- paste(file, 'b', sep='')
                png(file, width=960, height=960, bg="white")
                plot(x, y, xlim=c(xMin, xTop), cex.main=3,
                    type='l', ylim=c(yMin, yTop), xlab='', ylab='', main=title, sub='')
                dev.off()
                file <- paste(file, 'c', sep='')
                png(file, width=960, height=960, bg="white")
                plot(x, y, xlim=c(xMin, xTop), cex.main=3,
                    type='l', ylim=c(yMin, yTop), xlab='', ylab='', main=title, sub='')
                dev.off()                }
            }
        jump <- jump + 1
        }

    'Done'
    }

