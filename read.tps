#function from <https://stackoverflow.com/questions/9729491/reading-a-tps-morphometrics-file-into-r>
read.tps = function(data) {
      a = readLines(data)
      LM = grep("LM", a)
      ID.ind = grep("ID", a)  
      images = basename(gsub("(IMAGE=)(.*)", "\\2", a[ID.ind - 1]))

      skip = LM
      nrows = as.numeric(gsub("(LM=)([0-9])", "\\2", grep("LM", a, value=T)))
      l = length(LM)

      landmarks = vector("list", l)

      for (i in 1:l) {
        landmarks[i] = list(data.frame(
            read.table(file=data, header=F, skip=LM[i],
                       nrows=nrows[i], col.names=c("X", "Y")),
            IMAGE = images[i],
            ID = read.table(file=data, header=F, skip=ID.ind[i]-1, 
                            nrows=1, sep="=", col.names="ID")[2,],
            Scale = read.table(file=data, header=F, skip=ID.ind[i],
                                nrows=1, sep="=")[,2]))
      }
      do.call(rbind, landmarks)
    }
