AFF = function(){
  if (TRUE) {
    install.packages("dplyr")
    install.packages("ggplot2")
    install.packages("tidytext")
    install.packages("pacman")
    install.packages("ggthemes")
    install.packages("readr")
    install.packages("showtext")
    install.packages("patchwork")
    install.packages("cluster")
    install.packages("kmed")
    install.packages("conclust")
    install.packages("clv")
    install.packages("SSLR")
    install.packages("dbscan")
    install.packages("plot3D")
    install.packages("wesanderson")
    install.packages("fs")#for make dir
    install.packages("gensvm")#for svm
    install.packages("Rdimtools")#for deminsion reduction
    install.packages("FactoMineR")#for pca
    install.packages("neuronorm")#for process sMRI
    install.packages("RNifti")#for read nifti
    install.packages("smotefamily")#for oversample
    install.packages("FNN")#for oversample
    install.packages("igraph")#for oversample
    install.packages("vioplot")#for vioplot
    install.packages("apa")#for Cohen's d
    install.packages("kknn")
    install.packages("FastKNN")
    install.packages("ksNN")
    install.packages("mildsvm")
    install.packages("ramsvm")
    install.packages("metR")#for counter plot
    install.packages("arulesCBA")
    install.packages("dbstats")
    library(dbstats)
    library(arulesCBA)
    library(metR)
    library(smotefamily)#for oversample
    library(ramsvm)
    library(mildsvm)
    library(ksNN)
    library(FastKNN)
    library(kknn)
    library(Rtsne)#for t-SNE
    library(apa)
    library(vioplot)
    library(FNN)
    library(igraph)
    library(smotefamily)
    library(RNifti)
    library(FactoMineR)
    library(Rdimtools)
    library(gensvm)
    library(wesanderson)
    library(plot3D)
    library(dplyr)
    library(cluster)
    library(ggplot2)
    library(tidytext)
    library(patchwork)
    library(kmed)
    library(conclust)
    library(clv)
    library(SSLR)
    library(pacman)
    library(ggthemes)
    library(readr)
    library(showtext)
    library(dbscan)
    library(fs)
    library(e1071)#svr
  }
  iterate = 100
  datasetpath = paste("./dataset/",sep="")
  datalist = data_initialize(datasetpath)
  resultspath = paste("./BA_results_20221001/",sep="")
  palette <- RColorBrewer::brewer.pal(9, "Pastel1")
  # small test data 
  test_acc_list = method_name_list = dataset_name_list = c()
  if (T) {
    for(i in c(1:10)){ # each data set
      dataset = datalist[[i]]
      datalabel = dataset$datalabel
      ranges = unique(datalabel)
      ncluster = length(ranges)
      data = dataset$data
      idnum = dataset$idnum
      idbin = dataset$idbin
      idcat = dataset$idcat
      # folds
      folds = list()
      for (j in c(1:10)) {
        fold = c()
        for (k in ranges) {
          range_idx = which(datalabel == k)
          get_length = round(length(range_idx)*0.1)
          if (get_length < 1) {get_length = 1} 
          unused_idx = setdiff(range_idx, unlist(folds))
          if (length(unused_idx) < get_length) {
            if (length(range_idx) == 1) {
              fold = c(fold, rep(range_idx, get_length))
            } else {
              set.seed(j)
              fold = c(fold, unused_idx, sample(range_idx, get_length-length(unused_idx)))
            }
          } else if (length(unused_idx) == 1) {
            fold = c(fold, rep(unused_idx, get_length))
          } else {
            set.seed(j)
            fold = c(fold, sample(unused_idx, get_length))
          }
        }
        folds[[j]] = fold
      }
      data = data[unlist(folds),]
      datalabel = datalabel[unlist(folds)]
      folds = list()
      for (j in c(1:10)) {
        fold = c()
        for (k in ranges) {
          range_idx = which(datalabel == k)
          get_length = round(length(range_idx)*0.1)
          if (get_length < 1) {get_length = 1} 
          unused_idx = setdiff(range_idx, unlist(folds))
          if (length(unused_idx) < get_length) {
            set.seed(j)
            fold = c(fold, unused_idx, sample(range_idx, get_length-length(unused_idx)))
          } else if (length(unused_idx) == 1) {
            fold = c(fold, rep(unused_idx, get_length))
          } else {
            set.seed(j)
            fold = c(fold, sample(unused_idx, get_length))
          }
        }
        folds[[j]] = fold
      }
      # divide
      tfnn_acc = brfsvm_acc = sknn_acc = lsvm_acc = tfglm_acc = rglm_acc = otfnn_acc = 0
      for (j in c(1:10)) {
        test_idx = folds[[j]]
        valid_idx = train_idx = c()
        for (k in setdiff(c(1:10),j)[1:3]) {
          valid_idx = c(valid_idx, folds[[k]])
        }
        for (k in setdiff(c(1:10),j)[4:9]) {
          train_idx = c(train_idx, folds[[k]])
        }
        #algorithms
        notest_label = datalabel
        notest_label[test_idx] = NA
        result = algorithms(data, idnum, idbin, idcat, train_idx, valid_idx, test_idx, notest_label, dataname = names(datalist[i]), ncluster, fold_idx=j, resultspath)
        tfnn_acc = tfnn_acc + sum(datalabel[test_idx] == result$tfnn[test_idx])/length(datalabel[test_idx])*100
        otfnn_acc = otfnn_acc + sum(datalabel[test_idx] == result$otfnn[test_idx])/length(datalabel[test_idx])*100
        sknn_acc = sknn_acc + sum(datalabel[test_idx] == result$sknn[test_idx])/length(datalabel[test_idx])*100
        lsvm_acc = lsvm_acc + sum(datalabel[test_idx] == result$lsvm[test_idx])/length(datalabel[test_idx])*100
        brfsvm_acc = brfsvm_acc + sum(datalabel[test_idx] == result$brfsvm[test_idx])/length(datalabel[test_idx])*100
        rglm_acc = rglm_acc + sum(datalabel[test_idx] == result$rglm[test_idx])/length(datalabel[test_idx])*100
        tfglm_acc = tfglm_acc + sum(datalabel[test_idx] == result$tfglm[test_idx])/length(datalabel[test_idx])*100
        # owknn_acc = owknn_acc + sum(datalabel[test_idx] == result$owknn[test_idx])/length(datalabel[test_idx])*100
        cat("dataset",i,"fold",j,".\n")
      }
      test_acc_list = c(test_acc_list, tfnn_acc/10, sknn_acc/10, lsvm_acc/10, brfsvm_acc/10, rglm_acc/10, tfglm_acc/10, otfnn_acc/10)
      method_name_list = c(method_name_list, "TFNN", "KSNN", "LSVM", "RBFSVM", "DBMLR", "TFMLR", "OTFNN")
      dataset_name_list = c(dataset_name_list , rep(names(datalist[i]), 7) )
    }
    #plot
    if (TRUE) {
      out = data.frame(
        test_acc_list = test_acc_list,
        method_name_list = method_name_list,
        dataset_name_list = dataset_name_list
      )
      # palette <- RColorBrewer::brewer.pal(9, "Pastel1")
      boxp1 = ggplot(out, aes(x = method_name_list, y = test_acc_list, fill = dataset_name_list))
      boxp1 = boxp1 + geom_col(position = 'stack')
      # boxp1 = boxp1 + scale_fill_manual(values=palette)
      boxp1 = boxp1 + labs(x="Algorithm",y = "Accuracy",title = "Stacked Bar") 
      boxp1 = boxp1 + theme_bw() 
      boxp1 = boxp1 + theme(legend.position = "bottom",
                            legend.direction = "horizontal",
                            legend.title = element_blank(),
                            axis.text.x = element_text(size = 13, vjust = 0.5, hjust = 0.5),#, angle = 15
                            axis.text.y = element_text(size = 13, vjust = 0.5, hjust = 0.5),
                            panel.grid = element_blank(),
                            plot.title = element_text(hjust = 0.5)
      )
      print(boxp1)
      png(file = paste(resultspath, "Stacked Bar.png", sep=""))
      plot(boxp1)
      dev.off()
    }
  }
  # save(out, file = paste(resultspath, "test_result", sep=""))
  # return(out)
  # COBRE and SALD
  test_acc_list = method_name_list = dataset_name_list = c()
  if (T) {
    for(i in c(13:14)){ #each dataset
      dataset = datalist[[i]]
      datalabel = dataset$datalabel
      ranges = unique(datalabel)
      ncluster = length(ranges)
      data = dataset$data
      idnum = dataset$idnum
      idbin = dataset$idbin
      idcat = dataset$idcat
      # folds
      folds = list()
      for (j in c(1:10)) {
        fold = c()
        for (k in ranges) {#
          range_idx = which(datalabel == k)
          get_length = round(length(range_idx)*0.1)
          if (get_length < 1) {get_length = 1} 
          unused_idx = setdiff(range_idx, unlist(folds))
          if (length(unused_idx) < get_length) {
            if (length(range_idx) == 1) {
              fold = c(fold, rep(range_idx, get_length))
            } else {
              set.seed(j)
              fold = c(fold, unused_idx, sample(range_idx, get_length-length(unused_idx)))
            }
          } else if (length(unused_idx) == 1) {
            fold = c(fold, rep(unused_idx, get_length))
          } else {
            set.seed(j)
            fold = c(fold, sample(unused_idx, get_length))
          }
        }
        folds[[j]] = fold
      }
      data = data[unlist(folds),]
      datalabel = datalabel[unlist(folds)]
      folds = list()
      for (j in c(1:10)) {
        fold = c()
        for (k in ranges) {#
          range_idx = which(datalabel == k)
          get_length = round(length(range_idx)*0.1)
          if (get_length < 1) {get_length = 1} 
          unused_idx = setdiff(range_idx, unlist(folds))
          if (length(unused_idx) < get_length) {
            set.seed(j)
            fold = c(fold, unused_idx, sample(range_idx, get_length-length(unused_idx)))
          } else if (length(unused_idx) == 1) {
            fold = c(fold, rep(unused_idx, get_length))
          } else {
            set.seed(j)
            fold = c(fold, sample(unused_idx, get_length))
          }
        }
        folds[[j]] = fold
      }
      # divide
      for (j in c(1:10)) {
        test_idx = folds[[j]]
        valid_idx = train_idx = c()
        for (k in setdiff(c(1:10),j)[1:3]) {
          valid_idx = c(valid_idx, folds[[k]])
        }
        for (k in setdiff(c(1:10),j)[4:9]) {
          train_idx = c(train_idx, folds[[k]])
        }
        #algorithms
        notest_label = datalabel
        notest_label[test_idx] = NA
        result = algorithms2(data, idnum, idbin, idcat, train_idx, valid_idx, test_idx, notest_label, dataname = names(datalist[i]), ncluster, fold_idx=j, resultspath)
        tfnn_mae = sum(abs(datalabel[test_idx]-result$tfnn[test_idx]))/length(datalabel[test_idx])
        # otfnn_mae = sum(abs(datalabel[test_idx]-result$otfnn[test_idx]))/length(datalabel[test_idx])
        brfsvm_mae = sum(abs(datalabel[test_idx]-result$brfsvm[test_idx]))/length(datalabel[test_idx])
        sknn_mae = sum(abs(datalabel[test_idx]-result$sknn[test_idx]))/length(datalabel[test_idx])
        lsvm_mae = sum(abs(datalabel[test_idx]-result$lsvm[test_idx]))/length(datalabel[test_idx])
        # rglm_mae = sum(abs(datalabel[test_idx]-result$rglm[test_idx]))/length(datalabel[test_idx])
        tfglm_mae = sum(abs(datalabel[test_idx]-result$tfglm[test_idx]))/length(datalabel[test_idx])
        # owknn_mae = sum(abs(datalabel[test_idx]-result$owknn[test_idx]))/length(datalabel[test_idx])
        test_acc_list = c(test_acc_list, tfnn_mae, brfsvm_mae, sknn_mae, lsvm_mae, tfglm_mae)
        method_name_list = c(method_name_list, "TFNN", "RBFSVM", "KSNN", "LSVM", "TFMLR")
        dataset_name_list = c(dataset_name_list , rep(names(datalist[i]), 5) )
        cat("dataset",i,"fold",j,".\n")
      }
    }
    # plot
    if(TRUE){
      palette = RColorBrewer::brewer.pal(9, "Pastel1")
      algori_result = data.frame(Algorithm = method_name_list,
                                 dataset_name_list = dataset_name_list,
                                 test_acc_list = test_acc_list)
      boxp1 = ggplot(algori_result, aes(x = dataset_name_list, y = test_acc_list, fill = method_name_list))
      boxp1 = boxp1 + geom_boxplot(notch=FALSE ,position=position_dodge(1))
      boxp1 = boxp1 + geom_dotplot(binaxis = "y", stackdir = "center", position = position_dodge(1))# points
      boxp1 = boxp1 + labs(x="Dataset",
                           y = "MAE",
                           title = names(datalist[i]))
      boxp1 = boxp1 + scale_fill_manual(values = palette)
      boxp1 = boxp1 + theme_bw() 
      boxp1 = boxp1 + theme(legend.position = "bottom",
                            legend.direction = "horizontal",
                            legend.title = element_blank(),
                            axis.text.x = element_text(size = 13, vjust = 0.5, hjust = 0.5),
                            axis.text.y = element_text(size = 13, vjust = 0.5, hjust = 0.5),
                            panel.grid = element_blank(),
                            plot.title = element_text(hjust = 0.5)
      )
      print(boxp1)
      png(file = paste(resultspath,names(datalist[i]), "SSMDNN.png", sep=""))
      plot(boxp1)
      dev.off() 
    }
    # save(algori_result, file = paste(resultspath, "cobre_result", sep=""))
    # return(algori_result)
    
    outs = list(out = out, algori_result = algori_result)
  }
  save(outs, file = paste(resultspath, "outs", sep=""))
  return(outs)
}

algorithms = function(data, idnum, idbin, idcat, train_idx, valid_idx, test_idx, notest_label, dataname, ncluster, fold_idx, resultspath){
  for (i in c(1:5)) {
    if (i==1) {
      my_data = data
      my_train_idx = train_idx
      my_valid_idx = valid_idx
      my_notest_label = notest_label
      #validation
      svm_acc_list = c_list = g_list = c()
      best_acc = 0
      for (k in c(1:10)) {
        distdata = ssmdf(my_data, idnum, idbin, idcat, my_train_idx, my_notest_label, ncluster, con=k)
        for (j in c(1:length(my_train_idx))) {
          my_label = distknn(distdata, my_notest_label, my_train_idx, my_valid_idx, j)
          my_acc = sum(my_notest_label[my_valid_idx]==my_label[my_valid_idx])/length(my_valid_idx)*100
          svm_acc_list = c(svm_acc_list, my_acc)
          c_list = c(c_list, k)
          g_list = c(g_list, j)
          if (my_acc > best_acc) {
            best_acc = my_acc
            best_k = j
            best_con = k
          }
        }
      }
      if (F) {
        counter_data = data.frame(
          svm_acc_list = svm_acc_list,
          c_list = c_list,
          g_list = g_list
        )
        palette <- RColorBrewer::brewer.pal(9, "Pastel1")
        boxp = ggplot(data = counter_data, aes(x=c_list, y=g_list, z=svm_acc_list))
        boxp = boxp + ggplot2::geom_contour_filled(aes(z = svm_acc_list),binwidth=1,position = 'identity')
        boxp = boxp + geom_contour(aes(z = svm_acc_list) ,binwidth=100 ,color="black")
        boxp = boxp + scale_x_continuous(expand = c(0,0))
        boxp = boxp + scale_y_continuous(expand = c(0,0))
        boxp = boxp + coord_flip()
        boxp = boxp + metR::geom_text_contour(aes(z = svm_acc_list),stat="text_contour",binwidth=1,check_overlap = T,
                                              skip=0,stroke = 0,color="red")
        boxp = boxp + labs(x="Cost",y = "Gamma", title = dataname) #title = "Stacked Bar"
        boxp = boxp + theme_bw() 
        boxp = boxp + theme(legend.position = "bottom",
                            legend.direction = "horizontal",
                            legend.title = element_blank(),
                            axis.text.x = element_text(size = 13, vjust = 0.5, hjust = 0.5),#, angle = 15
                            axis.text.y = element_text(size = 13, vjust = 0.5, hjust = 0.5),
                            panel.grid = element_blank(),
                            plot.title = element_text(hjust = 0.5)
        )
        print(boxp)
        png(file = paste(resultspath, dataname, fold_idx, " Contour map.png", sep=""))
        plot(boxp)
        dev.off()
      }
      #test
      distdata = ssmdf(my_data, idnum, idbin, idcat, c(my_train_idx, my_valid_idx), my_notest_label, ncluster, con=best_con)
      tfnn = distknn(distdata, my_notest_label, c(my_train_idx, my_valid_idx), test_idx, best_k)
      
      train_distdata = distdata[c(my_train_idx, my_valid_idx),c(my_train_idx, my_valid_idx)]
      class(train_distdata) = "D2"
      dbglm1 = dbglm(D2 = train_distdata, y = my_notest_label[c(my_train_idx, my_valid_idx)],family = poisson(link = "log"), method="rel.gvar")
      sapmle_num = length(c(my_train_idx, my_valid_idx))-length(test_idx)
      set.seed(2)
      sapmle_idx = sample(test_idx, sapmle_num, replace = T)
      test_distdata = distdata[c(test_idx, sapmle_idx), c(test_idx, sapmle_idx)]
      class(test_distdata) = "D2"
      dbglm2 = predict(dbglm1,test_distdata,type.pred="response",type.var="D2")
      tfglm = my_notest_label
      tfglm[test_idx] = round(dbglm2[c(1:length(test_idx))])
      
      rawdata = my_data[,idnum]
      rawdata = (rawdata-min(rawdata))/(max(rawdata)-min(rawdata))
      rawdistdata = as.matrix(dist(rawdata, method = "euclidean"))
      train_distdata = rawdistdata[c(my_train_idx, my_valid_idx),c(my_train_idx, my_valid_idx)]
      class(train_distdata) = "D2"
      glm1 = dbglm(D2 = train_distdata, y = my_notest_label[c(my_train_idx, my_valid_idx)],family = poisson(link = "log"), method="rel.gvar")
      sapmle_num = length(c(my_train_idx, my_valid_idx))-length(test_idx)
      set.seed(1)
      sapmle_idx = sample(test_idx, sapmle_num, replace = T)
      test_distdata = rawdistdata[c(test_idx, sapmle_idx), c(test_idx, sapmle_idx)]
      class(test_distdata) = "D2"
      glm2 = predict(glm1,test_distdata,type.pred="response",type.var="D2")
      rglm = my_notest_label
      rglm[test_idx] =  round(glm2[c(1:length(test_idx))])
      cat("TF done \n")
    }
    else if (i==5) {
      # over sample
      if (length(unique(table(notest_label))) != 1) {
        origin_length = length(notest_label)
        oversamples = oversample(data, notest_label)
        my_data = oversamples$data
        my_notest_label = oversamples$datalabel
        my_idx = c(train_idx, valid_idx, c((origin_length+1) : length(my_notest_label)))
        # divide train valid
        my_train_idx = my_valid_idx = c()
        ranges = unique(my_notest_label)
        for (j in ranges) {
          range_idx = which(my_notest_label == j)
          set.seed(1)
          my_valid_idx = c(my_valid_idx, sample(my_idx, round(length(range_idx)/3)))
          my_train_idx = c(my_train_idx, setdiff(range_idx, my_valid_idx))
        }
      }
      else {
        my_data = data
        my_train_idx = train_idx
        my_valid_idx = valid_idx
        my_notest_label = notest_label
      }
      #validation
      svm_acc_list = c_list = g_list = c()
      best_acc = 0
      for (k in c(1:10)) {
        distdata = ssmdf(my_data, idnum, idbin, idcat, my_train_idx, my_notest_label, ncluster, con=k)
        for (j in c(1:length(my_train_idx))) {
          my_label = distknn(distdata, my_notest_label, my_train_idx, my_valid_idx, j)
          my_acc = sum(my_notest_label[my_valid_idx]==my_label[my_valid_idx])/length(my_valid_idx)*100
          svm_acc_list = c(svm_acc_list, my_acc)
          c_list = c(c_list, k)
          g_list = c(g_list, j)
          if (my_acc > best_acc) {
            best_acc = my_acc
            best_k = j
            best_con = k
          }
        }
      }
      if (F) {
        counter_data = data.frame(
          svm_acc_list = svm_acc_list,
          c_list = c_list,
          g_list = g_list
        )
        palette <- RColorBrewer::brewer.pal(9, "Pastel1")
        boxp = ggplot(data = counter_data, aes(x=c_list, y=g_list, z=svm_acc_list))
        boxp = boxp + ggplot2::geom_contour_filled(aes(z = svm_acc_list),binwidth=1,position = 'identity')
        boxp = boxp + geom_contour(aes(z = svm_acc_list) ,binwidth=100 ,color="black")
        boxp = boxp + scale_x_continuous(expand = c(0,0))
        boxp = boxp + scale_y_continuous(expand = c(0,0))
        boxp = boxp + coord_flip()
        boxp = boxp + metR::geom_text_contour(aes(z = svm_acc_list),stat="text_contour",binwidth=1,check_overlap = T,
                                              skip=0,stroke = 0,color="red")
        boxp = boxp + labs(x="Cost",y = "Gamma", title = dataname) #title = "Stacked Bar"
        boxp = boxp + theme_bw() 
        boxp = boxp + theme(legend.position = "bottom",
                            legend.direction = "horizontal",
                            legend.title = element_blank(),
                            axis.text.x = element_text(size = 13, vjust = 0.5, hjust = 0.5),#, angle = 15
                            axis.text.y = element_text(size = 13, vjust = 0.5, hjust = 0.5),
                            panel.grid = element_blank(),
                            plot.title = element_text(hjust = 0.5)
        )
        print(boxp)
        png(file = paste(resultspath, dataname, fold_idx, "oversample TF Contour map.png", sep=""))
        plot(boxp)
        dev.off()
      }
      #test
      distdata = ssmdf(my_data, idnum, idbin, idcat, c(my_train_idx, my_valid_idx), my_notest_label, ncluster, con=best_con)
      otfnn = distknn(distdata, my_notest_label, c(my_train_idx, my_valid_idx), test_idx, best_k)
      cat("OTF done \n")
    }
    else if (i==3) {
      #validation
      rawdata = data[,idnum]
      rawdata=(rawdata-min(rawdata))/(max(rawdata)-min(rawdata))
      distdata = as.matrix(dist(rawdata, method = "euclidean"))
      best_acc = 0
      pred_ksNN = rep(0, length(valid_idx))
      for (j in c(1:length(train_idx))) {
        for (k in c(1:length(valid_idx))) {
          pred_ksNN[k] = rcpp_ksNN(notest_label[train_idx], distdata[valid_idx[k],train_idx],L_C=j)$pred
          pred_ksNN[k] = round(as.numeric(pred_ksNN[k]))
        }
        ksnn_acc = sum(notest_label[valid_idx]==as.numeric(pred_ksNN))/length(valid_idx)*100
        if (ksnn_acc > best_acc) {
          best_acc = ksnn_acc
          best_k = j
        }
      }
      #test
      pred_ksNN = rep(0, length(test_idx))
      for (j in c(1:length(test_idx))) {
        pred_ksNN[j] = rcpp_ksNN(notest_label[c(train_idx, valid_idx)], distdata[test_idx[j],c(train_idx, valid_idx)],L_C=best_k)$pred
        pred_ksNN[j] = round(as.numeric(pred_ksNN[j]))
      }
      sknn = notest_label
      sknn[test_idx] = as.numeric(pred_ksNN)
      cat("KSNN done \n")
    }
    else if (i==4) {
      #validation
      rawdata = data[,idnum]
      rawdata = (rawdata-min(rawdata))/(max(rawdata)-min(rawdata))
      rawdata = as.data.frame(cbind(data, as.matrix(notest_label)))
      colnames(rawdata) = paste('v',as.integer(c(1:ncol(rawdata))) ,sep = '')
      colnames(rawdata)[length(colnames(rawdata))] = 'label_col'
      rawdata[, ncol(rawdata)] = factor(rawdata[, ncol(rawdata)])
      best_acc = 0
      for (j in c(1:8)) {
        svm_model = svm(label_col~.,rawdata[train_idx,], cost  = 10^(j-4), kernel = "linear",scale = F)
        svm_acc = sum(notest_label[valid_idx]==as.numeric(predict(svm_model,rawdata[valid_idx,])))/length(valid_idx)*100
        if (svm_acc > best_acc) {
          best_acc = svm_acc
          best_c = 10^(j-4)
        }
      }
      #test
      rawdata[, ncol(rawdata)] = as.numeric(rawdata[, ncol(rawdata)])
      rawdata[test_idx, ncol(rawdata)] = rep(100000,length(test_idx))
      rawdata[, ncol(rawdata)] = factor(rawdata[, ncol(rawdata)])
      svm_model = svm(label_col~.,rawdata[train_idx,] , cost = best_c, kernel = "linear", scale = F)
      lsvm = notest_label
      lsvm[test_idx] = as.numeric(predict(svm_model,rawdata[test_idx,]))
      cat("LSVM done \n")
    }
    else if (i==2) {
      #validation
      rawdata = data[,idnum]
      rawdata = (rawdata-min(rawdata))/(max(rawdata)-min(rawdata))
      rawdata = as.data.frame(cbind(data, as.matrix(notest_label)))
      colnames(rawdata) = paste('v',as.integer(c(1:ncol(rawdata))) ,sep = '')
      colnames(rawdata)[length(colnames(rawdata))] = 'label_col'
      rawdata[, ncol(rawdata)] = factor(rawdata[, ncol(rawdata)])
      best_acc = 0
      svm_acc_list = c_list = g_list = c()
      for (j in c(1:8)) {
        for (k in 1/c(0.1,0.2,0.4,0.8,1.6,3.2,6.4,12.8)) {
          svm_model = svm(label_col~.,rawdata[train_idx,], cost  = 10^(j-4), kernel = "radial", gamma = k^2, scale = FALSE)
          svm_acc = sum(notest_label[valid_idx]==as.numeric(predict(svm_model,rawdata[valid_idx,])))/length(valid_idx)*100
          svm_acc_list = c(svm_acc_list, svm_acc)
          c_list = c(c_list, j)
          g_list = c(g_list, k)
          if (svm_acc > best_acc) {
            best_acc = svm_acc
            best_c = 10^(j-4)
            best_g = k
          }
        }
      }
      if (F) {
        counter_data = data.frame(
          svm_acc_list = svm_acc_list,
          c_list = c_list,
          g_list = g_list
        )
        palette <- RColorBrewer::brewer.pal(9, "Pastel1")
        boxp = ggplot(data = counter_data, aes(x=c_list, y=g_list, z=svm_acc_list))
        boxp = boxp + ggplot2::geom_contour_filled(aes(z = svm_acc_list),binwidth=1,position = 'identity')
        boxp = boxp + geom_contour(aes(z = svm_acc_list) ,binwidth=100 ,color="black")
        boxp = boxp + scale_x_continuous(expand = c(0,0))
        boxp = boxp + scale_y_continuous(expand = c(0,0))
        boxp = boxp + coord_flip()
        boxp = boxp + metR::geom_text_contour(aes(z = svm_acc_list),stat="text_contour",binwidth=1,check_overlap = T,
                                              skip=0,stroke = 0,color="red")
        boxp = boxp + labs(x="Cost",y = "Gamma", title = dataname) #title = "Stacked Bar"
        boxp = boxp + theme_bw() 
        boxp = boxp + theme(legend.position = "bottom",
                            legend.direction = "horizontal",
                            legend.title = element_blank(),
                            axis.text.x = element_text(size = 13, vjust = 0.5, hjust = 0.5),#, angle = 15
                            axis.text.y = element_text(size = 13, vjust = 0.5, hjust = 0.5),
                            panel.grid = element_blank(),
                            plot.title = element_text(hjust = 0.5)
        )
        print(boxp)
        png(file = paste(resultspath, dataname, fold_idx, " Contour map.png", sep=""))
        plot(boxp)
        dev.off()
      }
      #test
      rawdata[, ncol(rawdata)] = as.numeric(rawdata[, ncol(rawdata)])
      rawdata[test_idx, ncol(rawdata)] = rep(100000,length(test_idx))
      rawdata[, ncol(rawdata)] = factor(rawdata[, ncol(rawdata)])
      svm_model = svm(label_col~.,rawdata[c(train_idx),] , cost = best_c, kernel = "radial", gamma = best_g, scale = FALSE)
      svm_label = notest_label
      svm_label[test_idx] = as.numeric(predict(svm_model,rawdata[test_idx,]))
      brfsvm = svm_label
      cat("RBF SVM done \n")
    }
  }
  return(list(tfnn = tfnn, sknn = sknn, brfsvm = brfsvm, lsvm = lsvm, tfglm = tfglm, rglm = rglm, otfnn = otfnn))
}
ssmdf = function(data, idnum, idbin, idcat, train_idx, datalabel, ncluster, con){
  #constraints
  if (TRUE) {
    mustLink = NULL
    cantLink = NULL
    allink = t(combn(train_idx,2))
    for(k in c(1:nrow(allink))){
      if(datalabel[allink[k,1]] == datalabel[allink[k,2]]){mustLink = rbind(mustLink, allink[k,])}
      if(datalabel[allink[k,1]] != datalabel[allink[k,2]]){cantLink = rbind(cantLink, allink[k,])}
    }
  }
  #data dist
  if(is.null(idbin) && is.null(idcat)){
    data=(data-min(data))/(max(data)-min(data))
    distdata = as.matrix(dist(data, method = "euclidean"))
  } else {
    distdata = distmix(data, method = "gower", idnum = idnum, idbin = idbin, idcat = idcat)
  }
  #pro
  if (F) {
    n = nrow(distdata)
    m = nrow(mustLink)
    c = nrow(cantLink)
    promatrix = matrix(0,n,n)
    for (i in c(1:m)) {promatrix[mustLink[i,1], mustLink[i,2]] = promatrix[mustLink[i,2], mustLink[i,1]] = 1}
    for (i in c(1:c)) {promatrix[cantLink[i,1], cantLink[i,2]] = promatrix[cantLink[i,2], cantLink[i,1]] = 2}
    mustpoint = unique(c(mustLink))
    mustLink = matrix(,0,2)
    cantLink = matrix(,0,2)
    while (length(mustpoint) > 0) {
      pro = used = subi = c(mustpoint[1]) 
      mustpoint = mustpoint[-1]
      cant = c()
      while (length(pro) > 0) {
        for (i in c(1:n)) {
          if (promatrix[pro[1],i]==1 && !i%in%used) {
            pro = c(pro,i); used = c(used,i); subi = c(subi,i); mustpoint=mustpoint[-which(mustpoint==i)]
          }else if (promatrix[pro[1],i]==2) {
            cant = c(cant,i)
          }
        }
        pro = pro[-1]
        if (length(pro)==0 && length(subi)>0){
          tmustLink = t(combn(subi, 2))
          mustLink = rbind(mustLink, tmustLink)
        }
        if (length(pro)==0 && length(subi)>0 && length(cant)>0){
          tcantLink = matrix(unlist(expand.grid(subi,cant)), ncol=2)
          cantLink = rbind(cantLink, tcantLink)
        }
      }
    }
  }
  #constrained
  if (TRUE) {
    n = nrow(distdata)
    m = nrow(mustLink)
    c = nrow(cantLink)
    distdata_old = distdata_new = distdata
    maxdist = distdata_new[which(distdata==0 ,arr.ind = TRUE)] = max(distdata_new)
    mindist = min(distdata_new)
    if(m>0){
      for(i in c(1:m)){
        distdata[mustLink[i,2], mustLink[i,1]] = distdata[mustLink[i,1], mustLink[i,2]] = mindist#/con
      }
    }
    if(c>0){
      for(i in c(1:c)){
        distdata[cantLink[i,2], cantLink[i,1]] = distdata[cantLink[i,1], cantLink[i,2]] = maxdist#*con
      }
    }
  }
  # prior
  if (TRUE) {
    skm_label = skm(distdata, ncluster, ncluster, 100)$cluster
    mustLink = NULL
    cantLink = NULL
    allink = t(combn(c(1:length(skm_label)),2))
    for(k in c(1:nrow(allink))){
      if(skm_label[allink[k,1]] == skm_label[allink[k,2]]){mustLink = rbind(mustLink, allink[k,])}
      if(skm_label[allink[k,1]] != skm_label[allink[k,2]]){cantLink = rbind(cantLink, allink[k,])}
    }
    m = nrow(mustLink)
    c = nrow(cantLink)
    distdata_old = distdata_new = distdata
    maxdist = distdata_new[which(distdata==0 ,arr.ind = TRUE)] = max(distdata_new)
    mindist = min(distdata_new)
    if(m>0){
      for(i in c(1:m)){
        distdata[mustLink[i,2], mustLink[i,1]] = distdata[mustLink[i,1], mustLink[i,2]] = distdata[mustLink[i,1], mustLink[i,2]]/con
      }
    }
    if(c>0){
      for(i in c(1:c)){
        distdata[cantLink[i,2], cantLink[i,1]] = distdata[cantLink[i,1], cantLink[i,2]] = distdata[cantLink[i,1], cantLink[i,2]]*con
      }
    }
  }
  return(distdata)
}
oversample = function(data, datalabel){
  sample_num = max(table(datalabel))
  target = names(which(table(datalabel) < sample_num))
  for (i in target) {
    target_idx = which(datalabel == i)
    if (length(target_idx) == 1) {#if target length = 1, sample 244 means 1:244
      add_idx = rep(target_idx, sample_num - 1)
    } else {
      set.seed(1)
      add_idx = sample(target_idx, sample_num-length(target_idx), replace = TRUE)
    }
    data = rbind(data, data[add_idx, ])
    datalabel = c(datalabel, datalabel[add_idx])
  }
  out = list(data = data, datalabel = datalabel)
  return(out)
}
distknn = function(distdata, datalabel, train_idx, test_idx, k){
  distdata[-train_idx, -train_idx] = Inf
  diag(distdata) = Inf
  predictions = rep(0, length(test_idx))
  datalabel[-train_idx] = NA
  for (i in c(1:length(test_idx))) {
    ordered_neighbors = order(distdata[test_idx[i], ])
    indices = ordered_neighbors[1:k]
    label_q = datalabel[indices]
    table_q = table(label_q)
    vote = rep(0, length(table_q))
    for (p in c(1:length(table_q))) {
      distdata_colnames = intersect(indices, which(datalabel==names(table_q)[p]))
      vote[p] = sum(1/exp(distdata[test_idx[i], distdata_colnames]))
    }
    predictions[i] = names(table_q)[which.max(vote)]
  }
  datalabel[test_idx] = as.numeric(predictions)
  return(datalabel)
}
data_initialize = function(datasetpath = NULL){
  if (is.null(datasetpath)==TRUE) {datasetpath = paste("./dataset/",sep="")}
  load(file=paste(datasetpath, "overlapping_clusters", sep="")) 
  load(file=paste(datasetpath, "noise20_100_clusters", sep=""))
  load(file=paste(datasetpath, "halfring_clusters", sep=""))
  load(file=paste(datasetpath, "ring_clusters", sep=""))
  for (i in c(1:13)) {
    if (i==1) {
      results_path = paste(datasetpath, "project_cobre/", sep="")
      pca_data = read.table(paste(results_path, "cobre_pca_data.txt", sep=""), header=F)
      pca_anat = read.table(paste(results_path, "cobre_pca_anat.txt", sep=""), header=F)
      pca_func = read.table(paste(results_path, "cobre_pca_func.txt", sep=""), header=F)
      pca_age = read.table(paste(results_path, "cobre_pca_age.txt", sep=""), header=F)
      pca_sex = read.table(paste(results_path, "cobre_pca_sex.txt", sep=""), header=F)
      pca_hand = read.table(paste(results_path, "cobre_pca_hand.txt", sep=""), header=F)
      pca_label = read.table(paste(results_path, "cobre_pca_label.txt", sep=""), header=F)
      train_idx = read.table(paste(results_path, "cobre_train_idx.txt", sep=""), header=F)
      #convert to vector
      pca_age = c(t(pca_age))
      pca_sex = c(t(pca_sex))
      pca_hand = c(t(pca_hand))
      pca_label = c(t(pca_label))
      train_idx = c(t(train_idx))
      #only control
      pca_data = pca_data[train_idx,]
      pca_anat = pca_anat[train_idx,]
      pca_func = pca_func[train_idx,]
      pca_age = pca_age[train_idx]
      pca_sex = pca_sex[train_idx]
      pca_hand = pca_hand[train_idx]
      pca_label = pca_label[train_idx]
      RCOBRE = list(
        data = cbind(pca_func, pca_anat),
        datalabel = c(t(pca_label)),
        idnum = c(1: (ncol(pca_func)+ncol(pca_anat)) ),
        idbin = NULL,
        idcat = NULL
      )
      MCOBRE = list(
        data = pca_data[, 1:(ncol(pca_data)-1)],
        datalabel = c(t(pca_label)),
        idnum = c(1: (ncol(pca_func)+ncol(pca_anat)+1) ),
        idbin = c(ncol(pca_func)+ncol(pca_anat)+2 ),
        idcat = c( (ncol(pca_func)+ncol(pca_anat)+3):((ncol(pca_data)-1)) )
      )
    }
    else if (i==12) {
      results_path = paste(datasetpath, "project_sald/", sep="")
      pca_data = read.table(paste(results_path, "sald_pre_data.txt", sep=""), header=F)
      pca_anat = read.table(paste(results_path, "sald_pre_anat.txt", sep=""), header=F)
      pca_func = read.table(paste(results_path, "sald_pre_func.txt", sep=""), header=F)
      # pca_age = read.table(paste(results_path, "sald_pre_age.txt", sep=""), header=F)
      # pca_sex = read.table(paste(results_path, "sald_pre_sex.txt", sep=""), header=F)
      pca_label = read.table(paste(results_path, "sald_pre_label.txt", sep=""), header=F)
      #dataset
      RSALD = list(
        data = cbind(pca_func, pca_anat),
        datalabel = c(t(pca_label)),
        idnum = c(1: (ncol(pca_func)+ncol(pca_anat)) ),
        idbin = NULL,
        idcat = NULL
      )
      MSALD = list(
        data = pca_data[, 1:(ncol(pca_data)-1)],
        datalabel = c(t(pca_label)),
        idnum = c(1: (ncol(pca_func)+ncol(pca_anat)+1) ),
        idbin = c(ncol(pca_func)+ncol(pca_anat)+2 ),
        idcat = NULL
      )
    }
    else if (i==13) {
      results_path1 = paste(datasetpath, "project_sald/", sep="")
      results_path2 = paste(datasetpath, "project_cobre/", sep="")
      pca_anat = rbind(read.table(paste(results_path1, "sald_pca_anat.txt", sep=""), header=F), read.table(paste(results_path2, "cobre_pca_anat.txt", sep=""), header=F)) 
      pca_func = rbind(read.table(paste(results_path1, "sald_pca_func.txt", sep=""), header=F), read.table(paste(results_path2, "cobre_pca_func.txt", sep=""), header=F)) 
      pca_age = rbind(read.table(paste(results_path1, "sald_pca_age.txt", sep=""), header=F), read.table(paste(results_path2, "cobre_pca_age.txt", sep=""), header=F)) 
      pca_sex = rbind(read.table(paste(results_path1, "sald_pca_sex.txt", sep=""), header=F), read.table(paste(results_path2, "cobre_pca_sex.txt", sep=""), header=F))  
      pca_label = rbind(read.table(paste(results_path1, "sald_pca_label.txt", sep=""), header=F), read.table(paste(results_path2, "cobre_pca_label.txt", sep=""), header=F)) 
      control_idx = c(t(read.table(paste(results_path2, "cobre_train_idx.txt", sep=""), header=F)))
      slad_length = nrow(read.table(paste(results_path1, "sald_pca_anat.txt", sep=""), header=F))
      cobre_length = nrow(read.table(paste(results_path2, "cobre_pca_anat.txt", sep=""), header=F))
      slad_label = c(t(read.table(paste(results_path1, "sald_pca_label.txt", sep=""), header=F)))
      valid_idx = c()
      for (j in names(table(slad_label))) {
        if (length(which(slad_label == j)) == 1) {
          valid_idx = c(valid_idx, which(valid_idx == j))
        } else {
          valid_idx = c(valid_idx, sample(which(slad_label == j), 1))
        }
      }
      train_idx = setdiff(c(1:slad_length),valid_idx) 
      control_idx = slad_length + control_idx
      patient_idx = slad_length + setdiff(c(1:cobre_length),control_idx)
      RDATA = list(
        data = cbind(pca_func, pca_anat),
        datalabel = c(t(pca_label)),
        idnum = c(1: (ncol(pca_func)+ncol(pca_anat)) ),
        idbin = NULL,
        idcat = NULL,
        # control_idx = control_idx
        train_idx = train_idx,
        valid_idx = valid_idx,
        control_idx = control_idx,
        patient_idx = patient_idx
      )
      MDATA = list(
        data = cbind(cbind(pca_func, pca_anat),cbind(pca_age, pca_sex)),
        datalabel = c(t(pca_label)),
        idnum = c(1: (ncol(pca_func)+ncol(pca_anat)+1) ),
        idbin = c(ncol(pca_func)+ncol(pca_anat)+2 ),
        idcat = NULL,
        # control_idx = control_idx
        train_idx = train_idx,
        valid_idx = valid_idx,
        control_idx = control_idx,
        patient_idx = patient_idx
      )
    }
    else if (i==2) {
      data = overlapping_clusters
      data[which(data=="cluster 1", arr.ind = TRUE)] = 1
      data[which(data=="cluster 2", arr.ind = TRUE)] = 2
      data[,3] = as.numeric(data[,4])
      datalabel = data[,4]
      data = data[,1:3]
      idnum = c(1,2)
      idbin = c(3)
      idcat = NULL
      OVERLAPPING = list(
        data = data,
        datalabel = datalabel,
        idnum = idnum,
        idbin = idbin,
        idcat = idcat
      )
    } 
    else if (i==3) {
      data = noise20_100_clusters
      data[which(data=="cluster 1", arr.ind = TRUE)] = 1
      data[which(data=="cluster 2", arr.ind = TRUE)] = 2
      data[which(data=="noise", arr.ind = TRUE)] = 3
      data[,3] = as.numeric(data[,3])
      datalabel = data[,3]
      data = data[,1:2]
      idnum = c(1,2)
      idbin = NULL
      idcat = NULL
      NOISE = list(
        data = data,
        datalabel = datalabel,
        idnum = idnum,
        idbin = idbin,
        idcat = idcat
      )
    }
    else if (i==4) {
      data = ring_clusters
      data[which(data=="cluster 1", arr.ind = TRUE)] = 1
      data[which(data=="cluster 2", arr.ind = TRUE)] = 2
      data[,3] = as.numeric(data[,3])
      datalabel = data[,3]
      data = data[,1:2]
      idnum = c(1,2)
      idbin = NULL
      idcat = NULL
      RING = list(
        data = data,
        datalabel = datalabel,
        idnum = idnum,
        idbin = idbin,
        idcat = idcat
      )
    }
    else if (i==5) {
      data = halfring_clusters
      data[which(data=="cluster 1", arr.ind = TRUE)] = 1
      data[which(data=="cluster 2", arr.ind = TRUE)] = 2
      data[,3] = as.numeric(data[,3])
      datalabel = data[,3]
      data = data[,1:2]
      idnum = c(1,2)
      idbin = NULL
      idcat = NULL
      HALFRING = list(
        data = data,
        datalabel = datalabel,
        idnum = idnum,
        idbin = idbin,
        idcat = idcat
      )
    }
    else if (i==6) {
      data = iris
      data$Species = as.numeric(data$Species)
      datalabel = data$Species
      data = data[,1:4]
      data = scale(data, center = TRUE, scale = TRUE)
      idnum = c(1:4)
      idbin = NULL
      idcat = NULL
      IRIS = list(
        data = data,
        datalabel = datalabel,
        idnum = idnum,
        idbin = idbin,
        idcat = idcat
      )
    }
    else if (i==7) {
      #59 71 48, so exclude
      data = wine
      datalabel = as.numeric(data$Wine)
      data = data[,1:13]
      idnum = c(1:13)
      idbin = NULL
      idcat = NULL
      WINE = list(
        data = data,
        datalabel = datalabel,
        idnum = idnum,
        idbin = idbin,
        idcat = idcat
      )
    }
    else if (i==8) {
      # 88 12,so exclude
      fertility_Diagnosis = read.csv(paste(datasetpath, "fertility_Diagnosis.txt", sep=""), header=F)
      data = fertility_Diagnosis
      data[which(data=="N", arr.ind = TRUE)] = 1
      data[which(data=="O", arr.ind = TRUE)] = 2
      datalabel = as.numeric(data[,10])
      data = data[,1:9]
      idnum = c(1,2,9)
      idbin = c(3,4,5)
      idcat = c(6,7,8)
      FERTILITY = list(
        data = data,
        datalabel = datalabel,
        idnum = idnum,
        idbin = idbin,
        idcat = idcat
      )
    }
    else if (i==9) {
      #41 20  5 13  4  8 10,so exclude
      zoo = read.csv(paste(datasetpath, "zoo.data", sep=""), header=F)
      data = zoo[,2:18]
      datalabel = as.numeric(data[,17])
      data = data[,1:16]
      idnum = c(13)
      idbin = c(1:12,14:16)
      idcat = NULL
      ZOO = list(
        data = data,
        datalabel = datalabel,
        idnum = idnum,
        idbin = idbin,
        idcat = idcat
      )
    }
    else if (i==10){
      tae = read.csv(paste(datasetpath, "tae.data", sep=""), header=F)
      data = tae
      datalabel = as.numeric(data[,6])
      data = data[,1:5]
      idnum = c(5)
      idbin = c(1,4)
      idcat = c(2,3)
      # set.seed(1)
      # add_one_in_1 = sample(which(datalabel==1),1)
      # datalabel = c(datalabel,datalabel[add_one_in_1])
      # data = rbind(data, matrix(data[add_one_in_1,],1,5))
      # set.seed(1)
      # ext_two_in_3 = sample(which(datalabel==3),2)
      # datalabel = datalabel[-ext_two_in_3]
      # data = data[-ext_two_in_3,]
      TAE = list(
        data = data,
        datalabel = datalabel,
        idnum = idnum,
        idbin = idbin,
        idcat = idcat
      )
    }
    else if (i==11){
      #40 60 36  8  4 27 15  4,so e
      flag = read.csv(paste(datasetpath, "flag.data", sep=""), header=F)
      data = flag[,c(2:ncol(flag))]
      type18 = unique(flag[,18])
      type29 = unique(flag[,29])
      type30 = unique(flag[,30])
      for(i in c(1:length(type18))){
        data[which(data==type18[i], arr.ind = TRUE)] = i
      }
      for(i in c(1:length(type29))){
        data[which(data==type29[i], arr.ind = TRUE)] = i
      }
      for(i in c(1:length(type30))){
        data[which(data==type30[i], arr.ind = TRUE)] = i
      }
      datalabel = data[,6] + 1
      for(i in c(1:29)){data[,i] = as.numeric(data[,i])}
      idnum = c(3,4,7,8,9,18,19,20,21,22,28,29)
      idbin = c(10,11,12,13,14,15,16,23,24,25,26,27)
      idcat = c(1,2,5,17)
      FLAG = list(
        data = data,
        datalabel = datalabel,
        idnum = idnum,
        idbin = idbin,
        idcat = idcat
      )
    }
  }
  datalist = list(
    TAE=TAE, #1-2-3 49-50-52 50
    NOISE=NOISE,#1-2-3 50
    OVERLAPPING=OVERLAPPING, #1-2 50
    RING=RING,#1-2 100
    HALFRING=HALFRING,#1-2 120
    WINE=WINE, #1-2-3 59-71-48
    IRIS=IRIS,#1-2-3 50
    FLAG=FLAG, #1-2-3-4-5-6-7-8 40-60-36-8-4-27-15-4
    ZOO=ZOO, #1-2-3-4-5-6-7 41-20-5-13-4-8-10
    FERTILITY=FERTILITY, #1-2 88-12
    RCOBRE = RCOBRE, #all 10
    MCOBRE = MCOBRE, #all 10
    RSALD = RSALD,
    MSALD = MSALD
  )
  return(datalist)
}
algorithms2 = function(data, idnum, idbin, idcat, train_idx, valid_idx, test_idx, notest_label, dataname, ncluster, fold_idx, resultspath){
  for (i in c(1:4)) {
    if (i==1) {
      my_data = data
      my_train_idx = train_idx
      my_valid_idx = valid_idx
      my_notest_label = notest_label
      #validation
      svm_acc_list = c_list = g_list = c()
      best_acc = Inf
      for (k in c(1:10)) {
        distdata = ssmdf(my_data, idnum, idbin, idcat, my_train_idx, my_notest_label, ncluster, con=k)
        for (j in c(1:length(my_train_idx))) {
          my_label = distknn(distdata, my_notest_label, my_train_idx, my_valid_idx, j)
          my_acc = sum(abs(my_notest_label[my_valid_idx]-my_label[my_valid_idx]))/length(my_valid_idx)
          svm_acc_list = c(svm_acc_list, my_acc)
          c_list = c(c_list, k)
          g_list = c(g_list, j)
          if (my_acc < best_acc) {
            best_acc = my_acc
            best_k = j
            best_con = k
          }
        }
      }
      if (F) {
        counter_data = data.frame(
          svm_acc_list = svm_acc_list,
          c_list = c_list,
          g_list = g_list
        )
        palette <- RColorBrewer::brewer.pal(9, "Pastel1")
        boxp = ggplot(data = counter_data, aes(x=c_list, y=g_list, z=svm_acc_list))
        boxp = boxp + ggplot2::geom_contour_filled(aes(z = svm_acc_list),binwidth=1,position = 'identity')
        boxp = boxp + geom_contour(aes(z = svm_acc_list) ,binwidth=100 ,color="black")
        boxp = boxp + scale_x_continuous(expand = c(0,0))
        boxp = boxp + scale_y_continuous(expand = c(0,0))
        boxp = boxp + coord_flip()
        boxp = boxp + metR::geom_text_contour(aes(z = svm_acc_list),stat="text_contour",binwidth=1,check_overlap = T,
                                              skip=0,stroke = 0,color="red")
        boxp = boxp + labs(x="k_to_weight",y = "g_to_all_train_idx", title = dataname) #title = "Stacked Bar"
        boxp = boxp + theme_bw() 
        boxp = boxp + theme(legend.position = "bottom",
                            legend.direction = "horizontal",
                            legend.title = element_blank(),
                            axis.text.x = element_text(size = 13, vjust = 0.5, hjust = 0.5),#, angle = 15
                            axis.text.y = element_text(size = 13, vjust = 0.5, hjust = 0.5),
                            panel.grid = element_blank(),
                            plot.title = element_text(hjust = 0.5)
        )
        print(boxp)
        png(file = paste(resultspath, dataname, fold_idx, "SSMDNN Contour map.png", sep=""))
        plot(boxp)
        dev.off()
      }
      #test
      distdata = ssmdf(my_data, idnum, idbin, idcat, c(my_train_idx, my_valid_idx), my_notest_label, ncluster, con=best_con)
      tfnn = distknn(distdata, my_notest_label, c(my_train_idx, my_valid_idx), test_idx, best_k)
      
      train_distdata = distdata[c(my_train_idx, my_valid_idx),c(my_train_idx, my_valid_idx)]
      class(train_distdata) = "D2"
      dbglm1 = dbglm(D2 = train_distdata, y = my_notest_label[c(my_train_idx, my_valid_idx)],family = poisson(link = "log"), method="rel.gvar")
      sapmle_num = length(c(my_train_idx, my_valid_idx))-length(test_idx)
      set.seed(1)
      sapmle_idx = sample(c(my_train_idx, my_valid_idx), sapmle_num)
      test_distdata = distdata[c(test_idx, sapmle_idx), c(test_idx, sapmle_idx)]
      class(test_distdata) = "D2"
      dbglm2 = predict(dbglm1,test_distdata,type.pred="response",type.var="D2")
      tfglm = my_notest_label
      tfglm[test_idx] = round(dbglm2[c(1:length(test_idx))])
      
      # rawdata = my_data[,idnum]
      # rawdata = (rawdata-min(rawdata))/(max(rawdata)-min(rawdata))
      # rawdistdata = as.matrix(dist(rawdata, method = "euclidean"))
      # train_distdata = rawdistdata[c(my_train_idx, my_valid_idx),c(my_train_idx, my_valid_idx)]
      # class(train_distdata) = "D2"
      # glm1 = dbglm(D2 = train_distdata, y = my_notest_label[c(my_train_idx, my_valid_idx)],family = poisson(link = "log"), method="rel.gvar")
      # sapmle_num = length(c(my_train_idx, my_valid_idx))-length(test_idx)
      # set.seed(1)
      # sapmle_idx = sample(c(my_train_idx, my_valid_idx), sapmle_num)
      # test_distdata = rawdistdata[c(test_idx, sapmle_idx), c(test_idx, sapmle_idx)]
      # class(test_distdata) = "D2"
      # glm2 = predict(glm1,test_distdata,type.pred="response",type.var="D2")
      # rglm = my_notest_label
      # rglm[test_idx] =  round(glm2[c(1:length(test_idx))])
      cat("TF done \n")
    }
    else if (i==2) {
      #validation
      rawdata = data[,idnum]
      rawdata=(rawdata-min(rawdata))/(max(rawdata)-min(rawdata))
      distdata = as.matrix(dist(rawdata, method = "euclidean"))
      best_mae = Inf
      pred_ksNN = rep(0, length(valid_idx))
      for (j in c(1:length(train_idx))) {
        for (k in c(1:length(valid_idx))) {
          pred_ksNN[k] = rcpp_ksNN(notest_label[train_idx], distdata[valid_idx[k],train_idx],L_C=j)$pred
          pred_ksNN[k] = as.numeric(pred_ksNN[k])
        }
        ksnn_acc = sum(abs(notest_label[valid_idx]-as.numeric(pred_ksNN)))/length(valid_idx)
        if (ksnn_acc < best_mae) {
          best_mae = ksnn_acc
          best_k = j
        }
      }
      #test
      pred_ksNN = rep(0, length(test_idx))
      for (j in c(1:length(test_idx))) {
        pred_ksNN[j] = rcpp_ksNN(notest_label[c(train_idx, valid_idx)], distdata[test_idx[j],c(train_idx, valid_idx)],L_C=best_k)$pred
        pred_ksNN[j] = as.numeric(pred_ksNN[j])
      }
      sknn = notest_label
      sknn[test_idx] = as.numeric(pred_ksNN)
      cat("KSNN done \n")
    }
    else if (i==3) {
      #validation
      rawdata = data[,idnum]
      rawdata = (rawdata-min(rawdata))/(max(rawdata)-min(rawdata))
      rawdata = as.data.frame(cbind(data, as.matrix(notest_label)))
      colnames(rawdata) = paste('v',as.integer(c(1:ncol(rawdata))) ,sep = '')
      colnames(rawdata)[length(colnames(rawdata))] = 'label_col'
      rawdata[, ncol(rawdata)] = factor(rawdata[, ncol(rawdata)])
      best_mae = Inf
      for (j in c(1:8)) {
        svm_model = svm(label_col~.,rawdata[train_idx,], cost  = 10^(j-4), kernel = "linear", scale = F)
        svm_acc = sum(abs(notest_label[valid_idx]-as.numeric(predict(svm_model,rawdata[valid_idx,]))))/length(valid_idx)
        if (svm_acc < best_mae) {
          best_mae = svm_acc
          best_c = 10^(j-4)
        }
      }
      #test
      rawdata[, ncol(rawdata)] = as.numeric(rawdata[, ncol(rawdata)])
      rawdata[test_idx, ncol(rawdata)] = rep(100000,length(test_idx))
      rawdata[, ncol(rawdata)] = factor(rawdata[, ncol(rawdata)])
      svm_model = svm(label_col~.,rawdata[train_idx,] , cost = best_c, kernel = "linear", scale = F)
      lsvm = notest_label
      lsvm[test_idx] = as.numeric(predict(svm_model,rawdata[test_idx,]))
      cat("LSVM done \n")
    }
    else if (i==4) {
      #validation
      rawdata = data[,idnum]
      rawdata = (rawdata-min(rawdata))/(max(rawdata)-min(rawdata))
      rawdata = as.data.frame(cbind(data, as.matrix(notest_label)))
      colnames(rawdata) = paste('v',as.integer(c(1:ncol(rawdata))) ,sep = '')
      colnames(rawdata)[length(colnames(rawdata))] = 'label_col'
      rawdata[, ncol(rawdata)] = factor(rawdata[, ncol(rawdata)])
      best_mae = Inf
      svm_acc_list =c_list=g_list=  c()
      for (j in c(1:8)) {#(length(train_idx)-1)
        for (k in 1/c(0.1,0.2,0.4,0.6,0.8,1.6,3.2,6.4,12.8)) {
          svm_model = svm(label_col~.,rawdata[train_idx,], cost = 10^(j-4), kernel = "radial", gamma = k, scale = F)
          svm_acc = sum(abs(notest_label[valid_idx]-as.numeric(predict(svm_model,rawdata[valid_idx,]))))/length(valid_idx)
          svm_acc_list = c(svm_acc_list, svm_acc)
          c_list = c(c_list, j)
          g_list = c(g_list, k)
          if (svm_acc < best_mae) {
            best_mae = svm_acc
            best_c = 10^(j-4)
            best_g = k
          }
        }
      }
      if (F) {
        counter_data = data.frame(
          svm_acc_list = svm_acc_list,
          c_list = c_list,
          g_list = g_list
        )
        palette <- RColorBrewer::brewer.pal(9, "Pastel1")
        boxp = ggplot(data = counter_data, aes(x=c_list, y=g_list, z=svm_acc_list))
        boxp = boxp + ggplot2::geom_contour_filled(aes(z = svm_acc_list),binwidth=1,position = 'identity')
        boxp = boxp + geom_contour(aes(z = svm_acc_list) ,binwidth=1 ,color="black")
        boxp = boxp + scale_x_continuous(expand = c(0,0))
        boxp = boxp + scale_y_continuous(expand = c(0,0))
        boxp = boxp + coord_flip()
        boxp = boxp + metR::geom_text_contour(aes(z = svm_acc_list),stat="text_contour",binwidth=1,check_overlap = T,
                                              skip=0,stroke = 0,color="red")
        
        boxp = boxp + labs(x="Cost",y = "Gamma", title = dataname) #title = "Stacked Bar"
        boxp = boxp + theme_bw() 
        boxp = boxp + theme(legend.position = "bottom",
                            legend.direction = "horizontal",
                            legend.title = element_blank(),
                            axis.text.x = element_text(size = 13, vjust = 0.5, hjust = 0.5),#, angle = 15
                            axis.text.y = element_text(size = 13, vjust = 0.5, hjust = 0.5),
                            panel.grid = element_blank(),
                            plot.title = element_text(hjust = 0.5)
        )
        print(boxp)
        png(file = paste(resultspath, dataname,fold_idx, " Contour map.png", sep=""))
        plot(boxp)
        dev.off()
      }
      #test
      rawdata[, ncol(rawdata)] = as.numeric(rawdata[, ncol(rawdata)])
      rawdata[test_idx, ncol(rawdata)] = rep(100000,length(test_idx))
      rawdata[, ncol(rawdata)] = factor(rawdata[, ncol(rawdata)])
      svm_model = svm(label_col~.,rawdata[train_idx,] , cost = best_c, kernel = "radial", gamma = best_g, scale = F)
      brfsvm = notest_label
      brfsvm[test_idx] = as.numeric(predict(svm_model,rawdata[test_idx,]))
      cat("RBF SVM done \n")
    }
    else if (i==5) {
      # over sample
      if (length(unique(table(notest_label))) != 1) {
        origin_length = length(notest_label)
        oversamples = oversample(data, notest_label)
        my_data = oversamples$data
        my_notest_label = oversamples$datalabel
        my_idx = c(train_idx, valid_idx, c((origin_length+1) : length(my_notest_label)))
        # divide train valid
        my_train_idx = my_valid_idx = c()
        ranges = unique(my_notest_label)
        for (j in ranges) {
          range_idx = which(my_notest_label == j)
          set.seed(1)
          my_valid_idx = c(my_valid_idx, sample(my_idx, round(length(range_idx)/3)))
          my_train_idx = c(my_train_idx, setdiff(range_idx, my_valid_idx))
        }
      }
      else {
        my_data = data
        my_train_idx = train_idx
        my_valid_idx = valid_idx
        my_notest_label = notest_label
      }
      #validation
      svm_acc_list = c_list = g_list = c()
      best_acc = 0
      for (k in c(1:10)) {
        distdata = ssmdf(my_data, idnum, idbin, idcat, my_train_idx, my_notest_label, ncluster, con=k)
        for (j in c(1:length(my_train_idx))) {
          my_label = distknn(distdata, my_notest_label, my_train_idx, my_valid_idx, j)
          my_acc = sum(my_notest_label[my_valid_idx]==my_label[my_valid_idx])/length(my_valid_idx)*100
          svm_acc_list = c(svm_acc_list, my_acc)
          c_list = c(c_list, k)
          g_list = c(g_list, j)
          if (my_acc > best_acc) {
            best_acc = my_acc
            best_k = j
            best_con = k
          }
        }
      }
      if (F) {
        counter_data = data.frame(
          svm_acc_list = svm_acc_list,
          c_list = c_list,
          g_list = g_list
        )
        palette <- RColorBrewer::brewer.pal(9, "Pastel1")
        boxp = ggplot(data = counter_data, aes(x=c_list, y=g_list, z=svm_acc_list))
        boxp = boxp + ggplot2::geom_contour_filled(aes(z = svm_acc_list),binwidth=1,position = 'identity')
        boxp = boxp + geom_contour(aes(z = svm_acc_list) ,binwidth=100 ,color="black")
        boxp = boxp + scale_x_continuous(expand = c(0,0))
        boxp = boxp + scale_y_continuous(expand = c(0,0))
        boxp = boxp + coord_flip()
        boxp = boxp + metR::geom_text_contour(aes(z = svm_acc_list),stat="text_contour",binwidth=1,check_overlap = T,
                                              skip=0,stroke = 0,color="red")
        boxp = boxp + labs(x="Cost",y = "Gamma", title = dataname) #title = "Stacked Bar"
        boxp = boxp + theme_bw() 
        boxp = boxp + theme(legend.position = "bottom",
                            legend.direction = "horizontal",
                            legend.title = element_blank(),
                            axis.text.x = element_text(size = 13, vjust = 0.5, hjust = 0.5),#, angle = 15
                            axis.text.y = element_text(size = 13, vjust = 0.5, hjust = 0.5),
                            panel.grid = element_blank(),
                            plot.title = element_text(hjust = 0.5)
        )
        print(boxp)
        png(file = paste(resultspath, dataname, fold_idx, "oversample TF Contour map.png", sep=""))
        plot(boxp)
        dev.off()
      }
      #test
      distdata = ssmdf(my_data, idnum, idbin, idcat, c(my_train_idx, my_valid_idx), my_notest_label, ncluster, con=best_con)
      otfnn = distknn(distdata, my_notest_label, c(my_train_idx, my_valid_idx), test_idx, best_k)
      cat("OTF done \n")
    }
  }
  return(list(tfnn = tfnn, sknn = sknn, brfsvm = brfsvm, lsvm = lsvm, tfglm = tfglm))
}
GeomSplitViolin <- ggproto(
  "GeomSplitViolin", 
  GeomViolin, 
  draw_group = function(self, data, ..., draw_quantiles = NULL) {
    data <- transform(data, 
                      xminv = x - violinwidth * (x - xmin), 
                      xmaxv = x + violinwidth * (xmax - x))
    grp <- data[1,'group']
    newdata <- plyr::arrange(
      transform(data, x = if(grp%%2==1) xminv else xmaxv), 
      if(grp%%2==1) y else -y
    )
    newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
    newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
      quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <- GeomPath$draw_panel(both, ...)
      ggplot2:::ggname("geom_split_violin", 
                       grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
    } else {
      ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
    }
  }
)

geom_split_violin <- function (mapping = NULL, 
                               data = NULL, 
                               stat = "ydensity", 
                               position = "identity", ..., 
                               draw_quantiles = NULL, 
                               trim = TRUE, 
                               scale = "area", 
                               na.rm = FALSE, 
                               show.legend = NA, 
                               inherit.aes = TRUE) {
  layer(data = data, 
        mapping = mapping, 
        stat = stat, 
        geom = GeomSplitViolin, 
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes, 
        params = list(trim = trim, 
                      scale = scale, 
                      draw_quantiles = draw_quantiles, 
                      na.rm = na.rm, ...)
  )
}