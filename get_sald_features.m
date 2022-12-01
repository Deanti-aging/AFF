function get_sald_features(dataset_path, results_path, data_name)
if (nargin<3)
  dataset_path = 'D:\NHKJ012_WXG\data\project_sald\';
  results_path = 'D:\NHKJ012_WXG\result\project_sald\';
  data_name = 'sald';
end
%get func features
func_path = [results_path 'pca_func'];
func_list = dir_NameList(func_path);
ncols = length(func_list);
[volumn, ~, ~] = rp_readfile([func_path filesep func_list{1}]);
sizes = size(volumn(:)');
nrows = sizes(2);
feature_func = zeros(ncols, nrows);
for i = 1:length(func_list)
    [volumn, ~, ~] = rp_readfile([func_path filesep func_list{i}]);
    feature_func(i, :) = volumn(:)';
    disp(['the func run idx = ' num2str(i)]);
end
%pca func features
feature_func = single(feature_func);
disp('converted' );
[~,k]=size(feature_func);
feature_func=zscore(feature_func(:,1:k));  %归一化数据
% [~,score,latent]=pca(feature_func);
% latent = 100*latent/sum(latent);
% p = 0;
% for i = 1:length(latent)
%     p = p + latent(i);
%     if p >= 95
%         feature_num = i;
%         break;
%     end
% end
% feature_func = double(score(:, 1: feature_num)) ;
no_dims = round(intrinsic_dim(feature_func, 'MLE'));
[mappedX, ~] = compute_mapping(feature_func, 'PCA', 61);%no_dims
feature_func = mappedX;
%get anat features
anat_path = [results_path 'pca_anat'];
anat_list = dir_NameList(anat_path);
ncols = length(anat_list);
[volumn, ~, ~] = rp_readfile([anat_path filesep anat_list{1}]);
sizes = size(volumn(:)');
nrows = sizes(2);
feature_anat = zeros(ncols, nrows);
for i = 1:length(anat_list)
    [volumn, ~, ~] = rp_readfile([anat_path filesep anat_list{i}]);
    feature_anat(i, :) = volumn(:)';
    disp(['the anat run idx = ' num2str(i)]);
end
%pca anat features
[~,k]=size(feature_anat);
feature_anat=zscore(feature_anat(:,1:k));  %归一化数据
% [~,score,latent]=pca(feature_anat);
% latent = 100*latent/sum(latent);
% p = 0;
% for i = 1:length(latent)
%     p = p + latent(i);
%     if p >= 95
%         feature_num = i;
%         break;
%     end
% end
% feature_anat = score(:, 1: feature_num);
no_dims = round(intrinsic_dim(feature_anat, 'MLE'));
[mappedX, ~] = compute_mapping(feature_anat, 'PCA', 21);%no_dims
feature_anat = mappedX;
%mix anat, funcavg, and sub_information
sub_xls = xlsread([dataset_path 'sub_information.xlsx']);
age_feature = sub_xls(:, 1);
sex_feature = sub_xls(:, 2);
% scanner_feature = ones(length(sex_feature),1);
label_feature = age_feature;
QC_anatomical_feature = xlsread([dataset_path 'sub_information.xlsx'], 'QC_anatomical spatial');
QC_functional_spatial = xlsread([dataset_path 'sub_information.xlsx'], 'QC_functional spatial');
QC_functional_temporal = xlsread([dataset_path 'sub_information.xlsx'], 'QC_functional temporal');
feature_all = [feature_func, feature_anat, QC_anatomical_feature, QC_functional_spatial, QC_functional_temporal, age_feature, sex_feature,  label_feature];
save([results_path data_name '_pca_data.txt'], 'feature_all', '-ascii'); 
save([results_path data_name '_pca_func.txt'], 'feature_func', '-ascii'); 
save([results_path data_name '_pca_anat.txt'], 'feature_anat', '-ascii'); 
save([results_path data_name '_pca_age.txt'], 'age_feature', '-ascii'); 
save([results_path data_name '_pca_sex.txt'], 'sex_feature', '-ascii'); 
save([results_path data_name '_pca_label.txt'], 'label_feature', '-ascii'); 
save([results_path data_name '_pca_qcaf.txt'], 'QC_anatomical_feature', '-ascii'); 
save([results_path data_name '_pca_qcfs.txt'], 'QC_functional_spatial', '-ascii'); 
save([results_path data_name '_pca_qcft.txt'], 'QC_functional_temporal', '-ascii'); 
%save  D:\NHKJ012_WXG\project\scanner_feature.txt  scanner_feature -ascii

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%pre
% %get func features
% func_path = [results_path 'pre_func'];
% func_list = dir_NameList(func_path);
% ncols = length(func_list);
% [volumn, ~, ~] = rp_readfile([func_path filesep func_list{1}]);
% sizes = size(volumn(:)');
% nrows = sizes(2);
% feature_func = zeros(ncols, nrows);
% for i = 1:length(func_list)
%     %file_name = dir_NameList([func_path filesep func_list{i}]);
%     %[volumn, ~, ~] = rp_readfile([func_path filesep func_list{i} filesep file_name{1}]);
%     [volumn, ~, ~] = rp_readfile([func_path filesep func_list{i}]);
%     %feature_func = [feature_func; volumn(:)'];
%     feature_func(i, :) = volumn(:)';
%     disp(['the func run idx = ' num2str(i)]);
% end
% %pre func features
% feature_func = single(feature_func);
% [~,k]=size(feature_func);
% feature_func=zscore(feature_func(:,1:k));  %归一化数据
% % [~,score,latent]=pca(feature_func);
% % latent = 100*latent/sum(latent);
% % p = 0;
% % for i = 1:length(latent)
% %     p = p + latent(i);
% %     if p >= 95
% %         feature_num = i;
% %         break;
% %     end
% % end
% % feature_func = double(score(:, 1: feature_num)) ;
% no_dims = round(intrinsic_dim(feature_func, 'MLE'));
% [mappedX, ~] = compute_mapping(feature_func, 'PCA', no_dims);
% feature_func = mappedX;
% %get anat features
% anat_path = [results_path 'pre_anat'];
% anat_list = dir_NameList(anat_path);
% ncols = length(anat_list);
% [volumn, ~, ~] = rp_readfile([anat_path filesep anat_list{1}]);
% sizes = size(volumn(:)');
% nrows = sizes(2);
% feature_anat = zeros(ncols, nrows);
% for i = 1:length(anat_list)
%     [volumn, ~, ~] = rp_readfile([anat_path filesep anat_list{i}]);
%     feature_anat(i, :) = volumn(:)';
%     disp(['the anat run idx = ' num2str(i)]);
% end
% %pca anat features
% [~,k]=size(feature_anat);
% feature_anat=zscore(feature_anat(:,1:k));  %归一化数据
% % [~,score,latent]=pca(feature_anat);
% % latent = 100*latent/sum(latent);
% % p = 0;
% % for i = 1:length(latent)
% %     p = p + latent(i);
% %     if p >= 95
% %         feature_num = i;
% %         break;
% %     end
% % end
% % feature_anat = score(:, 1: feature_num);
% no_dims = round(intrinsic_dim(feature_anat, 'MLE'));
% [mappedX, ~] = compute_mapping(feature_anat, 'PCA', no_dims);
% feature_anat = mappedX;
% %mix anat, funcavg, and sub_information
% sub_xls = xlsread([dataset_path 'sub_information.xlsx']);
% age_feature = sub_xls(:, 1);
% sex_feature = sub_xls(:, 2);
% % scanner_feature = ones(length(sex_feature),1);
% label_feature = age_feature;
% QC_anatomical_feature = xlsread([dataset_path 'sub_information.xlsx'], 'QC_anatomical spatial');
% QC_functional_spatial = xlsread([dataset_path 'sub_information.xlsx'], 'QC_functional spatial');
% QC_functional_temporal = xlsread([dataset_path 'sub_information.xlsx'], 'QC_functional temporal');
% feature_all = [feature_func, feature_anat, QC_anatomical_feature, QC_functional_spatial, QC_functional_temporal, age_feature, sex_feature,  label_feature];
% save([results_path data_name '_pre_data.txt'], 'feature_all',  '-ascii'); 
% save([results_path data_name '_pre_func.txt'], 'feature_func', '-ascii'); 
% save([results_path data_name '_pre_anat.txt'], 'feature_anat', '-ascii'); 
% save([results_path data_name '_pre_age.txt'], 'age_feature', '-ascii'); 
% save([results_path data_name '_pre_sex.txt'], 'sex_feature', '-ascii'); 
% save([results_path data_name '_pre_label.txt'], 'label_feature', '-ascii'); 
% save([results_path data_name '_pre_qcaf.txt'], 'QC_anatomical_feature', '-ascii'); 
% save([results_path data_name '_pre_qcfs.txt'], 'QC_functional_spatial', '-ascii'); 
% save([results_path data_name '_pre_qcft.txt'], 'QC_functional_temporal', '-ascii'); 
end