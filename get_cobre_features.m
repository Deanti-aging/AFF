function get_cobre_features(dataset_path, results_path, data_name)
if (nargin<3)
  dataset_path = 'D:\NHKJ012_WXG\data\project_cobre\';
  results_path = 'D:\NHKJ012_WXG\result\project_cobre\';
  data_name = 'cobre';
end
%get func features
func_path = [dataset_path 'func'];
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
[mappedX, ~] = compute_mapping(feature_func, 'PCA', no_dims);
feature_func = mappedX;
%get anat features
anat_path = [dataset_path 'anat'];
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
[mappedX, ~] = compute_mapping(feature_anat, 'PCA', no_dims);
feature_anat = mappedX;
%mix anat, funcavg, and sub_information
sub_xls = xlsread([dataset_path 'COBRE_phenotypic_data_change_num.csv']);
age_feature = sub_xls(:, 2);
sex_feature = sub_xls(:, 3);
hand_feature = sub_xls(:, 4);
sub_idx = sub_xls(:, 5);
train_idx = find(sub_idx == 2); 
save([results_path data_name '_train_idx.txt'], 'train_idx', '-ascii'); 
label_feature = age_feature;
% scan_feature = ones(length(label_feature),1);
feature_all = [feature_func, feature_anat, age_feature, sex_feature, hand_feature, label_feature];
% save([results_path data_name '_pca_scan.txt'], 'scan_feature', '-ascii'); 
save([results_path data_name '_pca_data.txt'], 'feature_all', '-ascii'); 
save([results_path data_name '_pca_func.txt'], 'feature_func', '-ascii'); 
save([results_path data_name '_pca_anat.txt'], 'feature_anat', '-ascii'); 
save([results_path data_name '_pca_age.txt'], 'age_feature', '-ascii'); 
save([results_path data_name '_pca_sex.txt'], 'sex_feature', '-ascii'); 
save([results_path data_name '_pca_label.txt'], 'label_feature', '-ascii'); 
save([results_path data_name '_pca_hand.txt'], 'hand_feature', '-ascii'); 


% %%%%%%%%%%%%%%%%%%%%%pre
% %get func features
% func_path = [results_path 'pre_func'];
% func_list = dir_NameList(func_path);
% ncols = length(func_list);
% [volumn, ~, ~] = rp_readfile([func_path filesep func_list{1}]);
% sizes = size(volumn(:)');
% nrows = sizes(2);
% feature_func = zeros(ncols, nrows);
% for i = 1:length(func_list)
%     [volumn, ~, ~] = rp_readfile([func_path filesep func_list{i}]);
%     feature_func(i, :) = volumn(:)';
%     disp(['the func run idx = ' num2str(i)]);
% end
% %pca func features
% feature_func = single(feature_func);
% disp('converted' );
% [~,k]=size(feature_func);
% feature_func=zscore(feature_func(:,1:k));  %归一化数据
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
% get anat features
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
% %mix anat, funcavg, and sub_information
% sub_xls = xlsread([dataset_path 'COBRE_phenotypic_data_change_num.csv']);
% age_feature = sub_xls(:, 2);
% sex_feature = sub_xls(:, 3);
% hand_feature = sub_xls(:, 4);
% sub_idx = sub_xls(:, 5);
% train_idx = find(sub_idx == 2); 
% save([results_path data_name '_train_idx.txt'], 'train_idx', '-ascii'); 
% label_feature = age_feature;
% % scan_feature = ones(length(label_feature),1);
% feature_all = [feature_func, feature_anat, age_feature, sex_feature, hand_feature, label_feature];
% % save([results_path data_name '_pca_scan.txt'], 'scan_feature', '-ascii'); 
% save([results_path data_name '_pre_data.txt'], 'feature_all', '-ascii'); 
% save([results_path data_name '_pre_func.txt'], 'feature_func', '-ascii'); 
% save([results_path data_name '_pre_anat.txt'], 'feature_anat', '-ascii'); 
% save([results_path data_name '_pre_age.txt'], 'age_feature', '-ascii'); 
% save([results_path data_name '_pre_sex.txt'], 'sex_feature', '-ascii'); 
% save([results_path data_name '_pre_label.txt'], 'label_feature', '-ascii'); 
% save([results_path data_name '_pre_hand.txt'], 'hand_feature', '-ascii'); 
end