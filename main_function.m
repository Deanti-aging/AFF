function main_function()
    dataset_cobre_path = 'D:\NHKJ012_WXG\data\project_cobre\';
    result_cobre_path = 'D:\NHKJ012_WXG\result\project_cobre\';
%     dataset_neurocon_path = 'D:\NHKJ012_WXG\data\project_neurocon\';
%     result_neurocon_path = 'D:\NHKJ012_WXG\result\project_neurocon\';
    dataset_sald_path = 'D:\NHKJ012_WXG\data\project_sald\';
    result_sald_path = 'D:\NHKJ012_WXG\result\project_sald\';
%     dataset_taowu_path = 'D:\NHKJ012_WXG\data\project_taowu\';
%     result_taowu_path = 'D:\NHKJ012_WXG\result\project_taowu\';
%     code_path = 'D:\NHKJ012_WXG\code\';
%     combine_path = 'D:\NHKJ012_WXG\result\project_combine\';
%     batch_smri(dataset_neurocon_path, result_neurocon_path, code_path, 'neurocon');
%     get_pd_neurocon_features(dataset_neurocon_path, result_neurocon_path, 'neurocon');
%     batch_smri(dataset_taowu_path, result_taowu_path, code_path, 'taowu');
%     get_pd_taowu_features(dataset_taowu_path, result_taowu_path, 'taowu');
   
  
    get_cobre_features(dataset_cobre_path, result_cobre_path, 'cobre');
%     batch_smri(dataset_sald_path, result_sald_path, code_path, 'sald');
%     batch_fmri(dataset_sald_path, result_sald_path, code_path);
    get_sald_features(dataset_sald_path, result_sald_path, 'sald');
   
end