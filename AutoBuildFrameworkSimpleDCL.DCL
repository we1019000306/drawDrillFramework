framework:dialog{
  label= "钻井柱状图自动化工具";
  :row{

		:boxed_column{
        label="绘制基础框架";
        :row{
			 :button{
			  key = "EFSBtn";  //EraseFrameStructureButton
			  label = "单独删除该模块";
			  fixed_width = true;
			  alignment=left;             
			}		
			:button{
			  key = "DFSBtn"; //DrawFrameStructureButton
			  label = "绘制";
			  fixed_width = true;
			  alignment=right;             
			}
		}
        	
			
		}
  			
			
		:boxed_column{
			label="绘制多段线";

				:row{
					 :button{
					  key = "ELBtn";  //EraseLineButton
					  label = "单独删除该模块";
					  fixed_width = true;
					  alignment=left;             
					}		
					:button{
					  key = "DLBtn";  //DrawLineButton
					  label = "绘制";
					  fixed_width = true;
					  alignment=right;             
					}
				}
		}
			
			
			:boxed_column{
				label="绘制系统组";
				:row{
					 :button{
					  key = "ERSBtn";  //EraseRockStructureButton
					  label = "单独删除该模块";
					  fixed_width = true;
					  alignment=left;             
					}		
					:button{
					  key = "DRSBtn";  //DrawRockStructureButton
					  label = "绘制";
					  fixed_width = true;
					  alignment=right;             
					}
				}
	
			}
		}		
		
		:row{
				:boxed_column{
					label="绘制钻井和测井柱状图";
					
					:row{
						 :button{
						  key = "ECDBtn";  //EraseColumnarDiagramButton
						  label = "单独删除该模块";
						  fixed_width = true;
						  alignment=left;             
						}
						:button{
						  key = "DCDBtn";  //DrawColumnarDiagramButton
						  label = "绘制";
						  fixed_width = true;
						  alignment=right;             
						}
					}
				}
		

				:boxed_column{
						label="绘制标志层代号";
						:row{
							 :button{
							  key = "ERSignBtn";  //EraseRockSignButton
							  label = "单独删除该模块";
							  fixed_width = true;
							  alignment=left;             
							}
							:button{
							  key = "DRSignBtn";  //DrawRockSignButton
							  label = "绘制";
							  fixed_width = true;
							  alignment=right;             
							}
						}
		
				}
				
		
			:boxed_column{
					label="绘制完井结构";
					:row{
						 :button{
						  key = "EDCSBtn";  //EraseDrillCompletionStructureButton
						  label = "单独删除该模块";
						  fixed_width = true;
						  alignment=left;             
						}
						:button{
							key = "DDCSBtn";  //DrawDrillCompletionStructureButton 
							label = "绘制";
							fixed_width = true;
							alignment=right;             
						}
					}
					
			}
			
			
			:boxed_column{
					label="绘制井斜方位角";
					
					:row{
						 :button{
						  key = "EDDBtn";  //EraseDrillDeviationButton
						  label = "单独删除该模块";
						  fixed_width = true;
						  alignment=left;             
						}
						:button{
						  key = "DDDBtn";  //DrawDrillDeviationButton
						  label = "绘制";
						  fixed_width = true;
						  alignment=right;             
						}
					}
			
					}
					
		}
	
	:row{
			:button{
			  key = "startBtn"; 
			  label = "启   动";
			  is_default = true;
			  fixed_width = true;
			  alignment=centered;             
			}
			:button{
			  key = "baseDataBtn"; 
			  label = "更新数据源文件";
			  fixed_width = true;
			  alignment=centered;             
			}
			:button{
			  key = "automationDrawBtn"; 
			  label = "自动绘制所有模块";
			  fixed_width = true;
			  alignment=centered;             
			}
			:button{
				  key = "cancelBtn";  
				  label = "退出";
				  fixed_width = true;
				  alignment=centered;             
			}
	}
}
