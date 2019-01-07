framework:dialog{
  label= "x";
  :row{

		:boxed_column{
        label="x";
        :row{
			 :button{
			  key = "EFSBtn";  //EraseFrameStructureButton
			  label = "x";
			  fixed_width = true;
			  alignment=left;             
			}		
			:button{
			  key = "DFSBtn"; //DrawFrameStructureButton
			  label = "x";
			  fixed_width = true;
			  alignment=right;             
			}
		}
        	
			
		}
  			
			
		:boxed_column{
			label="x";

				:row{
					 :button{
					  key = "ELBtn";  //EraseLineButton
					  label = "x";
					  fixed_width = true;
					  alignment=left;             
					}		
					:button{
					  key = "DLBtn";  //DrawLineButton
					  label = "x";
					  fixed_width = true;
					  alignment=right;             
					}
				}
		}
			
			
			:boxed_column{
				label="x";
				:row{
					 :button{
					  key = "ERSBtn";  //EraseRockStructureButton
					  label = "x";
					  fixed_width = true;
					  alignment=left;             
					}		
					:button{
					  key = "DRSBtn";  //DrawRockStructureButton
					  label = "x";
					  fixed_width = true;
					  alignment=right;             
					}
				}
	
			}
		}		
		
		:row{
				:boxed_column{
					label="x";
					
					:row{
						 :button{
						  key = "ECDBtn";  //EraseColumnarDiagramButton
						  label = "x";
						  fixed_width = true;
						  alignment=left;             
						}
						:button{
						  key = "DCDBtn";  //DrawColumnarDiagramButton
						  label = "x";
						  fixed_width = true;
						  alignment=right;             
						}
					}
				}
		

				:boxed_column{
						label="x";
						:row{
							 :button{
							  key = "ERSignBtn";  //EraseRockSignButton
							  label = "x";
							  fixed_width = true;
							  alignment=left;             
							}
							:button{
							  key = "DRSignBtn";  //DrawRockSignButton
							  label = "x";
							  fixed_width = true;
							  alignment=right;             
							}
						}
		
				}
				
		
			:boxed_column{
					label="x";
					:row{
						 :button{
						  key = "EDCSBtn";  //EraseDrillCompletionStructureButton
						  label = "x";
						  fixed_width = true;
						  alignment=left;             
						}
						:button{
							key = "DDCSBtn";  //DrawDrillCompletionStructureButton 
							label = "x";
							fixed_width = true;
							alignment=right;             
						}
					}
					
			}
			
			
			:boxed_column{
					label="x";//鍔犳鍒楃殑鏍囩    
					
					:row{
						 :button{
						  key = "EDDBtn";  //EraseDrillDeviationButton
						  label = "x";
						  fixed_width = true;
						  alignment=left;             
						}
						:button{
						  key = "DDDBtn";  //DrawDrillDeviationButton
						  label = "x";
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
