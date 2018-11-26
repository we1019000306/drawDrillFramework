framework:dialog{ //定义名字为yuan的对话框
  label= "绘制钻井图外挂";//显示在对话框顶部的标签
  :row{//引用行

		:boxed_column{//引用加框列
        label="钻井基础信息表";//加框列的标签
		:image{ 
			width=15;
		   aspect_ratio=0.15;
		   key= "FrameImg";
		   color=red;
       }
	   :text{ 
			width=15;
		   is_bold=true;
		   key="FrameText";
		   value="正在绘制中......";
       }

        :row{
			 :button{
			  key = "EFSBtn";  //EraseFrameStructureButton 单独删除系统组所有已画部分
			  label = "单独清除该模式下已绘模型";
			  fixed_width = true;
			  alignment=left;             
			}		
			:button{
			  key = "DFSBtn"; //DrawFrameStructureButton
			  label = "绘制框架";
			  fixed_width = true;
			  alignment=right;             
			}
		}
        	
			
		}//加框列引用结束 
  			
			
		:boxed_column{//引用加框列2
			label="根据EXCEL绘制多段线";//加框列的标签
				:image{ 
					width=15;
				   aspect_ratio=0.15;
				   key= "LineImg";
				   color=white;
			   }
			   :text{ 
					width=15;
				   is_bold=true;
				   key="LineText";
				   value="请稍后......";
			   }

				:row{
					 :button{
					  key = "ELBtn";  //EraseLineButton 单独删除系统组所有已画部分
					  label = "单独清除该模式下已绘模型";
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
			}//加框列引用结束
			
			
			:boxed_column{//引用加框列5
				label="根据EXCEL绘制系统组";//加框列的标签
				
				:image{ 
					width=15;
				   aspect_ratio=0.15;
				   key= "RSImg";
				   color=white;
			   }
			   :text{ 
					width=15;
				   is_bold=true;
				   key="RSText";
				   value="请稍后......";
			   }
				
				:row{
					 :button{
					  key = "ERSBtn";  //EraseRockStructureButton 单独删除系统组所有已画部分
					  label = "单独清除该模式下已绘模型";
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
	
			}//加框列引用结束
	}//行引用结束
		
		:row{//引用行
				:boxed_column{//引用加框列5
					label="根据EXCEL绘制井柱状图";//加框列的标签
				
					:image{ 
						width=15;
					   aspect_ratio=0.15;
					   key= "CDImg";
					   color=white;
				   }
				   :text{ 
						width=15;
					   is_bold=true;
					   key="CDText";
					   value="请稍后......";
				   }
					
					:row{
						 :button{
						  key = "ECDBtn";  //EraseColumnarDiagramButton 单独删除柱状图所有已画部分
						  label = "单独清除该模式下已绘模型";
						  fixed_width = true;
						  alignment=left;             
						}
						:button{
						  key = "DRBtn";  //DrawRockButton
						  label = "绘制";
						  fixed_width = true;
						  alignment=right;             
						}
					}
				}//加框列引用结束
		

				:boxed_column{
						label="根据EXCEL绘制标志层代号";//加框列的标签    
						:image{ 
							width=15;
						   aspect_ratio=0.15;
						   key= "RSignImg";
						   color=white;
						}
					   :text{ 
							width=15;
						   is_bold=true;
						   key="RSignText";
						   value="请稍后......";
					   }
						:row{
							 :button{
							  key = "ERSignBtn";  //EraseRockSignButton 单独删除标志层代号所有已画部分
							  label = "单独清除该模式下已绘模型";
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
		
				}//加框列引用结束
				
		
			:boxed_column{//引用加框列3
					label="完井结构";//加框列的标签
					:image{ 
							width=15;
						   aspect_ratio=0.15;
						   key= "DCSImg";
						   color=white;
						}
					   :text{ 
							width=15;
						   is_bold=true;
						   key="DCSText";
						   value="请稍后......";
					   }
					:row{
						 :button{
						  key = "EDCSBtn";  //EraseDrillCompletionStructureButton 单独删除柱状图所有已画部分
						  label = "单独清除该模式下已绘模型";
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
					
			}//加框列引用结束
			
			
			:boxed_column{
					label="根据EXCEL绘制井斜方位角";//加框列的标签    
					:image{ 
							width=15;
						   aspect_ratio=0.15;
						   key= "DDImg";
						   color=white;
						}
					   :text{ 
							width=15;
						   is_bold=true;
						   key="DDText";
						   value="请稍后......";
					   }	
					:row{
						 :button{
						  key = "EDDBtn";  //EraseDrillDeviationButton 单独删除井斜方位角所有已画部分
						  label = "单独清除该模式下已绘模型";
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
			
					}//加框列引用结束
					
		}//行引用结束
		
	:button{
		  key = "startBtn"; 
		  label = "启动";
		  is_default = true;
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
