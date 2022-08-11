framework:dialog{ //定义名字为yuan的对话框
  label= "绘制整体架构";//显示在对话框顶部的标签
  :row{//引用行
  	:boxed_column{//引用加框列1
        label="作图结构化数据";//加框列的标签
        
        	:edit_box{ //引用编辑框1
      			label = "井深Max："; //编辑框1的标签
     	 		edit_width = 8; //编辑框1的宽度
     	 		key = "D";//编辑框1的关键字
     			mnemonic = "D";//编辑框1的热键
     			value = 100;
        	}//编辑框1引用结束
        	:edit_box{ //引用编辑框2
      			label = "消耗量Max："; //编辑框2的标签&之后的字符是热键Maximum Consumption
      			edit_width = 8; //编辑框2的宽度
      			key = "MC";//编辑框2的关键字
      	       		mnemonic = "MC";
      	       		value = 0.51;
      		}//编辑框2引用结束

      		:edit_box{ //引用编辑框3
  		        label = "钻时Max："; //编辑框3的标签
  	 	        edit_width = 8; //编辑框3的宽度
                        key = "DT";//编辑框3的关键字
  	  		mnemonic = "DT";//编辑框3的热键
  	  		value = 48;
    	  	}//编辑框3引用结束

    	  	:edit_box{ //引用编辑框4
  	 	 	label = "水位Max："; //编辑框4的标签
  	  		edit_width = 8; //编辑框4的宽度
   	  		key = "WL";//编辑框4的关键字
  	  		mnemonic = "WL";//编辑框4的热键
  	  		value = 121;
      		}//编辑框4引用结束
			
			:button{
			  key = "DFBtn";  //DrawFrameButton
			  label = "绘制框架";
			  fixed_width = true;
			  alignment=right;             
			}
		}//加框列引用结束
			
			:boxed_column{//引用加框列2
				label="根据EXCEL绘制多段线";//加框列的标签
        
				:edit_box{ //引用编辑框1
					label = "列1(eg A1:A10)："; //编辑框1的标签
					edit_width = 8; //编辑框1的宽度
					key = "columnPLineOne";//编辑框1的关键字 
					value = "A1:A10";//默认A1
				}//编辑框1引用结束

				:edit_box{ //引用编辑框2
					label = "列2(eg A1:A10)："; //编辑框2的标签&之后的字符是热键Maximum Consumption
					edit_width = 8; //编辑框2的宽度
					key = "columnPLineTwo";//编辑框2的关键字 
					value = "B1：B10";
				}//编辑框2引用结束

				:popup_list{label="线宽：";
						edit_width = 15;
						list="ByLayer\n\1.00\n待开发";
						key= "LineWeight_List";
				}		
				
				:popup_list{label="线名：";
						edit_width = 15;
						list="钻时曲线\n\消耗量曲线\n水位曲线\n普通曲线\n待开发";
						key= "LineName_List";
				}		
			
				:popup_list{label="线型：";
						edit_width = 15;
						list="ByLayer\n\ACAD_ISO02W100\n待开发";
						key= "LineType_List";
				}		
				
				:button{
				  key = "DLBtn";  //DrawLineButton
				  label = "绘制";
				  fixed_width = true;
				  alignment=right;             
				}

	
    	}//加框列引用结束
		//
		:boxed_column{//引用加框列5
				label="根据EXCEL绘制系统组";//加框列的标签
    
				:popup_list{label="系统组：";
						edit_width = 15;
						list="系\n\统\n\组";
						key= "RockStructureType_List";
				}		    
	
				:edit_box{ //引用编辑框1
					label = "深度列(eg A1:A10)："; //编辑框1的标签
					edit_width = 8; //编辑框1的宽度
					key = "columnRockStructureOne";//编辑框1的关键字 
					value = "A1:A10";//默认A1
				}//编辑框1引用结束
				:edit_box{ //引用编辑框2
					label = "厚度列(eg A1:A10)：";
					edit_width = 8; //编辑框2的宽度
					key = "columnRockStructureTwo";//编辑框5的关键字 
					value = "B1：B10";
				}//编辑框2引用结束
				:edit_box{ //引用编辑框3
					label = "全称列(eg A1:A10)："; 
					edit_width = 8; //编辑框2的宽度
					key = "columnRockStructureThree";//编辑框3的关键字 
					value = "C1：C10";
				}//编辑框3引用结束
				:edit_box{ //引用编辑框4
					label = "简称列(eg A1:A10)：";
					edit_width = 8; 
					key = "columnRockStructureFour";
					value = "D1：D10";
				}//编辑框4引用结束
	
					
				:button{
				  key = "DRSBtn";  //DrawRockStructureButton
				  label = "绘制";
				  fixed_width = true;
				  alignment=right;             
				}
			}//加框列引用结束
	}//行引用结束
		
		
	:button{
				  key = "startBtn";  //DrawLineButton
				  label = "启动";
				  is_default = true;
				  fixed_width = true;
				  alignment=centered;             
				}
	:button{
				  key = "cancelBtn";  //DrawLineButton
				  label = "退出";
				  fixed_width = true;
				  alignment=centered;             
				}
}
