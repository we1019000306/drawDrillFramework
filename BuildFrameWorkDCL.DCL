framework:dialog{ //定义名字为yuan的对话框
  label= "绘制钻井图外挂";//显示在对话框顶部的标签
  :row{//引用行
	:boxed_column{//引用加框列
        label="钻井基础信息";//加框列的标签
        
        	:edit_box{ 
      			label = "井号："; //编辑框标签
     	 		edit_width = 20; //编辑框宽度
     	 		key = "DN";//编辑框关键字 DrillName
     			value = "SHCK-";
        	}

        	:edit_box{ 
      			label = "井位：";
      			edit_width = 20; 
      			key = "DL";//编辑框关键字 drillLocation
      	       		value = "县村镇";
      		}

      		:edit_box{ 
  		        label = "坐标X："; 
  	 	        edit_width = 20; 
                key = "LX";//LocationX
				value = "x";
    	  	}

    	  	:edit_box{ 
				label = "坐标Y："; 
				edit_width = 20; 
				key = "LY";//LocationY
				value = "y";
      		}
			:edit_box{ 
				label = "高度H："; 
				edit_width = 20; 
				key = "LH";//LocationHeight
				value = "h";
      		}
			:edit_box{ 
				label = "施工单位："; 
				edit_width = 40; 
				key = "Company";
				value = "山西蓝焰煤层气集团有限责任公司勘探分公司";
      		}
			:edit_box{ 
				label = "开工日期："; 
				edit_width = 12; 
				key = "StartDay";
				value = "年月日";
      		}
			:edit_box{ 
				label = "完工日期："; 
				edit_width = 12; 
				key = "EndDay";
				value = "年月日";
      		}
			:edit_box{ 
				label = "钻机类型："; 
				edit_width = 8; 
				key = "DrillType";
				value = "TSJ-2000";
      		}
			:edit_box{ 
				label = "水泵："; 
				edit_width = 8; 
				key = "WaterPump";
				value = "TBW-1200";
      		}
			:edit_box{ 
				label = "机动力："; 
				edit_width = 8; 
				key = "MachinePower";
				value = "12V135";
      		}
		}//加框列引用结束
 :boxed_column{//引用加框列
        label="制图基础信息";
        
        	:edit_box{ 
      			label = "制图：";
     	 		edit_width = 15;
     	 		key = "PictureMaker";
     			value = "PictureMaker";
        	}

        	:edit_box{ 
      			label = "校对：";
      			edit_width = 15; 
      			key = "PictureProofreader";
      	       		value = "PictureProofreader";//校对
      		}

      		:edit_box{ 
  		        label = "审核："; 
  	 	        edit_width = 15; 
                key = "PictureChecker";
				value = "PictureChecker";
    	  	}

    	  	:edit_box{ 
				label = "机长："; 
				edit_width = 15; 
				key = "DrillCommander";
				value = "DrillCommander";
      		}
			:edit_box{ 
				label = "技术负责："; 
				edit_width = 15; 
				key = "TechnicalLeader";//技术负责
				value = "TechnicalLeader";
      		}
			:edit_box{ 
				label = "项目经理："; 
				edit_width = 15; 
				key = "DrillManager";
				value = "DrillManager";
      		}
			:edit_box{ 
				label = "图号："; 
				edit_width = 15; 
				key = "PictureNumber";
				value = "PictureNumber";
      		}
			:edit_box{ 
				label = "比例："; 
				edit_width = 15; 
				key = "Proportion";
				value = "1:200";
      		}
			:edit_box{ 
				label = "制作日期："; 
				edit_width = 15; 
				key = "PictureTime";
				value = "年月日";
      		}
			:edit_box{ 
				label = "资料来源："; 
				edit_width = 40; 
				key = "DataSource";
				value = "山西蓝焰煤层气集团有限责任公司勘探分公司";
      		}
		}//加框列引用结束 
  
  
  
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
		:row{//引用行
				:boxed_column{//引用加框列5
				label="根据EXCEL绘制井柱状图";//加框列的标签
    
				:popup_list{label="柱状图类型：";
						edit_width = 15;
						list="钻井柱状图\n\测井柱状图\n待开发";
						key= "RockType_List";
				}		    
	
				:edit_box{ //引用编辑框1
					label = "深度列(eg A1:A10)："; //编辑框1的标签
					edit_width = 8; //编辑框1的宽度
					key = "columnRockOne";//编辑框1的关键字 
					value = "A1:A10";//默认A1
				}//编辑框1引用结束
				:edit_box{ //引用编辑框2
					label = "层厚列(eg A1:A10)：";
					edit_width = 8; //编辑框2的宽度
					key = "columnRockTwo";//编辑框5的关键字 
					value = "B1：B10";
				}//编辑框2引用结束
				:edit_box{ //引用编辑框3
					label = "岩性名称列(eg A1:A10)："; 
					edit_width = 8; //编辑框2的宽度
					key = "columnRockThree";//编辑框3的关键字 
					value = "C1：C10";
				}//编辑框3引用结束
				:edit_box{ //引用编辑框4
					label = "岩性描述列(eg A1:A10)：";
					edit_width = 8; 
					key = "columnRockFour";
					value = "D1：D10";
				}//编辑框4引用结束
				:edit_box{ //引用编辑框5
					label = "层号列(eg A1:A10)："; 
					edit_width = 8; //
					key = "columnRockFive";//编辑框5的关键字 
					value = "E1：E10";
				}//编辑框5引用结束
	
				
				//:popup_list{label="文字大小：";
					//	edit_width = 15;
						//list="3.00\n\2.00\n1.00\n待开发";
						//key= "RockTextSize_List";
				//}		
					
				:button{
				  key = "DRBtn";  //DrawRockButton
				  label = "绘制";
				  fixed_width = true;
				  alignment=right;             
				}

	
			}//加框列引用结束
		
		
		
		
		
				:boxed_column{
						label="根据EXCEL绘制标志层代号";//加框列的标签    
						:edit_box{ //引用编辑框1
							label = "深度列(eg A1:A10)："; //编辑框1的标签
							edit_width = 8; //编辑框1的宽度
							key = "columnRockSignOne";//编辑框1的关键字 
							value = "A1:A10";//默认A1
						}//编辑框1引用结束
						:edit_box{ //引用编辑框2
							label = "层厚列(eg A1:A10)：";
							edit_width = 8; //编辑框2的宽度
							key = "columnRockSignTwo";//编辑框5的关键字 
							value = "B1：B10";
						}//编辑框2引用结束
						:edit_box{ //引用编辑框3
							label = "标志层名称列(eg A1:A10)："; 
							edit_width = 8; //编辑框2的宽度
							key = "columnRockSignThree";//编辑框3的关键字 
							value = "C1：C10";
						}//编辑框3引用结束
						
						:button{
						  key = "DRSignBtn";  //DrawRockSignButton
						  label = "绘制";
						  fixed_width = true;
						  alignment=right;             
						}

			
					}//加框列引用结束
				
		
		
		
			:boxed_column{//引用加框列3
					label="完井结构";//加框列的标签
					
					:popup_list{label="开孔数：";
							edit_width = 15;
							list="二开\n\三开\n待开发";
							key= "HoleNumber_List";
					}		
					
					:edit_box{ //引用编辑框1
						label = "一开钻头直径(mm)："; //编辑框1的标签
						edit_width = 8; //编辑框1的宽度
						key = "HoleOneDrillDiameter";//编辑框1的关键字  HoleOneDrillDiameter一开钻头直径
						value = "φ425.00mm";//默认A1
					}//编辑框1引用结束

					:edit_box{ //引用编辑框2
						label = "一开井深(m)："; //编辑框2的标签&之后的字符是热键Maximum Consumption
						edit_width = 8; //编辑框2的宽度
						key = "HoleOneDeep";//编辑框2的关键字 
						value = 0.0;
					}//编辑框2引用结束

						:edit_box{ //引用编辑框1
						label = "一开套管外径(mm)："; //编辑框1的标签
						edit_width = 8; //编辑框1的宽度
						key = "HoleOneBushDiameter";//编辑框1的关键字  HoleOneBushDiameter一开钻头直径
						value = "J55φ377.7×10.00mm";//默认A1
					}//编辑框1引用结束

					:edit_box{ //引用编辑框2
						label = "一开套管下深(m)："; //编辑框2的标签&之后的字符是热键Maximum Consumption
						edit_width = 8; //编辑框2的宽度
						key = "HoleOneBushDeep";//编辑框2的关键字 
						value = 0.0;
					}//编辑框2引用结束
					
					:edit_box{ //引用编辑框7
						label = "一开水泥上返深度(m)："; //编辑框7的标签
						edit_width = 8; //编辑框7的宽度
						key = "HoleOneCement";//编辑框7的关键字  水泥上反
						value = 0.0;
					}//编辑框7引用结束
					
					:edit_box{ //引用编辑框3
						label = "二开钻头直径(mm)："; //编辑框1的标签
						edit_width = 8; //编辑框1的宽度
						key = "HoleTwoDrillDiameter";//编辑框1的关键字  HoleOneDrillDiameter一开钻头直径
						value = "φ311.15mm";//默认A1
					}//编辑框1引用结束

					:edit_box{ //引用编辑框4
						label = "二开井深(m)："; //编辑框2的标签&之后的字符是热键Maximum Consumption
						edit_width = 8; //编辑框2的宽度
						key = "HoleTwoDeep";//编辑框2的关键字 
						value = 0.0;
					}//编辑框2引用结束
					
					:edit_box{ //引用编辑框3
						label = "二开套管外径(mm)："; //编辑框1的标签
						edit_width = 8; //编辑框1的宽度
						key = "HoleTwoBushDiameter";//编辑框1的关键字  HoleOneBushDiameter一开钻头直径
						value = "J55φ244.48×8.94mm";//默认A1
					}//编辑框1引用结束

					:edit_box{ //引用编辑框4
						label = "二开套管下深(m)："; //编辑框2的标签
						edit_width = 8; //编辑框2的宽度
						key = "HoleTwoBushDeep";//编辑框2的关键字 
						value = 0.0;
					}//编辑框2引用结束
					
					:edit_box{ //引用编辑框8
						label = "二开水泥上返深度(m)："; //编辑框8的标签
						edit_width = 8; //编辑框8的宽度
						key = "HoleTwoCement";//编辑框8的关键字  水泥上反
						value = 0.0;
					}//编辑框8引用结束
					
			}//加框列引用结束
			
			:boxed_column{//引用加框列3
				

					
					:edit_box{ //引用编辑框5
						label = "三开钻头直径(mm)："; //编辑框1的标签
						edit_width = 8; //编辑框1的宽度
						key = "HoleThreeDrillDiameter";//编辑框1的关键字  HoleOneDrillDiameter一开钻头直径
						value = "φ215.9mm";//默认A1
						is_enabled = False;
					}//编辑框1引用结束

					:edit_box{ //引用编辑框6
						label = "三开井深(m)："; //编辑框2的标签&之后的字符是热键Maximum Consumption
						edit_width = 8; //编辑框2的宽度
						key = "HoleThreeDeep";//编辑框2的关键字 
						value = 0.0;
						is_enabled = False;
					}//编辑框2引用结束
					
					:edit_box{ //引用编辑框5
						label = "三开套管外径(mm)："; //编辑框1的标签
						edit_width = 8; //编辑框1的宽度
						key = "HoleThreeBushDiameter";//编辑框1的关键字  HoleOneBushDiameter一开钻头直径
						value = "φ139.70×7.72";//默认A1
						is_enabled = False;
					}//编辑框1引用结束

					:edit_box{ //引用编辑框6
						label = "三开套管下深(m)："; //编辑框2的标签
						edit_width = 8; //编辑框2的宽度
						key = "HoleThreeBushDeep";//编辑框2的关键字 
						value = 0.0;
						is_enabled = False;
					}//编辑框6引用结束
					
					:edit_box{ //引用编辑框9
						label = "三开水泥上返深度(m)："; //编辑框9的标签
						edit_width = 8; //编辑框9的宽度
						key = "HoleThreeCement";//编辑框9的关键字  水泥上反
						value = 0.0;
						is_enabled = False;
					}//编辑框9引用结束
					
					:edit_box{ //引用编辑框10
						label = "人工井底深度(m)："; //编辑框10的标签
						edit_width = 8; //编辑框10的宽度
						key = "HoleArtificialDeep";//编辑框10的关键字  人工井底深度
						value = 0.0;
					}//编辑框10引用结束
					
					:edit_box{ //引用编辑框11
						label = "阻流环位置(m)："; //编辑框11的标签
						edit_width = 8; //编辑框11的宽度
						key = "HoleDamperLocation";//编辑框11的关键字  阻流环位置
						value = 0.0;
					}//编辑框11引用结束
					
					:edit_box{ //引用编辑框12
						label = "套管鞋(m)："; //编辑框12的标签
						edit_width = 8; //编辑框12的宽度
						key = "BushShoes";//编辑框12的关键字  套管鞋
						value = 0.0;
					}//编辑框11引用结束
					
					:button{
						key = "DDBtn";  //DrawDrillButton
						label = "绘制";
						fixed_width = true;
						alignment=right;             
					}
					
			}//加框列引用结束
			
			
			:boxed_column{
						label="根据EXCEL绘制井斜方位角";//加框列的标签    
						:edit_box{ //引用编辑框1
							label = "深度列(eg A1:A10)："; //编辑框1的标签
							edit_width = 8; //编辑框1的宽度
							key = "columnDrillDeviationOne";//编辑框1的关键字 
							value = "A1:A10";//默认A1
						}//编辑框1引用结束
						:edit_box{ //引用编辑框2
							label = "井斜列(eg A1:A10)：";
							edit_width = 8; //编辑框2的宽度
							key = "columnDrillDeviationTwo";//编辑框5的关键字 
							value = "B1：B10";
						}//编辑框2引用结束
						:edit_box{ //引用编辑框3
							label = "方位角列(eg A1:A10)："; 
							edit_width = 8; //编辑框2的宽度
							key = "columnDrillDeviationThree";//编辑框3的关键字 
							value = "C1：C10";
						}//编辑框3引用结束
						
						:button{
						  key = "DDDBtn";  //DrawDrillDeviationButton
						  label = "绘制";
						  fixed_width = true;
						  alignment=right;             
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
