framework:dialog{
  label= "1";
  :row{
		:boxed_column{
        label="2";
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
		   value="3.....";
       }

        :row{
			 :button{
			  key = "EFSBtn";  //EraseFrameStructureButton 
			  label = "4";
			  fixed_width = true;
			  alignment=left;             
			}		
			:button{
			  key = "DFSBtn"; //DrawFrameStructureButton
			  label = "5";
			  fixed_width = true;
			  alignment=right;             
			}
		}
        	
			
		}
  			
			
		:boxed_column{
			label="6";//加框列的标签
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
				   value="7";
			   }

				:row{
					 :button{
					  key = "ELBtn";  //EraseLineButton 
					  label = "8";
					  fixed_width = true;
					  alignment=left;             
					}		
					:button{
					  key = "DLBtn";  //DrawLineButton
					  label = "9";
					  fixed_width = true;
					  alignment=right;             
					}
				}
			}//加框列引用结�?
			
			
			:boxed_column{
				label="10";
				
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
				   value="11";
			   }
				
				:row{
					 :button{
					  key = "ERSBtn";  //EraseRockStructureButton
					  label = "12";
					  fixed_width = true;
					  alignment=left;             
					}		
					:button{
					  key = "DRSBtn";  //DrawRockStructureButton
					  label = "13";
					  fixed_width = true;
					  alignment=right;             
					}
				}
	
			}
	}
		
		:row{
				:boxed_column{
					label="17";
				
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
					   value="18";
				   }
					
					:row{
						 :button{
						  key = "ECDBtn";  //EraseColumnarDiagramButton
						  label = "19";
						  fixed_width = true;
						  alignment=left;             
						}
						:button{
						  key = "DCDBtn";  //DrawColumnarDiagramButton
						  label = "20";
						  fixed_width = true;
						  alignment=right;             
						}
					}
				}
		

				:boxed_column{
						label="21";    
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
						   value="22";
					   }
						:row{
							 :button{
							  key = "ERSignBtn";  //EraseRockSignButton
							  label = "23";
							  fixed_width = true;
							  alignment=left;             
							}
							:button{
							  key = "DRSignBtn";  //DrawRockSignButton
							  label = "24";
							  fixed_width = true;
							  alignment=right;             
							}
						}
		
				}
				
		
			:boxed_column{
					label="25";
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
						   value="26";
					   }
					:row{
						 :button{
						  key = "EDCSBtn";  //EraseDrillCompletionStructureButton
						  label = "27";
						  fixed_width = true;
						  alignment=left;             
						}
						:button{
							key = "DDCSBtn";  //DrawDrillCompletionStructureButton 
							label = "28";
							fixed_width = true;
							alignment=right;             
						}
					}
					
			}//加框列引用结�?
			
			
			:boxed_column{
					label="29";    
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
						   value="30";
					   }	
					:row{
						 :button{
						  key = "EDDBtn";  //EraseDrillDeviationButton
						  label = "31";
						  fixed_width = true;
						  alignment=left;             
						}
						:button{
						  key = "DDDBtn";  //DrawDrillDeviationButton
						  label = "32";
						  fixed_width = true;
						  alignment=right;             
						}
					}
			
					}
					
		}
	
	:row{
			:button{
			  key = "startBtn"; 
			  label = "33";
			  is_default = true;
			  fixed_width = true;
			  alignment=centered;             
			}
			:button{
			  key = "baseDataBtn"; 
			  label = "34";
			  fixed_width = true;
			  alignment=centered;             
			}
			:button{
			  key = "automationDrawBtn"; 
			  label = "35";
			  fixed_width = true;
			  alignment=centered;             
			}
			:button{
				  key = "cancelBtn";  
				  label = "36";
				  fixed_width = true;
				  alignment=centered;             
			}
	}
}
