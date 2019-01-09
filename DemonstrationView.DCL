demonstrationView:dialog{ 
	label = "demonstrationController";
	:row{
		:button{
		  key = "reduceSpeedBtn"; 
		  label = "-";
		  edit_width = 3; 
		  alignment=centered;             
		}
		:edit_box{ 
			label = "delayTimeSeconds"; 
			edit_width = 5; 
			key = "delayTime";
			alignment=left; 
			value = "1";
		}
		:button{
		  key = "increaseBtn";  
		  label = "+";
		  edit_width = 3; 
		  alignment=centered;             
		}
	}
	:row{
		:radio_button{
			key = "unit_productionMode";
			label = "productionMode";
			value = 0;
		}
		:radio_button{
			key = "unit_demonstrationMode";
			label = "demostrationMode";
			value = 1;
		}
	}
	:row{
		:radio_button{
			key = "unit_singleColor";
			label = "singleColor         ";
			value = 0;
		}
		:radio_button{
			key = "unit_mutipleColor";
			label = "mutipleColor          ";
			value = 1;
		}
	}	
	ok_cancel;
}