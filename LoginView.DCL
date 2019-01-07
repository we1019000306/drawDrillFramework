login:dialog{ 
	label = "login";
		:edit_box{ 
			label = "username"; 
			edit_width = 20; 
			key = "username";
			value = "Monster";
		}
		:edit_box{ 
			label = "password"; 
			edit_width = 20; 
			key = "password";
			value = "123456";
			password_char = "*";
		}
	:row{
		:button{
				  key = "loginBtn"; 
				  label = "login";
				  is_default = true;
				  fixed_width = true;
				  alignment=centered;             
				}
		:button{
				  key = "closeBtn";  
				  label = "quit";
				  fixed_width = true;
				  alignment=centered;             
				}
		}
}