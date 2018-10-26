login:dialog{ 
	label = "用户登录界面";
		:edit_box{ 
			label = "用户名"; 
			edit_width = 20; 
			key = "username";
			value = "Monster";
		}
		:edit_box{ 
			label = "密码"; 
			edit_width = 20; 
			key = "password";
			value = "123456";
			password_char = "*";
		}
	:row{
		:button{
				  key = "loginBtn"; 
				  label = "登录";
				  is_default = true;
				  fixed_width = true;
				  alignment=centered;             
				}
		:button{
				  key = "closeBtn";  
				  label = "退出";
				  fixed_width = true;
				  alignment=centered;             
				}
		}
}